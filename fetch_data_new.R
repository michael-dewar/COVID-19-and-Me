library(tidyverse)
library(readr)
library(lubridate)
library(glue)
library(feather)
library(curl)

# tmp <- tempfile()
# curl_download("https://codeload.github.com/CSSEGISandData/COVID-19/tar.gz/master", tmp)
# untar(tmp, files = "COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports")

daily_dir <- "COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports"

daily <- dir(daily_dir, "csv") %>% 
  paste0(daily_dir, "/", .) %>% 
  map(read_csv) %>% 
  map2(dir(daily_dir, "csv") %>% str_sub(1,-5) %>% mdy(), ~mutate(.x, Date = .y)) %>% 
  map(~select(.x, Date, starts_with("Country"), starts_with("Province"), starts_with("Admin2"),
              Confirmed, Deaths, Recovered, starts_with("Active"),
              starts_with("Lat"), starts_with("Long"))) %>% 
  bind_rows %>% 
  transmute(Date,
            Country = coalesce(`Country/Region`, Country_Region),
            Province = coalesce(`Province/State`, Province_State),
            County = Admin2,
            Lat = coalesce(Latitude, Lat),
            Long = coalesce(Longitude, Long_),
            Confirmed, Deaths, Recovered, Active) %>% 
  replace_na(list(Confirmed = 0, Deaths = 0, Recovered =0)) %>% 
  pivot_longer(cols = Confirmed:Active, names_to = "Metric", values_to = "Value") %>% 
  filter(Metric %in% c("Confirmed", "Deaths")) %>% 
  # On 2020-03-22 District of Columbia had two entries.  Fix this:
  group_by(Country, Province, County, Lat, Long, Metric, Date) %>% 
  summarize(Value = sum(Value, na.rm = TRUE)) %>% 
  ungroup %>% 
  filter(Date >= ymd("2020-03-22"))

# There are many inconsistencies in the data before March 22.  Naming changed frequently.

daily <- daily %>% 
  mutate(County = if_else(Province == "Northern Mariana Islands", NA_character_, County))


# No Territories ----------------------------------------------------------

no_territories <- daily %>% filter(is.na(Province)) %>% 
  mutate(Place = Country,
         key = Country,
         type = "Country")
  
no_territories_lat_long <- no_territories %>% 
  filter(!is.na(Lat), !is.na(Long), Lat != 0, Long != 0) %>% 
  group_by(key) %>%
  arrange(Date) %>% 
  filter(row_number() == n()) %>% 
  select(-Metric, -Date, - Value)
no_territories_lat_long

no_territories_final <- no_territories %>% 
  select(-Lat, -Long)


# Territories -------------------------------------------------------------

territories <- daily %>% 
  filter(is.na(County) == TRUE, is.na(Province) == FALSE) %>% 
  filter(!Country %in% c("Australia", "Canada", "China")) %>% 
  filter(!Province %in% c("Diamond Princess", "Grand Princess", "Recovered")) %>% 
  mutate(Province = str_replace_all(Province, "\\(Islas Malvinas\\)", "\\(Malvinas\\)")) %>% 
  mutate(Country = glue("{Province} ({Country})"),
         Province = NA) %>% 
  group_by(Country) %>% 
  mutate(Lat = median(Lat, na.rm = TRUE),
         Long = median(Long, na.rm = TRUE),
         Place = Country,
         key = glue("{Country}"),
         type = "Country",
         Province = NA_character_,
         County = NA_character_) %>% 
  ungroup

territories

# US ----------------------------------------------------------------------

counties <- daily %>% 
  filter(!is.na(County), Country == "US" ) %>% 
  mutate(Place = County,
         key = glue("{Country}, {Province}, {County}"),
         type = "County") %>% 
  group_by(key) %>% 
  mutate(Lat = median(Lat, na.rm = TRUE),
         Long = median(Long, na.rm = TRUE)) %>% 
  ungroup

states <- counties %>% 
  group_by(Country, Province, Date, Metric) %>% 
  summarize(Value = sum(Value, na.rm = TRUE),
            Lat = median(Lat, na.rm = TRUE),
            Long = median(Long, na.rm = TRUE)) %>% 
  group_by(Country, Province) %>% 
  mutate(Lat = median(Lat, na.rm = TRUE),
         Long = median(Long, na.rm = TRUE),
         Place = glue("{Province} State"),
         key = glue("{Country}, {Province}"),
         type = "State",
         County = "All")

us <- states %>% 
  group_by(Country, Date, Metric) %>% 
  summarize(Value = sum(Value, na.rm = TRUE),
            Lat = median(Lat, na.rm = TRUE),
            Long = median(Long, na.rm = TRUE)) %>% 
  group_by(Country) %>% 
  mutate(Lat = median(Lat, na.rm = TRUE),
         Long = median(Long, na.rm = TRUE),
         Place = Country,
         key = glue("{Country}"),
         type = "Country",
         Province = "All",
         County = NA_character_)
us
states
counties

cross_keys <- function(df, type){
  new <- expand_grid(key = df$key, B_key = df$key) %>% 
    left_join(df, by = "key") %>% 
    left_join(df %>% rename_with(~paste0("B_", .)), by = "B_key") %>% 
    mutate(distance = sqrt((Lat - B_Lat)^2 + (Long - B_Long)^2)) %>% 
    select(key, neighbour_key = B_key, distance)
    
    if(type == "County"){
      aggregate_keys <- df %>% 
        transmute(key, neighbour_key = glue("{Country}, {Province}"), distance = 0)
    } else if(type %in% c("State", "Province")) {
      aggregate_keys <- df %>% 
        transmute(key, neighbour_key = glue("{Country}"), distance = 0)
    } else {
      aggregate_keys <- new %>% filter(FALSE)
    }
  bind_rows(new, aggregate_keys) %>% 
    filter(!is.na(distance)) %>% 
    arrange(key, distance)
}


geo_counties <- counties %>% 
  ungroup %>% 
  filter(!Province %in% c("Wuhan Evacuee"), str_to_lower(County) != "unassigned") %>% 
  distinct(Country, Province, County, Place, key, type, Lat, Long) %>% 
  group_by(Province) %>% 
  group_split %>% 
  map(cross_keys, "County") %>% 
  bind_rows

geo_us_states <- states %>% 
  ungroup %>% 
  filter(!Province %in% c("Wuhan Evacuee"), str_to_lower(County) != "unassigned") %>% 
  distinct(Country, Province, County, Place, key, type, Lat, Long) %>% 
  group_by(Country) %>% 
  group_split %>% 
  map(cross_keys, "State") %>% 
  bind_rows
#geo_us_states %>% filter(str_starts(key, "US, Ohio"))


# Australia, Canada, China ------------------------------------------------

provinces <- daily %>% filter(Country %in% c("Australia", "Canada", "China")) %>% 
  group_by(Country, Province) %>% 
  mutate(Lat = median(Lat, na.rm = TRUE),
         Long = median(Long, na.rm = TRUE),
         Place = glue("{Province}"),
         key = glue("{Country}, {Province}"),
         type = if_else(Country == "Australia", "State", "Province")) %>% 
  ungroup
geo_provinces <- provinces %>% 
  distinct(Country, Province, County, Place, key, type, Lat, Long) %>% 
  filter(!is.na(Lat), !is.na(Long), Lat != 0, Long != 0) %>% 
  group_by(Country) %>% 
  group_split() %>% 
  map(cross_keys, "State") %>% 
  bind_rows


geo_provinces %>% print(n=300)

aus_can_chi <- provinces %>% 
  group_by(Country, Metric, Date) %>% 
  summarize(Value = sum(Value, na.rm = TRUE),
            Lat = median(Lat, na.rm = TRUE),
            Long = median(Long, na.rm = TRUE)) %>% 
  group_by(Country) %>% 
  mutate(Lat = mean(Lat, na.rm = TRUE),
         Long = mean(Long, na.rm = TRUE),
         Place = Country,
         key = glue("{Country}"),
         type = "Country",
         Province = "All",
         County = NA_character_)
aus_can_chi %>% print(n=300)


# Country Geo -------------------------------------------------------------

geo_countries <- bind_rows(territories, no_territories, aus_can_chi, us) %>% 
  group_by(Country, Province, County, Place, key, type) %>% 
  summarize(Lat = mean(Lat, na.rm = TRUE),
            Long = mean(Long, na.rm = TRUE)) %>% 
  ungroup %>% 
  cross_keys("Country")

geo_countries
# Combine -----------------------------------------------------------------

neighbours <- bind_rows(geo_countries, geo_counties, geo_us_states, geo_provinces) %>% 
  mutate(across(contains("key"), as.character))
data_temp <- bind_rows(territories, no_territories, aus_can_chi, provinces, us, states, counties) %>%
  select(-Lat, -Long) %>% 
  mutate(across(c("Country", "Place", "key"), as.character)) %>% 
  filter(!Province %in% c("Wuhan Evacuee", "Diamond Princess", "Grand Princess", "Recovered") | is.na(Province)) %>% 
  filter(tolower(County) != "unassigned" | is.na(County))
geo <- data_temp %>% distinct(Country, Province, County, key, Place, type)
data <- data_temp %>% select(key, Date, Metric, Value)

data %>% write_feather("data2.feather")
geo %>% write_feather("geo2.feather")
neighbours %>% write_feather("neighbours.feather")













