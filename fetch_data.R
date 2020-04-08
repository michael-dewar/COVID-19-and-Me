library(tidyverse)
library(readr)
library(lubridate)
library(glue)
library(feather)
library(curl)

daily_dir <- "csse_covid_19_daily_reports"

daily <- dir(daily_dir, "csv") %>% 
  paste0(daily_dir, "/", .) %>% 
  map(read_csv) %>% 
  map2(dir(daily_dir, "csv") %>% str_sub(1,-5) %>% mdy(), ~mutate(.x, Date = .y)) %>% 
  map(~select(.x, Date, starts_with("Country"), starts_with("Province"), starts_with("Admin2"),
              Confirmed, Deaths, Recovered, starts_with("Active"),
              starts_with("Lat"), starts_with("Long"))) %>% 
  bind_rows %>% 
  #filter(is.na(Admin2)==FALSE) %>% 
  filter(Date >= ymd("2020-03-22")) %>% 
  transmute(Date,
            Country = coalesce(`Country/Region`, Country_Region),
            Province = coalesce(`Province/State`, Province_State),
            County = Admin2,
            Lat = coalesce(Latitude, Lat),
            Long = coalesce(Longitude, Long_),
            Confirmed, Deaths, Recovered, Active) %>% 
  replace_na(list(Confirmed = 0, Deaths = 0, Recovered =0)) %>% 
  pivot_longer(cols = Confirmed:Active, names_to = "Metric", values_to = "Value") %>% 
  # On 2020-03-22 District of Columbia had two entries.  Fix this:
  group_by(Country, Province, County, Lat, Long, Metric, Date) %>% 
  summarize(Value = sum(Value, na.rm = TRUE)) %>% 
  ungroup

write_feather(daily, "daily.feather")

geo <- daily %>% 
  group_by(Country, Province, County) %>% 
  arrange(desc(Date)) %>% 
  filter(row_number()==1) %>% 
  ungroup %>% 
  select(Country, Province, County, Lat, Long) %>% 
  mutate(key = case_when(is.na(Province) ~ Country,
                         is.na(County) ~ as.character(glue("{Country}, {Province}")),
                         TRUE ~ as.character(glue("{Country}, {Province}, {County}")))) %>% 
  filter(!is.na(Lat), !is.na(Long), Lat != 0, Long != 0)

cross_keys <- function(df){
  expand_grid(key = df$key, B_key = df$key) %>% 
    left_join(df, by = "key") %>% 
    left_join(df %>% rename_with(~paste0("B_", .)), by = "B_key") %>% 
    mutate(distance = sqrt((Lat - B_Lat)^2 + (Long - B_Long)^2)) %>% 
    group_by(key) %>% 
    arrange(distance, .by_group = TRUE) %>% 
    filter(row_number()<=20) %>% 
    ungroup
}

by_county <- geo %>% 
  filter(!is.na(County)) %>% 
  group_by(Province) %>% 
  group_split %>% 
  map(cross_keys) %>% 
  bind_rows %>% 
  mutate(type = "County")

by_province <- geo %>% 
  filter(Country %in% c("Canada", "China")) %>% 
  group_by(Country) %>% 
  group_split %>% 
  map(cross_keys) %>% 
  bind_rows %>% 
  mutate(type = "Province")

by_state <- geo %>% 
  filter(Country == "Australia") %>% 
  cross_keys %>% 
  mutate(type = "State")

by_country <- geo %>% 
  filter(is.na(County), !Country %in% c("Canada", "China", "Australia")) %>% 
  cross_keys %>% 
  mutate(type = "Country")

neighbours <- bind_rows(by_country, by_state, by_province, by_county)

neighbours %>% write_feather("geo.feather")
daily %>% 
  inner_join(geo %>% select(key, Country, Province, County), by = c("Country", "Province", "County")) %>% 
  select(-Lat, -Long, -Country, -Province, -County) %>% 
  write_feather("data.feather")
