# Preliminaries -----------------------------------------------------------

library(tidyverse)
library(shiny)
library(feather)
library(glue)
library(lubridate)
library(modelr)
library(gitlink)

data <- read_feather("data.feather")
geo <- read_feather("geo.feather")
countries <- geo %>% distinct(Country)  %>% pull
as_of_dates <- data %>% distinct(Date) %>% arrange(desc(Date)) %>% pull

# Functions ---------------------------------------------------------------

add_doubling_time_to_filtered_df <- function(filtered_df, days = 7){
  filtered_df %>% 
    group_by(key, Metric) %>% 
    filter(Value >1 ) %>% 
    mutate(Log2_Value = log2(Value)) %>% 
    mutate(Data = list(cur_data())) %>% 
    mutate(Data = map2(Data, Date, ~filter(.x, Date <= .y, Date > .y - days))) %>% 
    mutate(Model = map(Data, ~lm(Log2_Value ~ Date, data = .x))) %>% 
    mutate(Slope = map_dbl(Model, ~.x$coefficients[names(.x$coefficients)=="Date"])) %>% 
    mutate("Doubling Time" = 1/Slope)
}

plot_doubling_time_by_region <- function(df, metric = NULL, days = 7, as_of = NULL){
  req(nrow(df)>0)
  if(is.null(as_of)){
    as_of <- df %>% 
      ungroup %>% 
      summarize(max(Date)) %>% 
      pull
  }
  
  if(is.null(metric)){
    if("Confirmed" %in% distinct(df, Metric))
      metric <- "Confirmed"
    else
      metric <- df$Metric[[1]]
  }
  
  highlight_string <- df$Place[df$highlight] %>% unique %>% paste0(., collapse = ", ") %>% paste0(., " highlighted")
  type_string <- df$type[df$highlight] %>% unique %>% paste0(., collapse = ", ")
  
  df %>% 
    filter(Metric == metric, Date == as_of) %>% 
    ggplot(aes(x=fct_reorder(Place, `Doubling Time`, min),
               y = `Doubling Time`,
               fill=highlight)) +
    geom_col() +
    #coord_flip(ylim = c(0,10)) +
    coord_flip() +
    labs(title = glue("Doubling Time by {type_string}.  Higher is better."),
         subtitle = highlight_string,
         caption = glue("Doubling Time is computed over the last {days} days."),
         x = type_string,
         y = "Doubling Time (Days)")+
    theme(legend.position = "none")
}

plot_value_vs_date_by_region <- function(df, plot_type = c("log", "raw", "doubling"), metric = NULL){
  req(nrow(df)>0)
  
  if(is.null(metric)){
    if("Confirmed" %in% distinct(df, Metric))
      metric <- "Confirmed"
    else
      metric <- df$Metric[[1]]
  }
  
  highlight_string <- df$Place[df$highlight] %>% unique %>% paste0(., collapse = ", ") %>% paste0(., " highlighted")
  type_string <- df$type[df$highlight] %>% unique %>% paste0(., collapse = ", ")
  
  if(plot_type == "doubling"){
    df <- df %>% mutate(Value = `Doubling Time`)
  }
  
  plot <- df %>% 
    filter(Metric == metric) %>% 
    ggplot(aes(x = Date,
               y = Value,
               group = Place,
               color=highlight))+
    geom_line()
  if(plot_type == "log"){
    plot <- plot + 
      scale_y_log10() +
      labs(title = glue("Log Scale {metric} by {type_string}"),
           subtitle = highlight_string,
           x = "Date",
           y = glue("{metric}"))
  } else if(plot_type == "raw"){
    plot <- plot + 
      labs(title = glue("Raw {metric} by {type_string}"),
           subtitle = highlight_string,
           x = "Date",
           y = glue("Raw {metric}"))
  } else{
    plot <- plot + 
      ylim(0,10) +
      labs(title = glue("Doubling Time by {type_string}"),
           subtitle = highlight_string,
           x = "Date",
           y = glue("Doubling Time of {metric}, Days"))
  }
  plot +
    theme(legend.position = "none")
}

plot_line_of_best_fit <- function(df, days = 7, as_of = NULL, metric = NULL){
  req(nrow(df)>0)
  
  if(is.null(metric)){
    if("Confirmed" %in% distinct(df, Metric))
      metric <- "Confirmed"
    else
      metric <- df$Metric[[1]]
  }
  
  if(is.null(as_of))
    as_of <- df %>% 
      ungroup %>% 
      summarize(max(Date)) %>% 
      pull
  
  local <- df %>% 
    filter(Date==as_of, Metric == metric) %>% 
    pull(Data) %>% 
    head(1) %>% 
    bind_rows
  
  model <- lm(Log2_Value ~ Date, local)
  
  local %>% 
    add_predictions(model) %>% 
    ggplot(aes(x=Date, y = Log2_Value)) +
    geom_point() +
    geom_line(aes(y=pred), color = "red") +
    labs(y = glue("Log2 {metric}"),
         title = "Line of Best Fit",
         subtitle = glue("Most recent {days} days up to {as_of}"))
}

# UI ----------------------------------------------------------------------

ui <- function(request) {
  fluidPage(
  titlePanel("COVID-19 and Me"),
  sidebarLayout(
  sidebarPanel(style = "position:fixed;width:23%",
               "Where do you live?",
               selectInput("user_country", "Country:", choices = countries, selected = "Canada"),
               uiOutput("user_province_ui"),
               uiOutput("user_county_ui"),
               conditionalPanel("input.unlock_advanced == true",
               numericInput("days", "Number of Days:", value = 7, min = 2, max = 14, step = 1),
               selectInput("as_of", "Doubling As Of:", choices = as_of_dates),
               selectInput("metric", "Metric:", choices = c("Confirmed", "Deaths", "Recovered", "Active"))),
               bookmarkButton()),
  mainPanel(
  tabsetPanel(
    tabPanel("Near Me",
             p("The ",em("doubling time"), "is the time until the number of cases doubles.  It is better if it takes a long time until we have ",
               "more cases, and so longer lines are better in the graph below."),
             plotOutput("plot_doubling_time_by_region"),
             p("Raw data is from the",
             a("Johns Hopkins University Center for Systems Science and Engineering", href="https://github.com/CSSEGISandData/COVID-19"))),
    tabPanel("Why/How",
             h2("The Problem With The Exponential Graph"),
             p("Looking at a graph of the number of COVID-19 cases is usually not very illuminating because:"), 
             HTML("<ul><li> exponential graphs can look pretty much the same.</li>"),
             HTML("<li>a small number of cases can quickly become a large number of cases.</li></ul>"),
                      plotOutput("plot_raw"),
             p("We need to pay attention to the rate of growth."),
             h2("Log Scales Are Better"),
             p("If we plot the number of cases using a log scale, then we can begin to see the difference between growth rates.",
             "The slope of the curve represents the rate of growth.  You can see which places are growing fastest by looking at which ",
             "places have the steepest curve."),
                      plotOutput("plot_log"),
             p("But it can be tricky to visually compare different slopes.  Moreover, the impact of the different slopes can be hard to ",
             "appreciate."),
             h2("Doubling Time"),
             p("Since we want to look at the slope, we should actually just graph that directly."),
                      plotOutput("plot_doubling"),
             h2("How Do We Make This Graph?"),
             p("There can be a lot of day-to-day variation in the growth rate.  We smooth this out by taking ",
             "the most recent seven days, plotting the points on a log_2 scale, and then computing the line of best fit.  The slope of this ",
             "line is the reciprocal of the doubling time."),
                      plotOutput("plot_line_of_best_fit"),
             p("Why seven days? If we take too few days, our computed doubling time will jump around a lot because of random variation. ",
             "This noise can distract us from the underlying trend.  If you take too many days, then we will be slow to detect changes to the ",
             "trend.  People change their behaviour quite quickly and we want to be able to see this.  You can try a different number of days ",
             "by unlocking advanced settings:"),
                      checkboxInput("unlock_advanced", "Unlock Advanced Settings")))
             )))}


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  provinces <- reactive({
    geo %>% 
      filter(Country == input$user_country) %>% 
      distinct(Province) %>% 
      pull
    })
  counties  <- reactive({
    req(provinces(), input$user_province)
    geo %>% 
      filter(Country == input$user_country, Province == input$user_province) %>% 
      distinct(County) %>% 
      pull
    })
  
  output$user_province_ui <- renderUI({
    req(provinces())
    if(input$user_country %in% c("US", "Australia"))
      return(selectInput("user_province", "State:", choices = provinces(), selected = c("Florida", "New South Wales")))
    if(input$user_country %in% c("Canada", "China"))
      return(selectInput("user_province", "Province:", choices = provinces(), selected = c("Quebec", "Zhejiang")))
    return(NULL)
    })
  
  output$user_county_ui <- renderUI({
    req(counties())
    selectInput("user_county", "County:", choices = counties(), selected = c("Alachua"))
  })
  
  data_to_plot <- reactive({
    req(input$metric)
    data <- data %>% filter(Metric == input$metric)
    
    if(input$user_country == "US"){
      req(input$user_province, input$user_county)
      
      rows <- geo %>% 
        filter(Country == input$user_country, Province == input$user_province, County == input$user_county) %>% 
        mutate(highlight = if_else(distance == 0, TRUE, FALSE)) %>% 
        select(type, Country = B_Country, Province = B_Province, County = B_County, key = B_key, Place = B_County, highlight) 
        
      return(data %>% inner_join(rows, by = "key"))
    }
    if(input$user_country %in% c("Australia", "Canada", "China")){
      req(input$user_province)
      rows <- geo %>% 
        filter(Country == input$user_country, Province == input$user_province) %>% 
        mutate(highlight = if_else(distance == 0, TRUE, FALSE)) %>% 
        select(type, Country = B_Country, Province = B_Province, County = B_County, key = B_key, Place = B_Province, highlight)
      
      return(data %>% inner_join(rows, by = "key"))
    }
    rows <- geo %>% 
      filter(Country == input$user_country) %>% 
      mutate(highlight = if_else(distance == 0, TRUE, FALSE)) %>% 
      select(type, Country = B_Country, Province = B_Province, County = B_County, key = B_key, Place = B_Country, distance, highlight)
    
    return(data %>% inner_join(rows, by = "key"))
    
  })
  
  doubled <- reactive({
    req(data_to_plot())
    data_to_plot() %>% 
      add_doubling_time_to_filtered_df(days = input$days)
  })
  
  output$plot_raw <- renderPlot({
    req(doubled())
    doubled() %>% plot_value_vs_date_by_region(plot_type = "raw")
  })
  
  output$plot_log <- renderPlot({
    req(doubled())
    doubled() %>% plot_value_vs_date_by_region(plot_type = "log")
  })
  
  output$plot_doubling <- renderPlot({
    req(doubled())
    doubled() %>% plot_value_vs_date_by_region(plot_type = "doubling")
  })
  
  output$plot_doubling_time_by_region <- renderPlot({
    req(doubled())
    #input$user_province
    doubled() %>% plot_doubling_time_by_region(days = input$days, as_of = input$as_of)
  })
  
  output$plot_line_of_best_fit <- renderPlot({
    req(doubled())
    doubled() %>% 
      filter(highlight == TRUE) %>% 
      plot_line_of_best_fit(as_of = input$as_of, days = input$days)
  })
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")