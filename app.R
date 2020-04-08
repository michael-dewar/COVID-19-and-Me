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
  fluidPage(HTML("<a href=\"https://github.com/michael-dewar/COVID-19-and-Me\" class=\"github-corner\" aria-label=\"View source on GitHub\"><svg width=\"80\" height=\"80\" viewBox=\"0 0 250 250\" style=\"fill:#151513; color:#fff; position: absolute; top: 0; border: 0; right: 0;\" aria-hidden=\"true\"><path d=\"M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z\"></path><path d=\"M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2\" fill=\"currentColor\" style=\"transform-origin: 130px 106px;\" class=\"octo-arm\"></path><path d=\"M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z\" fill=\"currentColor\" class=\"octo-body\"></path></svg></a><style>.github-corner:hover .octo-arm{animation:octocat-wave 560ms ease-in-out}@keyframes octocat-wave{0%,100%{transform:rotate(0)}20%,60%{transform:rotate(-25deg)}40%,80%{transform:rotate(10deg)}}@media (max-width:500px){.github-corner:hover .octo-arm{animation:none}.github-corner .octo-arm{animation:octocat-wave 560ms ease-in-out}}</style>"),
  titlePanel("COVID-19 and Me"),
  fluidRow(column(12, "Where do you live?")),
  fluidRow(
    column(4, selectInput("user_country", "Country:", choices = countries, selected = "Canada")),
    column(4, uiOutput("user_province_ui")),
    column(4, uiOutput("user_county_ui"))),
  conditionalPanel("input.unlock_advanced == true",
  fluidRow(
    column(4, numericInput("days", "Number of Days:", value = 7, min = 2, max = 14, step = 1), bookmarkButton()),
    column(4, selectInput("as_of", "Doubling As Of:", choices = as_of_dates)),
    column(4, selectInput("metric", "Metric:", choices = c("Confirmed", "Deaths", "Recovered", "Active")))
  )),
  # sidebarLayout(
  # sidebarPanel(#style = "position:fixed;width:23%",
  #              "Where do you live?",
  #              selectInput("user_country", "Country:", choices = countries, selected = "Canada"),
  #              uiOutput("user_province_ui"),
  #              uiOutput("user_county_ui"),
  #              conditionalPanel("input.unlock_advanced == true",
  #              numericInput("days", "Number of Days:", value = 7, min = 2, max = 14, step = 1),
  #              selectInput("as_of", "Doubling As Of:", choices = as_of_dates),
  #              selectInput("metric", "Metric:", choices = c("Confirmed", "Deaths", "Recovered", "Active"))),
  #              bookmarkButton()),
  # mainPanel(
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
             )}


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