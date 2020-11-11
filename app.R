# Preliminaries -----------------------------------------------------------

library(tidyverse)
library(shiny)
library(feather)
library(glue)
library(lubridate)
library(modelr)
library(gitlink)
library(writexl)

# Functions ---------------------------------------------------------------

add_doubling_time_to_filtered_df <- function(filtered_df, days){
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
    filter(`Doubling Time` < 50, `Doubling Time`>-20) %>% 
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
      scale_y_log10() +
      #ylim(0,20) +
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
    column(4, uiOutput("user_country_ui")),
    column(4, uiOutput("user_province_ui")),
    column(4, uiOutput("user_county_ui"))),
  fluidRow(column(12, checkboxInput("unlock_advanced", "Unlock Advanced Settings"))),
  conditionalPanel("input.unlock_advanced == true",
  fluidRow(
    column(4, numericInput("num_neighbours", "Number of Neighbours to Plot:", value = 20, min = 1, max = 30, step = 1)),
    column(4, radioButtons("metric", "Type of Case:", choices = c("Confirmed", "Deaths"), selected = "Confirmed", inline = TRUE)),
    column(4, numericInput("days", "Compute Doubling Time over X Days:", value = 5, min = 2, max = 14, step = 1),
              uiOutput("as_of_ui"), uiOutput("update_ui"))
  )),
  tabsetPanel(
    tabPanel("Near Me",
             h2("Current Doubling Period"),
             p("The ",em("doubling Period"), "is the time until the number of cases doubles. It is better if it takes a long time until we have ",
               "more cases, and so longer lines are better in the graph below."),
             plotOutput("plot_doubling_time_by_region"),
             p("Raw data is from the",
             a("Johns Hopkins University Center for Systems Science and Engineering", href="https://github.com/CSSEGISandData/COVID-19")),
             h2("Doubling Time Trend"),
             p("As social distancing measures take effect, we should see a change in the doubling period over time. The chart below has the same ",
               "regions as the chart above."),
             plotOutput("plot_doubling2"),
             p("You can get the table of values for this chart:", 
               downloadButton("download_xlsx", "Download XLSX"), 
               downloadButton("download_csv", "Download CSV"),
               actionButton("view_table", "View Here")),
             conditionalPanel("input.view_table % 2 == 1", tableOutput("doubling_by_region_over_time"))
             ),
    tabPanel("Why/How",
             h2("The Problem With The Exponential Graph"),
             p("Looking at a graph of the number of COVID-19 cases is usually not very illuminating because:"), 
             HTML("<ul><li> exponential graphs can look pretty much the same.</li>"),
             HTML("<li>a small number of cases can quickly become a large number of cases.</li></ul>"),
                      plotOutput("plot_raw"),
             p("We need to pay attention to the rate of growth."),
             h2("Log Scales Are Better"),
             p("If we plot the number of cases using a log scale, then we can begin to see the difference between growth rates. ",
             "The slope of the curve represents the rate of growth. You can see which places are growing fastest by looking at which ",
             "places have the steepest curve."),
                      plotOutput("plot_log"),
             p("But it can be tricky to visually compare different slopes. Moreover, the impact of the different slopes can be hard to ",
             "appreciate."),
             h2("Doubling Time"),
             p("Since we want to look at the slope, we should actually just graph that directly."),
                      plotOutput("plot_doubling"),
             h2("How Do We Make This Graph?"),
             p("There can be a lot of day-to-day variation in the growth rate. We smooth this out by taking ",
             "the most recent five days, plotting the points on a log_2 scale, and then computing the line of best fit. The slope of this ",
             "line is the reciprocal of the doubling time. We clean up the doubling times by ignoring doubling times longer than 1000 days."),
                      plotOutput("plot_line_of_best_fit"),
             p("Why five days? If we take too few days, our computed doubling time will jump around a lot because of random variation. ",
             "This noise can distract us from the underlying trend. If you take too many days, then we will be slow to detect changes to the ",
             "trend. People change their behaviour quite quickly and we want to be able to see this. You can try a different number of days ",
             "by unlocking advanced settings above."),
                      ),
    tabPanel("Mix'n'Match",
             fluidRow(column(4,uiOutput("select_many_ui")),
                      column(4, style = "margin-top: 25px;", bookmarkButton())),
             plotOutput("select_many_doubling"),
             plotOutput("select_many_log_cases")
             )))}


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  data <- reactiveFileReader(1000*60*3, session, "data.feather", read_feather)
  geo <- reactiveFileReader(1000*60*3, session, "geo.feather", read_feather) 
  neighbours <- reactiveFileReader(1000*60*3, session, "neighbours.feather", read_feather) 
  
  output$select_many_ui <- renderUI({
    selectInput("select_many", "Select as many regions as you like:", choices = geo() %>% pull(key) %>% sort, multiple = TRUE)
  })
  
  select_many_data <- reactive({
    data() %>% 
      filter(key %in% input$select_many,
             Metric == input$metric) %>% 
      add_doubling_time_to_filtered_df(days = input$days) %>% 
      mutate(`Doubling Time` = case_when(`Doubling Time` >  1000 ~ Inf,
                                         `Doubling Time` < -1000 ~ -Inf,
                                         TRUE ~ `Doubling Time`)) %>% 
      filter(is.na(`Doubling Time`) == FALSE)
  })
  output$select_many_doubling <- renderPlot({
    select_many_data() %>% 
      ggplot(aes(x = Date, color = key, y = `Doubling Time`)) + 
      geom_line() +
      scale_y_log10() +
      #coord_flip() +
      labs(title = "Doubling Time",
           y = "Doubling Time, (Days)",
           x = NULL,
           color = NULL)
  })
  
  output$select_many_log_cases <- renderPlot({
    select_many_data() %>% 
      ggplot(aes(x = Date, color = key, y = Value)) + 
      geom_line() +
      scale_y_log10() +
      #coord_flip() +
      labs(title = glue("{input$metric}"),
           y = "Cases",
           x = NULL,
           color = "")
  })
  
  countries <- reactive({
    geo() %>% distinct(Country)  %>% pull %>% sort
  })
    
  as_of_dates <- reactive({
    data() %>% distinct(Date) %>% arrange(desc(Date)) %>% pull
  })
  
  update_in_process <- reactiveVal(FALSE)
  observeEvent(input$update_data_button,{
    updateActionButton(session,  "update_data_button", label = "Update in Process.  Please Do Not Close Browser.")
    source("fetch_data.R")
    update_in_process(TRUE)
    updateActionButton(session, "update_data_button", label = "Update Complete") #Probably won't show
  })
  
  provinces <- reactive({
    req(countries(), input$user_country)
    geo() %>% 
      filter(Country == input$user_country) %>% 
      distinct(Province) %>% 
      pull
    })
  counties  <- reactive({
    req(provinces(), input$user_province, input$user_country == "US", input$user_province != "All")
    geo() %>% 
      filter(Country == input$user_country, Province == input$user_province) %>% 
      distinct(County) %>% 
      pull
    })
  output$user_country_ui <- renderUI({
    req(countries())
    selectInput("user_country", "Country:", choices = countries(), selected = "Canada")
  })
  
  output$user_province_ui <- renderUI({
    req(provinces())
    if(input$user_country %in% c("US", "Australia"))
      return(selectInput("user_province", "State:", choices = provinces(), selected = "All"))
    if(input$user_country %in% c("Canada", "China"))
      return(selectInput("user_province", "Province:", choices = provinces(), selected = "All"))
    return(NULL)
    })
  
  output$user_county_ui <- renderUI({
    req(counties())
    if(input$user_country == "US" & input$user_province != "All")
      return(selectInput("user_county", "County:", choices = counties(), selected = "All"))
    else
      return(NULL)
  })
  
  output$as_of_ui <- renderUI({
    selectInput("as_of", "Doubling As Of:", choices = as_of_dates())
  })
  
  output$update_ui <- renderUI({
    most_recent_in_data <- data() %>% 
      ungroup %>% 
      summarize(Date = max(Date)) %>% 
      pull
    
    if(most_recent_in_data < today() -1 & update_in_process() == FALSE){
      return(actionButton("update_data_button", "Update Data"))
    } else{
      return(NULL)
    }
    
  })
  # output$US_instructions_ui <- renderUI({
  #   req(input$user_country)
  #   if(input$user_country == "US"){
  #     p("To compare your state with nearby states, choose ", tags$b("Overall"), " as your state, and then choose your state in the third box.")
  #   } else(NULL)
  # })
  ready_to_plot <- reactive({
    req(input$metric, input$user_country, geo(), neighbours(), data())
    if(!input$user_country %in% c("Australia", "Canada", "China", "US"))
      return(TRUE)
    req(input$user_province)
    if(input$user_country %in% c("Australia", "Canada", "China"))
      return(TRUE)
    if(input$user_province == "All")
      return(TRUE)
    req(input$user_county)
    return(TRUE)
  })
  
  data_to_plot <- reactive({
    req(ready_to_plot())

    data <- data() %>% filter(Metric == input$metric)
 
    if(!input$user_country %in% c("Australia", "Canada", "China", "US"))
      highlight_key <- glue("{input$user_country}")
    else if(input$user_province == "All")
      highlight_key <- glue("{input$user_country}")
    else if(input$user_country %in% c("Australia", "Canada", "China"))
      highlight_key <- glue("{input$user_country}, {input$user_province}")
    else if(input$user_county == "All")
      highlight_key <- glue("{input$user_country}, {input$user_province}")
    else
      highlight_key <- glue("{input$user_country}, {input$user_province}, {input$user_county}")
    
    highlight_key <- highlight_key %>% as.character
    
    if(!input$user_country %in% c("Australia", "Canada", "China", "US"))
      this_type <- "Country"
    else if(input$user_province == "All")
      this_type <- "Country"
    else if(input$user_country %in% c("Canada", "China"))
      this_type <- "Province"
    else if(input$user_country == "Australia")
      this_type <- "State"
    else if(input$user_county == "All")
      this_type <- "State"
    else
      this_type <- "County"
    
    data_key <- neighbours() %>% 
      filter(key == highlight_key) %>% 
      arrange(distance) %>% 
      filter(row_number()<= input$num_neighbours + 1) %>% 
      pull(neighbour_key)
    
    rows <- data() %>% 
      filter(key %in% data_key) %>% 
      mutate(highlight = if_else(key == highlight_key, TRUE, FALSE)) %>% 
      left_join(geo() %>% select(key, Place), by = "key") %>% 
      mutate(type = this_type)
    
    if(this_type == "State" & input$user_country == "US")
      rows <- rows %>% mutate(Place = str_remove(Place, " State"))
    
    return(rows)
    
  })
  
  doubled <- reactive({
    req(data_to_plot())
    data_to_plot() %>% 
      add_doubling_time_to_filtered_df(days = input$days) %>% 
      mutate(`Doubling Time` = case_when(`Doubling Time` >  1000 ~ Inf,
                                         `Doubling Time` < -1000 ~ -Inf,
                                         TRUE ~ `Doubling Time`))
  })
  
  data_for_export <- reactive({
    req(doubled())

    table <- doubled() %>%
      ungroup %>% 
      filter(Metric == input$metric) %>% 
      select(-Data, -Model) %>% 
      select(Place, Date, Value, `Doubling Time`) %>% 
      pivot_longer(cols = c("Value", "Doubling Time"), names_to = "Metric") %>% 
      pivot_wider(names_from = "Date", values_from = "value")
    
    
    table
    
    value <- table %>% filter(Metric == "Value") %>% mutate(Metric = input$metric)
    double <- table %>% filter(Metric == "Doubling Time")
    inputs <- reactiveValuesToList(input) %>% 
      bind_cols %>% 
      as_tibble() %>% 
      mutate_all(as.character) %>% 
      pivot_longer(cols = everything(), names_to = "Input", values_to = "Choice")
    
    table <- list("Doubling Time" = double, "Cases" = value, "Inputs" = inputs)
    
    return(table)
  })
  
  output$doubling_by_region_over_time <- renderTable({
    data_for_export()$`Doubling Time`
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
    doubled() %>% 
      mutate(`Doubling Time` = case_when(`Doubling Time` >  1000 ~ Inf,
                                         `Doubling Time` < -1000 ~ -Inf,
                                         TRUE ~ `Doubling Time`)) %>% 
      plot_value_vs_date_by_region(plot_type = "doubling")
  })
  
  output$plot_doubling2 <- renderPlot({
    req(doubled())
    doubled() %>% 
      mutate(`Doubling Time` = case_when(`Doubling Time` >  1000 ~ Inf,
                                         `Doubling Time` < -1000 ~ -Inf,
                                         TRUE ~ `Doubling Time`)) %>% 
      plot_value_vs_date_by_region(plot_type = "doubling")
  })
  
  output$download_xlsx <- downloadHandler(
    filename = function(){glue("COVID-19andMe_{today()}.xlsx")},
    content = function(file){
      data_for_export() %>% write_xlsx(file)
    }
  )
  
  output$download_csv <- downloadHandler(
    filename = function(){glue("COVID-19andMe_{today()}.csv")},
    content = function(file){
      data_for_export()$`Doubling Time` %>% write_csv(file)
    }
  )
  
  
  
  output$plot_doubling_time_by_region <- renderPlot({
    req(doubled())
    #input$user_province
    doubled() %>% 
      plot_doubling_time_by_region(days = input$days, as_of = input$as_of)
  })
  
  output$plot_line_of_best_fit <- renderPlot({
    req(doubled())
    doubled() %>% 
      filter(highlight == TRUE) %>% 
      plot_line_of_best_fit(as_of = input$as_of, days = input$days)
  })
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")