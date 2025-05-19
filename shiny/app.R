
library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(shinyjs)

billion_converter <- 1000000000.0
million_converter <- 1000000.0

data_path <- "../etc/data_to_check - without_raw_data - v3.0.xlsx"

df_all <- read_excel(data_path, sheet = "data", skip = 1)



df_all <- df_all %>%
  rename(
    "year" = "...1",
    "name" = "...2",
    "age" = "...3",
    "value1" = "To release...4",
    "value2" = "To release...5"
  )

df_all <- df_all[, c("year", "name", "age", "value1", "value2")]

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Taxable income time series plot"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("plot_type", "Select plot type", 
                   choices = c(
                     "income timeseries",
                     "income percentage",
                     "age contribution"
                   ),
                   selected = "income timeseries"),
      radioButtons("value_type", "Select value", 
                   choices = c(
                     "total",
                     "mean"
                   ),
                   selected = "total"),
      radioButtons("sensitivity", "Select sensitivity",
                   choices = c(TRUE, FALSE), 
                   selected = FALSE),
      uiOutput("add_name"),
      uiOutput("add_age"),
      uiOutput("add_unit"),
      uiOutput("use_norm"),
      width = 2
    ),
    mainPanel(
      h5("To minimize data errors, any data exceeding the 99.999% percentile are excluded"),
      plotOutput("Timeseriesplot"),
      width = 10
    )
  )
)

server <- function(input, output, session) {

  output$add_name <- renderUI({
    if (input$plot_type == "income timeseries") {
      checkboxGroupInput(
        "name", "Select name: ",
        choices = c(
          "labour",
          "capital",
          "benefits",
          "total"),
        selected = "total"
      )
    }
    else if (input$plot_type == "age contribution") {
      radioButtons("name", "Select name: ",
                   choices = c(
                     "labour",
                     "capital",
                     "benefits",
                     "total"
                   ),
                   selected = "labour"
                   )}
    })
    
  output$add_age <- renderUI({
    if (input$plot_type == "income timeseries") {
      checkboxGroupInput(
        "age", "Select age: ",
        choices = unique(df_all$age),
        selected = "all"
      )
    }
    else if (input$plot_type == "income percentage") {
      radioButtons("age", "Select age: ",
                   choices = unique(df_all$age),
                   selected = "all"
      )
    }
  })

  output$add_unit <- renderUI({
    if (input$plot_type == "income timeseries") {
      radioButtons(
        "unit", "Select unit: ",
        choices = c("billion", "million", "raw"),
        selected = "raw"
      )
    }
  })
  
  output$use_norm <- renderUI({
    if (input$plot_type == "income timeseries") {
      checkboxGroupInput(
        "norm", "Min/Max norm: ",
        choices = c("True", "False"),
        selected = "False"
      )
    }
  })
  
  output$Timeseriesplot <- renderPlot({
  
    df <- df_all

    if (input$value_type == "total") {
      df$value <- round(as.numeric(df$value1))
    } else {
      df$value <- round(as.numeric(df$value2))
    }
    
    df$value1 <- NULL
    df$value2 <- NULL

    if (input$plot_type == "income timeseries") {

      req(input$name)
      req(input$age)
      req(input$unit)
      req(input$norm)

      input_name <- input$name
      if(input$sensitivity == "TRUE"){
        if ("labour" %in% input$name) {
          input_name <- append(input_name, c("labour_sensitivity"))
        }
        if ("capital" %in% input$name) {
          input_name <- append(input_name, c("capital_sensitivity"))
        }
      }
      
      filtered_data <- df %>% filter(
        age %in% input$age, name %in% input_name
      )
      
      if (input$unit == "billion") {
        filtered_data$value <- filtered_data$value / billion_converter
      }
      if (input$unit == "million") {
        filtered_data$value <- filtered_data$value / million_converter
      }
      
      if (input$norm == "True") {
        filtered_data <- filtered_data %>%
          group_by(age) %>%
          mutate(value = (value - min(value)) / (max(value) - min(value))) %>%
          ungroup()
      }
      
      ggplot(filtered_data, aes(x = year, y = value, color = interaction(name, age))) + 
        geom_line() +
        labs(title = "Time Series", x = "Year", y = "Value", color = "Name and Age") +
        theme_minimal(base_size = 15) +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(size = 20, face = "bold"),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 12),
          panel.border = element_rect(fill = NA, size = 1, color = "black"),
          panel.background = element_rect(fill = "white")
        )
    }
    else if (input$plot_type == "income percentage") {
      req(input$age)
      
      input_name <- input$name
      if(input$sensitivity == "TRUE"){
          input_name <- c("labour_sensitivity", "capital_sensitivity", "benefits")
      }
      else {
        input_name <- c("labour", "capital", "benefits")
      }
      
      filtered_data <- df %>% filter(
        age %in% input$age, name %in% input_name) %>%
        group_by(year, name) %>%
        summarise(total_value = sum(value), .groups = "drop") %>%
        group_by(year) %>%
        mutate(percent = total_value / sum(total_value)) %>%
        ungroup()

      ggplot(filtered_data, aes(x = year, y = percent, fill = name)) + 
        geom_area(position = "stack") +
        scale_y_continuous(labels = scales::percent) + 
        theme_minimal(base_size = 15) +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(size = 20, face = "bold"),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 12),
          panel.border = element_rect(fill = NA, size = 1, color = "black"),
          panel.background = element_rect(fill = "white")
        )
    }
    else if (input$plot_type == "age contribution") {
      
      req(input$name)
      
      input_name <- input$name
      if(input$sensitivity == "TRUE"){
        if ("labour" %in% input$name) {
          input_name <- "labour_sensitivity"
        }
        if ("capital" %in% input$name) {
          input_name <- "capital_sensitivity"
        }
      }
      
      filtered_data <- df %>% filter(
        age != "all", name %in% input_name) %>%
        group_by(year, name, age) %>%
        summarise(total_value = sum(value), .groups = "drop") %>%
        group_by(year, name) %>%
        mutate(percent = total_value / sum(total_value)) %>%
        ungroup()
      
      ggplot(filtered_data, aes(x = year, y = percent, fill = age)) + 
        geom_area(position = "stack") +
        facet_wrap(~ name) +
        scale_y_continuous(labels = scales::percent) + 
        theme_minimal(base_size = 15) +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(size = 20, face = "bold"),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 12),
          panel.border = element_rect(fill = NA, size = 1, color = "black"),
          panel.background = element_rect(fill = "white")
        )
    }
    
  })
    
}

shinyApp(ui = ui, server = server)

  

