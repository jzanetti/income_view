
library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(shinyjs)
library(DT) 
library(tidyr)
library(dplyr)

billion_converter <- 1000000000.0
million_converter <- 1000000.0

data_path <- "data_to_check - without_raw_data - v3.0.xlsx"
proj_path <- "income_proj.csv"
pop_scaler_path <- "scaled_pop.csv"
cpi_scaler_path <- "scaled_cpi.csv"

df_all <- read_excel(data_path, sheet = "data", skip = 1)
df_proj <- read.csv(proj_path)
df_pop_scaler <- read.csv(pop_scaler_path)[, c("year", "age", "count")]
df_cpi_scaler <- read.csv(cpi_scaler_path)[, c("year", "scaled_cpi")]

df_cpi_scaler <- df_cpi_scaler %>%
  filter(year >= 2000, year <= 2023)

df_all <- df_all %>%
  rename(
    "year" = "...1",
    "name" = "...2",
    "age" = "...3",
    "value1" = "To release...4",
    "value2" = "To release...5"
  )

df_all <- df_all[, c("year", "name", "age", "value1", "value2")]

df_all <- df_all %>%
  left_join(select(df_pop_scaler, year, age, count), by = c("year", "age")) %>%
  mutate(value1_pop_scaled = value1 / count) %>%
  select(-count)

df_all <- df_all %>%
  left_join(select(df_cpi_scaler, year, scaled_cpi), by = c("year")) %>%
  mutate(value1_cpi_scaled = value1 / scaled_cpi) %>%
  select(-scaled_cpi)

joint_scaler <- df_pop_scaler %>%
  left_join(select(df_cpi_scaler, year, scaled_cpi), by = "year") %>%
  mutate(pop_cpi_scaler = count * scaled_cpi) %>%
  select(-scaled_cpi) %>%
  select(-count)

df_all <- df_all %>%
  left_join(select(joint_scaler, year, age, pop_cpi_scaler), by = c("year", "age")) %>%
  mutate(value1_pop_cpi_scaled = value1 / pop_cpi_scaler) %>%
  select(-pop_cpi_scaler)

df_all <- df_all %>%
  # Pivot the value1-related columns into long format
  pivot_longer(
    cols = c(value1, value1_pop_scaled, value1_cpi_scaled, value1_pop_cpi_scaled),
    names_to = "scaler",
    values_to = "value1_temp"
  ) %>%
  # Rename scaler values and handle value2's scaler
  mutate(
    scaler = case_when(
      scaler == "value1" ~ "No Scaler",
      scaler == "value1_pop_scaled" ~ "Population Scaler",
      scaler == "value1_cpi_scaled" ~ "CPI Scaler",
      scaler == "value1_pop_cpi_scaled" ~ "Pop + CPI Scaler"
    )
  ) %>%
  rename(value1 = value1_temp)

df_all <- df_all %>%
  mutate(value2 = if_else(
    scaler %in% c("Population Scaler", "CPI Scaler", "Pop + CPI Scaler"), 
    NA, 
    value2))


ui <- fluidPage(
  useShinyjs(),
  titlePanel("Taxable income time series analysis"),
  tabsetPanel(
  tabPanel(
    title = "Recorded income",
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
        checkboxGroupInput(
          "age", "Select age: ",
          choices = unique(df_all$age),
          selected = "all"
        ),
        radioButtons("sensitivity", "Select sensitivity",
                     choices = c(TRUE, FALSE), 
                     selected = FALSE),
        uiOutput("add_name"),
        uiOutput("add_unit"),
        uiOutput("add_scaler"),
        uiOutput("use_norm"),
        width = 2
      ),
      mainPanel(
        h5("To minimize data errors, any data exceeding the 99.999% percentile are excluded"),
        plotOutput("Timeseriesplot"),
        width = 10
      )
    )),
  # Tab 2: Placeholder for Additional Content
  tabPanel(
    title = "Projected income",
    sidebarLayout(
      sidebarPanel(
        radioButtons("plot_type2", "Select plot type", 
                     choices = c(
                       "total",
                       "labour",
                       "capital",
                       "benefits"
                     ),
                     selected = "total"),
        radioButtons("sensitivity2", "Select sensitivity",
                     choices = c(TRUE, FALSE), 
                     selected = FALSE),
        radioButtons(
          "unit2", "Select unit: ",
          choices = c("billion", "million", "raw"),
          selected = "raw"
        ),
        checkboxGroupInput("Scenario", "Select scenario", 
                     choices = c(
                       "Median",
                       "5th percentile",
                       "25th percentile",
                       "75th percentile",
                       "95th percentile",
                       "No immigration",
                       "Cyclic immigration",
                       "High immigration"
                     ),
                     selected = "Median"),
        width = 2
      ),
      mainPanel(
        h5("A Stacked model (Linear regression + XGBoost + Densely connected neural network) is applied"),
        plotOutput("Projplot"),
        width = 10
      )
    )
  ),
  # Tab 3: data download
  tabPanel(
    title = "Data download",
    selectInput("dataset_choice", "Choose dataset:",
                choices = c("Income Data", "Population structure projection (Stats NZ)"),
                selected = "Income Data"),
    DTOutput("table"),
    downloadButton("download_csv", "Download as CSV")
  )
  
  )
)

server <- function(input, output, session) {
  
  # Reactive expression to return the selected dataset
  selected_data <- reactive({
    # Replace these with your actual data frames or data processing
    if (input$dataset_choice == "Income Data") {
      df <- df_all[, c("year", "name", "age", "value1")] %>% 
        rename("income" = "value1")
    } else if (input$dataset_choice == "Population structure projection (Stats NZ)") {
      df <- df_proj
    } 
    return(df)
  })
  
  output$table <- renderDT({ 
    datatable(selected_data(), options = list(pageLength = 10, autoWidth = TRUE)) 
    }
  )
  
  output$download_csv <- downloadHandler(
    
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "") 
    }, 
    content = function(file) { 
      write.csv(df_all, file, row.names = FALSE) 
      } 
  )
  
  output$Projplot <- renderPlot({

    req(input$plot_type2)
    req(input$sensitivity2)
    req(input$Scenario)
    req(input$unit2)
    
    df <- df_proj
    
    input_income_types <- input$plot_type2
    if(input$sensitivity2 == "TRUE"){
      if ("labour" == input_income_types) {
        input_income_types <- "labour_sensitivity"
      }
      if ("capital" == input_income_types) {
        input_income_types <- "capital_sensitivity"
      }
    }
    
    
    proj_data <- df %>% filter(
      scenario %in% input$Scenario, status == "predicted", income_type == input_income_types
    )
    proj_data <- proj_data[, c("year", "all_income", "scenario")]
    obs_data <- df %>% filter(
      status == "observed", income_type == input_income_types
    )
    obs_data <- obs_data[, c("year", "all_income")]
    obs_data$scenario = "history"

    df <- bind_rows(obs_data, proj_data)
    
    if (input$unit2 == "million") {
      df$all_income <- df$all_income / million_converter
    } else if (input$unit2 == "billion") {
      df$all_income <- df$all_income / billion_converter
    }
    
    ggplot(df, aes(x = year, y = all_income, color=scenario)) + 
      geom_line() +
      labs(title = "Time Series", x = "Year", y = "Value") +
      theme_minimal(base_size = 15) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        panel.border = element_rect(fill = NA, size = 1, color = "black"),
        panel.background = element_rect(fill = "white")
      )
  })
  
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

  output$add_scaler <- renderUI({
    if ((input$plot_type == "income timeseries" & input$value_type == "total")) {
      checkboxGroupInput(
        "scaler", "Select scaler: ",
        choices = c("No Scaler", "Population Scaler", "CPI Scaler", "Pop + CPI Scaler"),
        selected = "No Scaler"
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
      radioButtons(
        "norm", "Min/Max norm: ",
        choices = c("True", "False"),
        selected = "False",
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
      req(input$scaler)
      
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
        age %in% input$age, scaler %in% input$scaler, name %in% input_name
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

      ggplot(filtered_data, aes(x = year, y = value, color = interaction(name, age, scaler))) + 
        geom_line() +
        labs(title = "Time Series", x = "Year", y = "Value", color = "Name, Age and Scaler") +
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
        age %in% input$age, name %in% input_name) %>%
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

  

