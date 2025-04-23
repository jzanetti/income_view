library(readxl)
library(dplyr)
source("constants.R")

# Function to replace values in a specified column based on name_mapping's 'name' field
replace_col_values <- function(data, column_name = "name", mapping = NAME_MAPS) {
  
  # Replace values in the specified column
  data[[column_name]] <- sapply(data[[column_name]], function(x) {
    if (x %in% names(mapping)) {
      paste(mapping[[x]]$name, mapping[[x]]$source, sep = "+") # Return the meaningful name
    } else {
      x  # Keep the original value if no match
    }
  })
  return(data)
}


read_data <- function() {
  data_num_records <- read_excel(INCOME_DATA_PATH, sheet = "data1", skip=1)
  data_value_records <- read_excel(INCOME_DATA_PATH, sheet = "data2", skip=1)
  data_value_records <- replace_col_values(data_value_records)

  cpi_records <- read_excel(CPI_DATA_PATH_RBNZ, sheet = "Data", skip=4)
  cpi_records <- cpi_records[, c("Series Id", "CPI.Q.C.ia")]
  names(cpi_records) <- c("date", "CPI")
  cpi_records <- cpi_records %>%
    mutate(year = substr(date, 1, 4),
           CPI = as.numeric(CPI)) %>%
    group_by(year) %>%
    summarise(mean_cpi = mean(CPI, na.rm = TRUE))
  base_cpi_year <- 2000
  base_cpi_records <- cpi_records$mean_cpi[cpi_records$year == base_cpi_year]
  cpi_records$scaled_cpi <- cpi_records$mean_cpi / base_cpi_records
  cpi_records$year <- as.numeric(cpi_records$year)
  
  return (
    list(
      data_num_records = data_num_records,
      data_value_records = data_value_records,
      cpi_records = cpi_records
    )
  )
}