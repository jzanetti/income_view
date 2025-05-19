# Load required libraries
library(xgboost)
library(caret)
library(tidyverse)
library(readxl)
source("constant.R")
source("data.R")
source("xgboost_model.R")
source("linear_regression_model.R")

billion_converter <- 1000000000.0

# -------------------------------
# Obtain input dataset
# -------------------------------
train_df <- get_input_data(map_new_age = TRUE)
print("Run XGB ...")
xgb_model <- run_xgboost(train_df)

print("Run Linear regression ...")
lr_model <- run_linear_regression(train_df)

output_all <- list()
index <- 1
for (year in c(2023, 2028, 2033)) {
  proj_data <- get_proj_data(year, train_df$scaler)
  browser()
  output <- predict(xgb_model$model, as.matrix(proj_data))
  output <- list(
    value = sum(output),
    year = year
  )
  output_all[[index]] <- output
  index <- index + 1
}

output_all <- bind_rows(output_all)
