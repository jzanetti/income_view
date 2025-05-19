

run_xgboost <- function(train_df) {

  print("Run XGBoost:")
  print(" - Split data for training and validation ...")
  trainIndex <- train_df$trainIndex
  x_train <- as.matrix(train_df$x[trainIndex, ])
  x_val <- as.matrix(train_df$x[-trainIndex, ])
  y_train <- train_df$y[trainIndex]
  y_val <- train_df$y[-trainIndex]
  
  print(" - Train XGBoost model ...")
  xgb_params <- list(
    objective = "reg:squarederror",
    eta = 0.1,
    max_depth = 5,
    nthread = 2
  )
  
  xgb_model <- xgboost(
    data = x_train,
    label = y_train,
    params = xgb_params,
    nrounds = 100,
    verbose = 0
  )

  print(" - Validate XGBoost model ...")
  xgb_pred_val <- predict(xgb_model, x_val)
  rmse <- sqrt(mean((xgb_pred_val - y_val)^2)) / billion_converter
  bias <- mean(xgb_pred_val - y_val) / billion_converter
  print(paste0(" - Validation bias:", bias))
  print(paste0(" - Validation RMSE:", rmse))
  
  return (
    list(
      model = xgb_model,
      pred = xgb_pred_val,
      x_val=x_val
    )
  )
  
}