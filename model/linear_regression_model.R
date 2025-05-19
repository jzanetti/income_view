
run_linear_regression <- function(train_df) {
  
  trainIndex <- train_df$trainIndex
  x_train <- as.matrix(train_df$x[trainIndex, ])
  x_val <- as.matrix(train_df$x[-trainIndex, ])
  y_train <- train_df$y[trainIndex]
  y_val <- train_df$y[-trainIndex]
  
  print(" - Train LM model ...")
  lr_model <- lm(income ~ ., data = data.frame(x_train , income = y_train))
  
  print(" - Validate LM model ...")
  lr_pred_val <- predict(lr_model, data.frame(x_val))
  lr_pred_val <- unname(unlist(as.list(lr_pred_val)))
  
  print(" - Validate LM model ...")
  rmse <- sqrt(mean((lr_pred_val - y_val)^2)) / billion_converter
  bias <- mean(lr_pred_val - y_val) / billion_converter
  print(paste0(" - Validation bias:", bias))
  print(paste0(" - Validation RMSE:", rmse))
  
  return (list(
    model = lr_model,
    pred = lr_pred_val
  ))
}