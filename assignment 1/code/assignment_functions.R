# functions
count_empty_rows <- function(dataframe) {
  result <- data.frame(
    Column = character(),
    EmptyRows = numeric(),
    stringsAsFactors = FALSE
  )
  for (col in names(dataframe)) {
    empty_rows <- sum(is.na(dataframe[[col]]))
    result <- rbind(result, data.frame(Column = col, EmptyRows = empty_rows))
  }
  
  return(result)
}

trend_plot <- function(x_values){
  plot <- ggplot(review_data_combined, mapping = aes(x = !!sym(x_values), y = stars_review)) +
    geom_hex() + geom_smooth(method = "glm", formula = y ~ x + x^2 + x^3) + ylim(1,5)
  return(plot)
}

shrinkage_estimator_computation <- function(lasso, trng_data_x_vals, trng_data_y_vals, test_data_x_vals, test_data_y_vals) {
  cv_out <- cv.glmnet(as.matrix(trng_data_x_vals), as.matrix(trng_data_y_vals), alpha = lasso, nfolds = 3) # adapt the number of folds
  cv_out_plot <- plot(cv_out)
  lambda_cv <- cv_out$lambda.min
  
  #Re-Estimate Ridge with lambda chosen by Cross validation
  model<-glmnet(trng_data_x_vals, data_train_y, alpha = lasso, lambda = lambda_cv, thresh = 1e-12) # what does the thresh mean?
  
  #Fit on Test Data
  predictions <- predict(ridge_model, s = lambda_ridge_cv, newx = as.matrix(test_data_x_vals))
  mse <- mean((ridge_predictions - test_data_y_vals) ^ 2)
  
  results <- list(cv_out, cv_out_plot, lambda_cv, model, predictions, mse)
  return(results)
}