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

trend_plot <- function(x_values, limit){
  plot <- ggplot(review_data_combined %>% filter(!!sym(x_values)<= I(0.5^limit)*max(!!sym(x_values), na.rm = TRUE)), mapping = aes(x = !!sym(x_values), y = stars_review)) +
    geom_hex() + geom_smooth(method = "glm", formula = y ~ x + I(x^2) + I(x^3)) + ylim(1,5)
  return(plot)
}

shrinkage_estimator_computation <- function(lasso, trng_data_x_vals, trng_data_y_vals, test_data_x_vals, test_data_y_vals) {
  cv_out <- cv.glmnet(as.matrix(trng_data_x_vals), as.matrix(trng_data_y_vals), alpha = lasso, nfolds = 10)
  cv_out_plot <- ggplot(data.frame(lambda = log(cv_out$lambda), cv_deviance = cv_out$cvm), aes(x = lambda, y = cv_deviance)) +
    geom_line() +
    scale_x_continuous(trans = "log", breaks = scales::trans_breaks("log", function(x) 10^x)) +
    labs(x = "Log(lambda)", y = "Mean Squared Error")
  lambda_cv <- cv_out$lambda.min
  
  #Re-Estimate Ridge with lambda chosen by Cross validation
  model<-glmnet(trng_data_x_vals, trng_data_y_vals, alpha = lasso, lambda = lambda_cv, thresh = 1e-12) # what does the thresh mean?
  
  #Fit on Test Data
  predictions <- predict(model, s = lambda_cv, newx = as.matrix(test_data_x_vals))
  mse <- mean((predictions - test_data_y_vals) ^ 2)
  rsq <- 1 - mse / var(test_data_y_vals)
  
  # coefficients
  coefficients_model <- coef(model)[,] %>% as.data.frame() %>% rownames_to_column(var = "term") %>% setNames(c("term", "coefficient"))
  coef_model_df_high_low <- rbind(coefficients_model[37:nrow(coefficients_model), ] %>% arrange(desc(coefficient)) %>% dplyr::slice(1:7), coefficients_model[37:nrow(coefficients_model), ] %>% arrange(coefficient) %>% dplyr::slice(1:7)) %>% arrange(desc(coefficient))
  
  # Fit on Training Data
  predictions_training <- predict(model, s = lambda_cv, newx = as.matrix(trng_data_x_vals))
  mse_training <- mean((predictions_training - trng_data_y_vals) ^ 2)
  rsq_training <- 1 - mse_training / var(trng_data_y_vals)
  
  results <- list(cv_out = cv_out,
                  cv_out_plot = cv_out_plot,
                  lambda_cv = lambda_cv,
                  model = model,
                  predictions = predictions,
                  mse = mse,
                  rsq = rsq,
                  predictions_training = predictions_training,
                  mse_training = mse_training,
                  rsq_training = rsq_training, 
                  coefficients_model = coefficients_model,
                  coef_model_df_high_low = coef_model_df_high_low)
  return(results)
}


generate_final_df <- function(start, end, comb_df){
  df <- comb_df[start:end, ]
  ## add text
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  
  ## prepare text for analysis:
  corpus <- Corpus(VectorSource(df$text))
  
  ## Preprocess the text: remove punctuation and stop words
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stemDocument)
  
  frequencies = DocumentTermMatrix(corpus)
  sparse = removeSparseTerms(frequencies, 0.98)
  tSparse = as.data.frame(as.matrix(sparse))
  
  # Close the parallel back-end
  stopCluster(cl)
  
  ## we are now left with a matrix that could then be merged onto the other df because the order remains the same, i.e. row order is the same
  tSparse$review_id <- df$review_id
  
  part_df <- df %>% left_join(tSparse, by = 'review_id', suffix = c("_review", ""))
  return(part_df)
}
