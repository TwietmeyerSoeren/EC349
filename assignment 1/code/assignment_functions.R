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
  cv_out_plot <- ggplot(data.frame(lambda = log(cv_out$lambda), cv_deviance = cv_out$cvm), aes(x = lambda, y = cv_deviance)) +
    geom_line() +
    scale_x_continuous(trans = "log", breaks = scales::trans_breaks("log", function(x) 10^x)) +
    labs(x = "Log(lambda)", y = "Cross-Validated Mean Deviance")
  lambda_cv <- cv_out$lambda.min
  
  #Re-Estimate Ridge with lambda chosen by Cross validation
  model<-glmnet(trng_data_x_vals, trng_data_y_vals, alpha = lasso, lambda = lambda_cv, thresh = 1e-12) # what does the thresh mean?
  
  #Fit on Test Data
  predictions <- predict(model, s = lambda_cv, newx = as.matrix(test_data_x_vals))
  mse <- mean((predictions - test_data_y_vals) ^ 2)
  
  results <- list(cv_out, cv_out_plot, lambda_cv, model, predictions, mse)
  return(results)
}


generate_final_df <- function(start, end){
  sliced_df <- review_data_small[start:end, ]
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  
  ## prepare text for analysis:
  corpus <- Corpus(VectorSource(sliced_df$text))
  
  ## Preprocess the text: remove punctuation and stopwords
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stemDocument)
  
  frequencies = DocumentTermMatrix(corpus)
  sparse = removeSparseTerms(frequencies, 0.98)
  tSparse = as.data.frame(as.matrix(sparse))
  
  # Close the parallel backend
  stopCluster(cl)
  
  ## we are now left with a matrix that could then be merged onto the other df because the order remains the same, i.e. row order is the same
  tSparse$review_id <- sliced_df$review_id
  
  ## business data
  cols_keep_business_data <- empty_rows_business_data %>% dplyr::filter(`EmptyRows` < 35000 & !Column %in% c('name', 'address', 'latitude', 'longitude'))
  
  ## tip data:
  num_comments_by_business <- tip_data %>% group_by(business_id) %>% dplyr::summarise(total_comments_business = n())
  
  # user data:
  user_data_selected <- user_data_small  %>% mutate(yelping_since_weeks = round(as.numeric(difftime("2023-12-31 00:00:00", yelping_since, units = "weeks")), digits = 0), num_friends = ifelse(friends != "None", sapply(strsplit(friends, ","), function(x) length(x)), 0), total_compliments = select(., starts_with("compliment_")) %>% rowSums(na.rm = TRUE)) %>% mutate(was_elite = ifelse(nchar(elite) > 1, 1, 0))%>%  select(-c(name, yelping_since, friends))# explain why you aggregated compliments instead of using each individually
  
  # merge other data sets onto review data which is the main one:
  combined_df <- sliced_df %>% left_join(business_data_unnested[, cols_keep_business_data$Column], by = 'business_id', suffix = c('_review', '_business')) %>% left_join(user_data_selected, by = 'user_id', suffix = c('_review', '_user')) %>% left_join(num_comments_by_business, by = 'business_id') %>% left_join(tSparse, by = 'review_id', suffix = c("_review", ""))
  
  return(combined_df)
}