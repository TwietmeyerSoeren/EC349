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