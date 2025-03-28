library(ggplot2)

data <- read.csv("data.csv", header = TRUE)

model <- lm(Height ~ Stories, data = data)

summary(model)

# Check if there are replicated x-values (required for lack-of-fit test)
if (length(unique(data$Stories)) < length(data$Stories)) {
  # Perform lack-of-fit test
  
  
  anova_model <- anova(model)
  
  print(anova_model)
  
  pure_error_df <- nrow(data) - length(unique(data$Stories))
  lack_of_fit_df <- length(unique(data$Stories)) - 2  # k - 2 (k = unique x-values)
  
  # Calculate Pure Error SS and Lack-of-Fit SS
  SSE <- sum(resid(model)^2)
  unique_x <- unique(data$Stories)
  pure_error_SS <- 0
  
  for (x in unique_x) {
    y_values <- data$Height[data$Stories == x]
    y_mean <- mean(y_values)
    pure_error_SS <- pure_error_SS + sum((y_values - y_mean)^2)
  }
  
  lack_of_fit_SS <- SSE - pure_error_SS
  
  # F-test
  MSE_pure <- pure_error_SS / pure_error_df
  F_stat <- (lack_of_fit_SS / lack_of_fit_df) / MSE_pure
  p_value <- pf(F_stat, lack_of_fit_df, pure_error_df, lower.tail = FALSE)
  
  cat("Lack-of-Fit Test Results:\n")
  cat("F-statistic:", F_stat, "\n")
  cat("P-value:", p_value, "\n")
  
  if (p_value < 0.05) {
    cat("Conclusion: Reject H0. The linear model is inadequate (evidence of nonlinearity).\n")
  } else {
    cat("Conclusion: Fail to reject H0. The linear model is adequate.\n")
  }
  
} else {
  cat("Cannot perform lack-of-fit test: No replicated x-values.\n")
}

data$Stories # This line is corrected.