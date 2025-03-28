library(ggplot2)

data <- read.csv("data.csv", header = TRUE)


model <- lm(Height ~ Stories, data=data)

# Extract coefficients (b_0 and b_1)
b_0 <- coef(model)[1]  # Intercept (b_0)
b_1 <- coef(model)[2]  # Slope (b_1)

# Calculate SSE (Sum of Squared Errors)
SSE <- sum(model$residuals^2)  # Sum of squared residuals
cat("Sum of Squared Errors (SSE):", SSE, "\n")


# Print the coefficients
cat("Intercept (b_0):", b_0, "\n")
cat("Slope (b_1):", b_1, "\n")

# Number of observations
n <- nrow(data)

# Degrees of freedom
df <- n - 2

# Compute the residual variance (s^2)
s_squared <- SSE / df

s <- sqrt(s_squared)

# Print the estimated model error variance
cat("Residual Variance (s^2):", s_squared, "\n")
cat("Residual Standard Deviation (s):", s, "\n")




# Scatterplot with manually added regression line using geom_abline()
ggplot(data, aes(x = Stories, y = Height)) +
  geom_point(color = "blue", alpha = 0.6) +          # Add scatterplot points
  geom_abline(intercept = b_0, slope = b_1, color = "red") +  # Add regression line
  labs(
    title = "Height vs. Stories",
    x = "Number of Stories",
    y = "Height"
  ) +
  theme_minimal()  # Use a clean theme for better visualization


# Summary of the model
summary_model <- summary(model)

# Extract coefficients and standard errors
beta_1 <- coef(summary_model)["Stories", "Estimate"]  # Slope estimate
se_beta_1 <- coef(summary_model)["Stories", "Std. Error"]  # Standard error of slope

# Compute the t-statistic
t_stat <- beta_1 / se_beta_1

# Degrees of freedom
df <- summary_model$df[2]  # Residual degrees of freedom (n - 2)

# Compute the p-value (two-tailed test)
p_value <- 2 * pt(-abs(t_stat), df = df)

# Print results
cat("Slope Estimate (beta_1):", beta_1, "\n")
cat("Standard Error of Slope:", se_beta_1, "\n")
cat("t-statistic:", t_stat, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("p-value:", p_value, "\n")

# Decision based on p-value
alpha <- 0.05  # Significance level
if (p_value < alpha) {
  cat("Reject the null hypothesis: There is a significant linear relationship.\n")
} else {
  cat("Fail to reject the null hypothesis: No significant linear relationship.\n")
}


# Confidence Interval for beta_1

# Compute 95% confidence interval for beta_1
conf_interval <- confint(model, level = 0.95)

# Extract CI for beta_1 (second row corresponds to Stories)
ci_beta_1 <- conf_interval[2, ]

# Print the result
cat("95% Confidence Interval for beta_1:\n")
cat("Lower Bound:", round(ci_beta_1[1], 3), "\n")
cat("Upper Bound:", round(ci_beta_1[2], 3), "\n")

# Scatterplot with regression line and confidence interval
ggplot(data, aes(x = Stories, y = Height)) +
  geom_point(color = "blue", alpha = 0.6) +          # Data points
  geom_smooth(
    method = "lm", 
    color = "red", 
    fill = "pink",    # Confidence band color
    se = TRUE,        # Show confidence interval
    level = 0.95      # 95% confidence level
  ) +
  labs(
    title = "Height vs. Stories with 95% Confidence Interval",
    x = "Number of Stories",
    y = "Height"
  ) +
  theme_minimal()


# Extract intercept-specific results
beta_0 <- coef(summary_model)["(Intercept)", "Estimate"]
se_beta_0 <- coef(summary_model)["(Intercept)", "Std. Error"]
t_stat <- coef(summary_model)["(Intercept)", "t value"]
p_value <- coef(summary_model)["(Intercept)", "Pr(>|t|)"]

# Print results
cat("Intercept Estimate (beta_0):", round(beta_0, 3), "\n")
cat("Standard Error of Intercept:", round(se_beta_0, 3), "\n")
cat("t-statistic:", round(t_stat, 3), "\n")
cat("p-value:", p_value, "\n")

# Decision based on p-value
alpha <- 0.05
if (p_value < alpha) {
  cat("Reject H0: The intercept is significantly different from zero.\n")
} else {
  cat("Fail to reject H0: The intercept is not significantly different from zero.\n")
}


#Confidence Interval for beta_0

# Compute 95% confidence interval for beta_0
conf_interval <- confint(model, level = 0.95)

# Extract CI for beta_0 (first row corresponds to the intercept)
ci_beta_0 <- conf_interval[1, ]

# Print the result
cat("95% Confidence Interval for beta_0:\n")
cat("Lower Bound:", round(ci_beta_0[1], 3), "\n")
cat("Upper Bound:", round(ci_beta_0[2], 3), "\n")

# Extract coefficients and CI
beta_0 <- coef(model)[1]
ci_lower <- conf_interval[1, 1]
ci_upper <- conf_interval[1, 2]

# Create a data frame for plotting
ci_data <- data.frame(
  Parameter = "beta_0",
  Estimate = beta_0,
  Lower = ci_lower,
  Upper = ci_upper
)

# Plot using ggplot2
ggplot(ci_data, aes(x = Parameter, y = Estimate)) +
  geom_point(size = 4, color = "red") +           # Point estimate
  geom_errorbar(
    aes(ymin = Lower, ymax = Upper),
    width = 0.2,
    size = 1,
    color = "blue"
  ) +
  labs(
    title = "95% Confidence Interval for beta_0 (Intercept)",
    y = "Coefficient Estimate",
    x = ""
  ) +
  theme_minimal() +
  coord_flip()  # Rotate the plot for better readability



# Compute R^2

# Extract observed and predicted values
y <- data$Height
y_hat <- predict(model)
y_bar <- mean(y)

# Compute SST, SSR, SSE
SST <- sum((y - y_bar)^2)
SSR <- sum((y_hat - y_bar)^2)
SSE <- sum((y - y_hat)^2)

# Compute R-squared
R_squared <- SSR / SST  # Or equivalently: 1 - (SSE / SST)

# Print results
cat("SST (Total Variability):", SST, 2, "\n")
cat("SSR (Explained Variability):", SSR, 2, "\n")
cat("SSE (Unexplained Variability):", SSE, 2, "\n")
cat("R-squared:", round(R_squared, 3), "\n")

# Verify with built-in R values
cat("\nVerification from model summary:\n")
cat("R-squared (from summary):", summary(model)$r.squared, "\n")

# Create a data frame for plotting
plot_data <- data.frame(
  Stories = data$Stories,
  Height = data$Height,
  Fitted = predict(model)
)

# Plot observed, fitted, and mean values
ggplot(plot_data, aes(x = Stories)) +
  geom_point(aes(y = Height), color = "blue", alpha = 0.6) +  # Observed
  geom_line(aes(y = Fitted), color = "red") +                 # Regression line
  geom_hline(yintercept = mean(plot_data$Height), 
             linetype = "dashed", color = "green") +         # Mean of Y
  labs(
    title = "Decomposition of Variability",
    y = "Height",
    x = "Number of Stories"
  ) +
  theme_minimal()


#Plot Residuals

# Residual plot
ggplot(data, aes(x = Stories, y = residuals(model))) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Stories",
       x = "Number of Stories",
       y = "Residuals") +
  theme_minimal()

#Confidence Interval for mean response


