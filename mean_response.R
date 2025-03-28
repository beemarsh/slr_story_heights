# ---------------------------------------------------
# 1) Load packages
# ---------------------------------------------------
library(ggplot2)

# ---------------------------------------------------
# 2) Read the data (replace with read.csv(...) in practice)
# ---------------------------------------------------
data <- read.csv("data.csv", header = TRUE)

model <- lm(Height ~ Stories, data = data)

summary(model)
# ---------------------------------------------------
# 3) Fit the linear model

# ---------------------------------------------------
# 4) Create a sequence of Stories (x) values for prediction
# ---------------------------------------------------
x_values <- seq(min(data$Stories), max(data$Stories), length.out = 100)

# ---------------------------------------------------
# 5) Generate predictions + 95% confidence intervals
# ---------------------------------------------------
preds <- predict(
  model,
  newdata = data.frame(Stories = x_values),
  interval = "confidence",  # intervals for the mean response
  level = 0.95
)

# Combine predictions in a data frame
ci_data <- data.frame(
  Stories = x_values,
  fit = preds[, "fit"],
  lower = preds[, "lwr"],
  upper = preds[, "upr"]
)

# ---------------------------------------------------
# 6) Plot data + fitted line + confidence intervals
# ---------------------------------------------------
ggplot() +
  geom_point(data = data, aes(x = Stories, y = Height), color = "blue", alpha = 0.6) +
  geom_line(data = ci_data, aes(x = Stories, y = fit), color = "red") +
  geom_ribbon(data = ci_data, aes(x = Stories, ymin = lower, ymax = upper),
              fill = "pink", alpha = 0.3) +
  labs(
    title = "Height vs. Stories with 95% CI for Mean Response",
    x = "Number of Stories",
    y = "Height"
  ) +
  theme_minimal()

