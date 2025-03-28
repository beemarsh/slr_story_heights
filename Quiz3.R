
install.packages("ggplot2")  # Install ggplot2 if not already installed
library(ggplot2)

# Load dataset
Salaries <- read.csv("Sample_SalaryData.csv", header = TRUE)

# Fit linear model
model <- lm(Salary ~ YearsExperience, data=Salaries)

# Plot with ggplot2 (Automatically adds 95% CI)
dev.new()
ggplot(Salaries, aes(x = YearsExperience, y = Salary)) +
  geom_point(color = "blue", size = 3) +   # Scatter plot
  geom_smooth(method = "lm", color = "red", fill = "pink", level = 0.95) + # Regression line with 95% CI
  labs(title = "Regression Line with 95% Confidence Interval",
       x = "Years of Experience", 
       y = "Salary") +
  theme_minimal()


# Plot Normal Probability Plot (Q-Q Plot)
dev.new()
qqnorm(residuals(model), pch = 16, col = "blue", 
       main = "Normal Q-Q Plot of Residuals")
qqline(residuals(model), col = "red", lwd = 2) # Add reference line


# print data
print(Salaries)

b <- coefficients(model)

cat("The estimate of regression line is y =",b[1],"+",b[2],"*x")

#print out the outcomes of the SLR model
summary(model)
