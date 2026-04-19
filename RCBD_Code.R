
# 1. Load the Data
data <- read.csv("/Users/opixdown/Downloads/dataset_memoryrecall.csv")

# 2. Data Cleaning & Preparation
colnames(data) <- c("ID", "Block", "Treatment", "Score")

# Convert Factor variables 
data$Treatment <- as.factor(data$Treatment)
data$Block <- as.factor(data$Block)

# Check structure
str(data)


boxplot(Score ~ Treatment, data = data,
        main = "EtchRate (Recall) by Power (Treatment)",
        col = c("#5dade2", "#eb984e"),
        ylab = "Score out of 10")

# 4. RCBD ANOVA Model

model <- aov(Score ~ Treatment + Block, data = data)
summary(model)

# ---------------- Residual Analysis ----------------
res <- resid(model)

# 5. Normal Probability (Q-Q) Plot
qqnorm(res, main = "Normal Probability Plot of Residuals")
qqline(res, col = "red", lwd = 2)

# 6. Residuals vs Run Order
plot(1:nrow(data), res,
     main = "Residuals vs Run Order",
     ylab = "Residuals",
     xlab = "Observation Order")


# 7. Residuals vs Fitted Values
fitted_vals <- predict(model)
plot(fitted_vals, res,
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values (Predicted)",
     ylab = "Residuals")
abline(h = 0, col = "red")

# 8. Equality of Variance Test
bartlett.test(Score ~ Treatment, data = data)

