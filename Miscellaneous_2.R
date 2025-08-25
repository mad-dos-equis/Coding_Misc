# Load required packages
library(tidyverse)
library(broom)
library(stargazer)
library(corrplot)

# Assuming your data is called 'trade_data' with columns:
# complexity, import_elasticity, export_elasticity, year, hs6_code

# 1. Basic descriptive statistics
summary_stats <- trade_data %>%
  select(complexity, import_elasticity, export_elasticity) %>%
  summary()
print(summary_stats)

# 2. Correlation matrix
cor_matrix <- cor(trade_data[c("complexity", "import_elasticity", "export_elasticity")], 
                  use = "complete.obs")
print(cor_matrix)

# Visualize correlations
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black")

# 3. Simple scatter plots
p1 <- ggplot(trade_data, aes(x = complexity, y = import_elasticity)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Import Elasticity vs Product Complexity",
       x = "Product Complexity", 
       y = "Import Elasticity") +
  theme_minimal()

p2 <- ggplot(trade_data, aes(x = complexity, y = export_elasticity)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Export Elasticity vs Product Complexity",
       x = "Product Complexity", 
       y = "Export Elasticity") +
  theme_minimal()

print(p1)
print(p2)

# 4. Basic linear regression models
# Import elasticity model
import_model1 <- lm(import_elasticity ~ complexity, data = trade_data)
import_model2 <- lm(import_elasticity ~ complexity + I(complexity^2), data = trade_data)

# Export elasticity model  
export_model1 <- lm(export_elasticity ~ complexity, data = trade_data)
export_model2 <- lm(export_elasticity ~ complexity + I(complexity^2), data = trade_data)

# 5. Model summaries
cat("=== IMPORT ELASTICITY MODELS ===\n")
summary(import_model1)
cat("\n=== IMPORT ELASTICITY (QUADRATIC) ===\n")
summary(import_model2)

cat("\n=== EXPORT ELASTICITY MODELS ===\n")
summary(export_model1)
cat("\n=== EXPORT ELASTICITY (QUADRATIC) ===\n")
summary(export_model2)

# 6. Model comparison
# AIC comparison
import_aic <- AIC(import_model1, import_model2)
export_aic <- AIC(export_model1, export_model2)

cat("\n=== MODEL COMPARISON (AIC) ===\n")
print("Import Models:")
print(import_aic)
print("Export Models:")  
print(export_aic)

# 7. Tidy results for easy interpretation
import_results <- map_dfr(list("Linear" = import_model1, "Quadratic" = import_model2), 
                         tidy, .id = "Model")
export_results <- map_dfr(list("Linear" = export_model1, "Quadratic" = export_model2), 
                         tidy, .id = "Model")

print("Import Elasticity Results:")
print(import_results)
print("Export Elasticity Results:")
print(export_results)

# 8. Residual diagnostics
par(mfrow = c(2, 2))
plot(import_model1, main = "Import Model Diagnostics")
par(mfrow = c(2, 2))
plot(export_model1, main = "Export Model Diagnostics")

# 9. Check for robustness - remove potential outliers
# Calculate standardized residuals and flag outliers
trade_data$import_fitted <- fitted(import_model1)
trade_data$import_residuals <- residuals(import_model1)
trade_data$import_std_resid <- rstandard(import_model1)

trade_data$export_fitted <- fitted(export_model1)
trade_data$export_residuals <- residuals(export_model1)
trade_data$export_std_resid <- rstandard(export_model1)

# Flag observations with |standardized residuals| > 2.5
outliers <- trade_data %>%
  filter(abs(import_std_resid) > 2.5 | abs(export_std_resid) > 2.5)

cat("\n=== POTENTIAL OUTLIERS ===\n")
print(paste("Number of potential outliers:", nrow(outliers)))
print(paste("Percentage of data:", round(100 * nrow(outliers)/nrow(trade_data), 2), "%"))

# Rerun models without outliers
clean_data <- trade_data %>%
  filter(abs(import_std_resid) <= 2.5 & abs(export_std_resid) <= 2.5)

import_model_clean <- lm(import_elasticity ~ complexity, data = clean_data)
export_model_clean <- lm(export_elasticity ~ complexity, data = clean_data)

cat("\n=== MODELS WITHOUT OUTLIERS ===\n")
cat("Import Model (Clean):\n")
summary(import_model_clean)
cat("\nExport Model (Clean):\n")  
summary(export_model_clean)

# 10. Create a nice output table
stargazer(import_model1, import_model2, export_model1, export_model2,
          type = "text",
          title = "Product Complexity and Trade Elasticity Relationships",
          column.labels = c("Import Linear", "Import Quad", "Export Linear", "Export Quad"),
          dep.var.labels = c("Import Elasticity", "Export Elasticity"),
          covariate.labels = c("Complexity", "Complexity Squared"),
          omit.stat = c("f", "ser"),
          digits = 4)
