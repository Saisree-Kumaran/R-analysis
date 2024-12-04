
install.packages("readxl")
install.packages("tidyverse")
install.packages("caret")
install.packages("smotefamily") # Replacing DMwR for SMOTE
install.packages("randomForest")
install.packages("gbm")
install.packages("pROC")
install.packages("reshape2")


library(readxl)
library(tidyverse)
library(caret)
library(smotefamily) # For SMOTE
library(randomForest)
library(gbm)
library(pROC)
library(reshape2)


data <- read_excel("/Users/saisreekumaran/Desktop/R_Data.xlsx")

# Data Preprocessing

data <- na.omit(data)
data <- data %>% distinct()

# Encoding of Categorical Variables

data$Month <- as.factor(data$Month)
data$VisitorType <- as.factor(data$VisitorType)
str(data)
data <- data %>%
  mutate(
    BounceRates = as.numeric(BounceRates),
    ExitRates = as.numeric(ExitRates),
    PageValues = as.numeric(PageValues)
  )

# Normalization Of Dataset 
normalize <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
data <- data %>%
  mutate(across(c(BounceRates, ExitRates, PageValues), normalize))
data <- data %>%
  mutate(
    Month = as.numeric(Month),
    VisitorType = as.numeric(VisitorType)
  )
numeric_data <- data %>%
  select_if(is.numeric)

#SMOTE Analysis

data$Revenue <- as.factor(data$Revenue) 
smote_data <- SMOTE(X = numeric_data[, -ncol(numeric_data)], target = data$Revenue, K = 5, dup_size = 0)
data <- smote_data$data
data$Revenue <- as.factor(data$class) 
data$class <- NULL




# Exploratory Data Analysis (EDA)

summary(data)

# Correlation Analysis
num_vars <- select_if(data, is.numeric)
cor_matrix <- cor(num_vars)
cor_target <- cor(num_vars, as.numeric(data$Revenue), use = "complete.obs")
print(cor_target)

# Feature Distribution for Skewness (PageValues)
ggplot(data, aes(PageValues)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(title = "Distribution of Page Values", x = "Page Values", y = "Frequency")

# Visualization
# Revenue Distribution
ggplot(data, aes(Revenue)) +
  geom_bar(fill = "green") +
  labs(title = "Revenue Distribution", x = "Revenue", y = "Count")

# Box Plot for Page Values
ggplot(data, aes(x = Revenue, y = PageValues)) +
  geom_boxplot() +
  labs(title = "Boxplot of Page Values", x = "Revenue", y = "Page Values")

# Scatterplot of Bounce vs Exit Rates
ggplot(data, aes(x = BounceRates, y = ExitRates, color = Revenue)) +
  geom_point(alpha = 0.7) +
  labs(title = "Scatterplot of Bounce Rates vs Exit Rates", x = "Bounce Rates", y = "Exit Rates")

# Heatmap
heatmap_data <- cor_matrix
heatmap_data <- melt(heatmap_data)
ggplot(heatmap_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Model Building
# Split Dataset
set.seed(123)
trainIndex <- createDataPartition(data$Revenue, p = 0.8, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

# Random Forest Model
rf_model <- randomForest(Revenue ~ ., data = train, importance = TRUE, ntree = 100)
rf_pred <- predict(rf_model, test)
print(confusionMatrix(rf_pred, test$Revenue))

# Convert Revenue to 0 and 1
train$Revenue <- as.numeric(train$Revenue) - 1
test$Revenue <- as.numeric(test$Revenue) - 1

# Gradient Boosting Model
gbm_model <- gbm(
  Revenue ~ ., 
  data = train, 
  distribution = "bernoulli", 
  n.trees = 100, 
  interaction.depth = 3, 
  shrinkage = 0.01, 
  cv.folds = 5
)

# Predictions
gbm_pred <- predict(gbm_model, test, n.trees = 100, type = "response")
gbm_pred_class <- ifelse(gbm_pred > 0.5, 1, 0)

# Confusion Matrix
print(confusionMatrix(as.factor(gbm_pred_class), as.factor(test$Revenue)))

# Ensure test$Revenue is numeric and coded as 0 and 1
test$Revenue <- as.numeric(as.factor(test$Revenue)) - 1

# ROC Curve for Random Forest
rf_roc <- roc(test$Revenue, as.numeric(rf_pred), levels = c(0, 1), direction = "<")

# ROC Curve for Gradient Boosting
gbm_roc <- roc(test$Revenue, gbm_pred, levels = c(0, 1), direction = "<") # Use predicted probabilities for Gradient Boosting

# Plot the ROC Curves
plot(rf_roc, col = "blue", main = "ROC Curve Comparison")
plot(gbm_roc, col = "red", add = TRUE)
legend("bottomright", legend = c("Random Forest", "Gradient Boosting"), col = c("blue", "red"), lwd = 2)

install.packages("corrplot")
library(corrplot)
numeric_data <- data[sapply(data, is.numeric)]
cor_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.cex = 0.8, number.cex = 0.7,
         title = "Correlation Heatmap")


install.packages("ggplot2")
library(ggplot2)
ggplot(data, aes(x = Revenue)) +
  geom_bar(fill = "skyblue") +
  ggtitle("Distribution of Revenue") +
  xlab("Revenue (True/False)") +
  ylab("Count") +
  theme_minimal()

install.packages("ggplot2")
library(ggplot2)
ggplot(monthly_revenue, aes(x = Month, y = Count, color = Revenue, group = Revenue)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("blue", "red")) +  # Customize colors
  ggtitle("Distribution of Revenue per Month") +
  xlab("Month") +
  ylab("Count of Revenue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


unique(data$Month)
# Count missing values
sum(is.na(data$Month))
data <- data[!is.na(data$Month), ]
data$Month[is.na(data$Month)] <- "Unknown"
data$Month <- factor(data$Month, 
                     levels = 1:12, 
                     labels = c("January", "February", "March", "April", "May", 
                                "June", "July", "August", "September", 
                                "October", "November", "December"))
install.packages("ggplot2")
library(ggplot2)
monthly_revenue <- as.data.frame(table(data$Month, data$Revenue))
colnames(monthly_revenue) <- c("Month", "Revenue", "Count")
ggplot(monthly_revenue, aes(x = Month, y = Count, fill = Revenue)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Revenue Distribution Across Months") +
  xlab("Month") +
  ylab("Count of Revenue") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


install.packages("ggplot2")
library(ggplot2)
ggplot(data, aes(x = BounceRates, y = ExitRates, color = Revenue)) +
  geom_point(alpha = 0.6) +
  ggtitle("Bounce Rates vs Exit Rates by Revenue") +
  xlab("Bounce Rates") +
  ylab("Exit Rates") +
  theme_minimal()

#Gradient Boost model
install.packages("xgboost")
install.packages("caret")
install.packages("ggplot2")

library(xgboost)
library(caret)
library(ggplot2)
set.seed(123)  # For reproducibility

# Split data (80% train, 20% test)
train_index <- createDataPartition(data$Revenue, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
X_train <- as.matrix(train_data[, -which(names(train_data) == "Revenue")])
y_train <- as.numeric(train_data$Revenue)

X_test <- as.matrix(test_data[, -which(names(test_data) == "Revenue")])
y_test <- as.numeric(test_data$Revenue)
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# Train the model
xgb_model <- xgboost(
  data = dtrain,
  max_depth = 6,           # Maximum depth of trees
  eta = 0.3,               # Learning rate
  nrounds = 100,           # Number of boosting rounds
  objective = "binary:logistic",  # Binary classification
  eval_metric = "auc",     # Evaluation metric
  verbose = 0              # Suppress training logs
)
predictions <- predict(xgb_model, X_test)

# Convert probabilities to binary predictions (0/1)
binary_predictions <- ifelse(predictions > 0.5, 1, 0)
confusionMatrix(as.factor(binary_predictions), as.factor(y_test))
importance_matrix <- xgb.importance(model = xgb_model)

# Print feature importance
print(importance_matrix)

importance_df <- as.data.frame(importance_matrix)

# Plot with ggplot2
install.packages("ggplot2")
library(ggplot2)
ggplot(importance_df, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  ggtitle("Feature Importance") +
  xlab("Features") +
  ylab("Importance (Gain)") +
  theme_minimal()



# Cleaning for revenue: changing it to binary.
str(train_data$Revenue)
unique(train_data$Revenue)
train_data$Revenue <- ifelse(train_data$Revenue == "TRUE", 1, 0)
test_data$Revenue <- ifelse(test_data$Revenue == "FALSE", 1, 0)
unique(train_data$Revenue)
unique(test_data$Revenue)
install.packages("gbm")
library(gbm)

# Training the model
gbm_model <- gbm(
  formula = Revenue ~ ., 
  data = train_data, 
  distribution = "bernoulli", 
  n.trees = 100, 
  shrinkage = 0.01, 
  interaction.depth = 3
)

X_test <- test_data[, -which(names(test_data) == "Revenue")]

gbm_predictions <- predict(gbm_model, newdata = X_test, n.trees = 100, type = "response"),
                             
                             
