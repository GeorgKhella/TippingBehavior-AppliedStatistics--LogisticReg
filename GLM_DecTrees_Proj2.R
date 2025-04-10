# Load dataset
data <- read.csv("~/Downloads/Chicago_rides_october24.csv")

# Convert categorical variables to factors
data$Shared.Trip.Match <- as.factor(ifelse(data$Shared.Trip.Match == "true", "1", "0"))
data$Trip.Pooled <- as.factor(ifelse(data$Trip.Pooled == "TRUE", "1", "0"))
data$Tip.Given <- as.factor(ifelse(data$Tip.Given == "TRUE", "1", "0"))

# Convert community areas to factors
data$Pickup.Community.Area <- as.factor(data$Pickup.Community.Area)
data$Dropoff.Community.Area <- as.factor(data$Dropoff.Community.Area)

# Remove non-informative variable
data$Shared.Trip.Authorized <- NULL  

# Convert Trips.Pooled to numeric
data$Trips.Pooled <- as.numeric(as.character(data$Trips.Pooled))

# Verify structure and summary
str(data)
summary(data)








## GLM on a reduced dataset (memory issue)
set.seed(97)

sample_data <- data[sample(nrow(data), 100000), ]

library(car)
library(pROC)

sample_data_reduced <- sample_data[, c("Tip.Given", "Fare", "Trip.Miles", 
                                       "Additional.Charges", "Trip.Pooled",
                                       "Dropoff.Community.Area", "Trip.Start.Timestamp")]

sample_data_reduced$log_Fare <- log(sample_data_reduced$Fare + 1)
sample_data_reduced$log_TripMiles <- log(sample_data_reduced$Trip.Miles + 1)

sample_data_reduced$Hour <- as.integer(format(as.POSIXct(sample_data_reduced$Trip.Start.Timestamp, format="%Y-%m-%d %H:%M:%S"), "%H"))

sample_data_reduced$TimeContinuous <- as.numeric(sample_data_reduced$Hour) / 24

sample_data_reduced$Trip.Start.Timestamp <- NULL
sample_data_reduced$Hour <- NULL

significant_areas <- c(41, 50, 51, 53, 69, 71)  
sample_data_reduced <- sample_data_reduced[sample_data_reduced$Dropoff.Community.Area %in% significant_areas, ]

sample_data_reduced$Dropoff.Community.Area <- factor(sample_data_reduced$Dropoff.Community.Area)

sample_data_reduced$Tip.Given <- as.numeric(as.character(sample_data_reduced$Tip.Given))
sample_data_reduced$Trip.Pooled <- as.numeric(as.character(sample_data_reduced$Trip.Pooled))

sample_data_reduced <- na.omit(sample_data_reduced)

model_final <- glm(Tip.Given ~ log_Fare + log_TripMiles + Additional.Charges +
                     Trip.Pooled + Dropoff.Community.Area + TimeContinuous,
                   family = binomial(link = "logit"), data = sample_data_reduced)

summary(model_final)

pred_probs <- predict(model_final, newdata = sample_data_reduced, type = "response")

auc_value <- auc(sample_data_reduced$Tip.Given, pred_probs)
print(paste("AUC:", auc_value))

# CV_AUC
AUC_eval <- function(model_formula, data, folds = 5) {
  set.seed(516)
  
  fold_ids <- sample(rep(1:folds, length.out = nrow(data)))
  auc_values <- numeric(folds)
  
  for (k in 1:folds) {
    train_data <- data[fold_ids != k, ]
    test_data  <- data[fold_ids == k, ]
    
    glm_model <- glm(model_formula, family = binomial(link = "logit"), data = train_data)
    
    test_pred <- predict(glm_model, newdata = test_data, type = "response")
    
    valid_idx <- !is.na(test_pred) & !is.na(test_data$Tip.Given)
    
    auc_values[k] <- auc(test_data$Tip.Given[valid_idx], test_pred[valid_idx])
  }
  
  return(mean(auc_values))
}


cv_auc <- AUC_eval(Tip.Given ~ log_Fare + log_TripMiles + Additional.Charges +
                     Trip.Pooled + Dropoff.Community.Area + TimeContinuous, 
                   sample_data_reduced)

print(paste("Cross-Validated AUC:", cv_auc))

# Generate predicted probabilities
pred_probs <- predict(model_final, newdata = sample_data_reduced, type = "response")

# Convert probabilities into class labels (threshold = 0.5)
pred_class <- ifelse(pred_probs >= 0.5, 1, 0)

# Create the Confusion Matrix
conf_matrix <- table(Predicted = pred_class, Actual = sample_data_reduced$Tip.Given)
print(conf_matrix)

# If you want more evaluation metrics, use caret
library(caret)
conf_matrix_caret <- confusionMatrix(as.factor(pred_class), as.factor(sample_data_reduced$Tip.Given))
print(conf_matrix_caret)


library(pROC)
library(ggplot2)

# Compute the ROC curve
roc_curve <- roc(sample_data_reduced$Tip.Given, pred_probs)

# Create a dataframe for ggplot
roc_df <- data.frame(
  FPR = rev(1 - roc_curve$specificities),
  TPR = rev(roc_curve$sensitivities)
)

# Plot the ROC curve
ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "ROC Curve - Logistic Regression", x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme_minimal()















############ DECISION TREES ###################
# Load required libraries
library(rpart)         
library(rattle)        
library(rpart.plot)    
library(caret)         
library(pROC)          
library(dplyr)         
library(ggplot2)       

# Set seed for reproducibility
set.seed(97)

# Extract a manageable sample
sample_data <- data[sample(nrow(data), 100000), ]

# Select relevant variables
selected_vars <- c("Tip.Given", "Fare", "Trip.Miles", "Additional.Charges",
                   "Trip.Pooled", "Dropoff.Community.Area", "Trip.Start.Timestamp")

sample_data_reduced <- sample_data[, selected_vars]

# Rename columns to ensure compatibility
colnames(sample_data_reduced) <- make.names(colnames(sample_data_reduced))

# Log transformation for continuous variables
sample_data_reduced$log_Fare <- log(sample_data_reduced$Fare + 1)
sample_data_reduced$log_TripMiles <- log(sample_data_reduced$Trip.Miles + 1)

# Extract hour and transform into a continuous feature
sample_data_reduced$Hour <- as.integer(format(as.POSIXct(sample_data_reduced$Trip.Start.Timestamp, format="%Y-%m-%d %H:%M:%S"), "%H"))
sample_data_reduced$TimeContinuous <- sample_data_reduced$Hour / 24  # Normalize to a 0-1 scale

# Remove unnecessary columns
sample_data_reduced$Trip.Start.Timestamp <- NULL
sample_data_reduced$Hour <- NULL

# Select significant Dropoff.Community.Area values
significant_areas <- c(50, 51, 53, 69, 71)  
sample_data_reduced <- sample_data_reduced[sample_data_reduced$Dropoff.Community.Area %in% significant_areas, ]

# Convert categorical variables to factors
sample_data_reduced$Dropoff.Community.Area <- as.factor(sample_data_reduced$Dropoff.Community.Area)
sample_data_reduced$Tip.Given <- as.factor(sample_data_reduced$Tip.Given)

# Check for missing values
print(colSums(is.na(sample_data_reduced)))

# Ensure Tip.Given is binary (0 or 1) and properly formatted
if (any(is.na(sample_data_reduced$Tip.Given))) {
  sample_data_reduced$Tip.Given[is.na(sample_data_reduced$Tip.Given)] <- 0
}
sample_data_reduced$Tip.Given <- factor(sample_data_reduced$Tip.Given, levels = c(0, 1))

# Ensure numerical variables are correctly formatted
sample_data_reduced$log_Fare <- as.numeric(sample_data_reduced$log_Fare)
sample_data_reduced$log_TripMiles <- as.numeric(sample_data_reduced$log_TripMiles)
sample_data_reduced$Additional.Charges <- as.numeric(sample_data_reduced$Additional.Charges)
sample_data_reduced$TimeContinuous <- as.numeric(sample_data_reduced$TimeContinuous)

# Remove rows with missing values
sample_data_reduced <- na.omit(sample_data_reduced)

# Ensure there are at least two classes before proceeding
if (length(unique(sample_data_reduced$Tip.Given)) < 2) {
  stop("Error: Only one class present in Tip.Given. Model requires at least two classes.")
}

# Balance classes using dplyr
tip_1 <- sample_data_reduced %>% filter(Tip.Given == 1)
tip_0 <- sample_data_reduced %>% filter(Tip.Given == 0)

n_min <- min(nrow(tip_1), nrow(tip_0))
tip_0 <- tip_0 %>% sample_n(n_min, replace = FALSE)
tip_1 <- tip_1 %>% sample_n(n_min, replace = FALSE)

# Combine balanced data
sample_data_reduced <- bind_rows(tip_0, tip_1)

# Create decision tree with optimized pruning
tree_model_full <- rpart(Tip.Given ~ log_Fare + log_TripMiles + Additional.Charges +
                           Trip.Pooled + Dropoff.Community.Area + TimeContinuous,
                         data = sample_data_reduced,
                         method = "class",
                         parms = list(split = "gini"),
                         control = rpart.control(minsplit = 20, cp = 0))

# Select best cp using cost-complexity pruning
printcp(tree_model_full)
best_cp <- tree_model_full$cptable[which.min(tree_model_full$cptable[, "xerror"]), "CP"]
tree_model <- prune(tree_model_full, cp = best_cp)

# Print model summary
summary(tree_model)

# Visualize decision tree
fancyRpartPlot(tree_model, palettes = c("Reds", "Greens"))

# Feature Importance
VI <- tree_model$variable.importance
names(VI) <- c("logMiles", "logFare", "AddCharges", "Time", "CommArea", "Pooled")
barplot(VI, xlab = "Variable", ylab = "Importance", names.arg = names(VI), cex.names = 0.8, col = "blue", las = 2, cex.axis = 0.8, cex.lab = 1, cex.main = 1.2)

# Predictions on training data
pred_probs <- predict(tree_model, sample_data_reduced, type = "prob")[,2]

# Convert predictions to binary classes (threshold 0.5)
pred_class <- ifelse(pred_probs > 0.5, 1, 0)

# Compute confusion matrix
conf_matrix <- confusionMatrix(as.factor(pred_class), sample_data_reduced$Tip.Given)
print(conf_matrix)

# Compute AUC for model evaluation
auc_value <- auc(sample_data_reduced$Tip.Given, pred_probs)
print(paste("AUC:", auc_value))

# Compute the ROC curve
roc_curve <- roc(sample_data_reduced$Tip.Given, pred_probs)

# Create a dataframe for ggplot
roc_df <- data.frame(
  FPR = rev(1 - roc_curve$specificities),
  TPR = rev(roc_curve$sensitivities)
)

# Plot the ROC curve with ggplot2
ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "ROC Curve - Decision Tree", x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme_minimal()

# Cross-Validation (5-fold)
cv_tree <- train(Tip.Given ~ log_Fare + log_TripMiles + Additional.Charges +
                   Trip.Pooled + Dropoff.Community.Area + TimeContinuous,
                 data = sample_data_reduced,
                 method = "rpart",
                 trControl = trainControl(method = "cv", number = 5),
                 tuneLength = 10)

print(cv_tree)