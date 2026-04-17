library(tidyverse)
library(rpart)
library(rpart.plot)
library(tidymodels)    


list.files()
# Importing the two data sets
forest_A <- read.csv("Forest_Fire_Data.csv")
forest_A
View(forest_A)
forest_B <- read.csv("Algers_Forest.csv")
forest_B
View(forest_B)

# Grouping the data sets together
forest <- full_join(forest_A, forest_B)
forest
View(forest)
str(forest)


# Performing Data cleaning
# checking for missing values

sum(is.na(forest))


# checking for duplicates
sum(duplicated(forest))
glimpse(forest)
str(forest)

class(forest$FFMC)
class(forest$outcome)

# Exploratory Data Analysis

# Load necessary library
library(dplyr)

# Select relevant columns and recode the 'Classes' column
forest_profile <- forest %>%
  select(month, FFMC, DMC, DC, ISI, Temperature, RH, wind, rain, outcome) %>%
  mutate(outcome_text = recode(outcome, 
                          `0` = "No Fire", `1` = "Fire")
)



# Factorize the selected columns
#forest_profile <- forest_profile %>%
#  mutate(
 #   month = factor(month),
  #  FFMC = factor(FFMC),  
   # DMC  = factor(DMC),      
    #DC   = factor(DC),
    #ISI  = factor(ISI),
    #Temperature = factor(Temperature),
    #RH   = factor(RH),
    #wind = factor(wind),
    #rain = factor(rain),
    #outcome = factor(outcome)
#)




                                # Feature Engineering
set.seed(250)

# Data Partitioning into training and testing
# Nb Mostly training data takes 80% or 75% of your data
                                

                                  # DATA PREPROCESSING
forest_data <- initial_split(forest_profile, prop = 0.75,
                      strata = outcome)

# creating the testing and training data set
# train data set
forest_train <- forest_data%>%
  training()

nrow(forest_train)

# test data set
forest_test <- forest_data %>%
  testing()
nrow(forest_test)


forest_train <- forest_train %>%
  mutate(outcome = as.factor(outcome))

forest_test <- forest_test %>%
  mutate(outcome = as.factor(outcome))

                               
                            # First model (Logistics Model)
Logistic_Model <- logistic_reg() %>%
  set_engine('glm')%>%
  set_mode('classification')


                                # Model Fitting
log_fit <- Logistic_Model %>%
  fit(outcome ~.,
      data = forest_train)

# Install if needed
install.packages("logistf")

# Use Firth's method which handles separation
# Load logistf
library(logistf)

# Fit Firth's logistic regression directly
log_fit_firth <- logistf(outcome ~ month + FFMC + DMC + DC + ISI + Temperature + RH + wind + rain, 
                         data = forest_train)



# View results
summary(log_fit_firth)

# Make predictions on test set
pred_probs <- predict(log_fit_firth, newdata = forest_test, type = "response")
pred_classes <- ifelse(pred_probs > 0.5, "1", "0")

# Convert to factor for evaluation
pred_classes <- factor(pred_classes, levels = c("0", "1"))


# Evaluate
table(Predicted = pred_classes, Actual = forest_test$outcome)

# Calculate accuracy
mean(pred_classes == forest_test$outcome)



# 1. ROC Curve
# Install pROC for ROC curve analysis
install.packages("pROC")

# Load the package
library(pROC)
roc_curve <- roc(forest_test$outcome, pred_probs)
plot(roc_curve, 
     main = "ROC Curve - Firth's Logistic Regression",
     col = "blue", 
     lwd = 2,
     print.auc = TRUE,
     auc.polygon = TRUE,
     grid = TRUE,
     legacy.axes = TRUE)

auc(roc_curve)  # Area Under Curve

# 2. Confusion Matrix Visualization
library(corrplot)
conf_matrix <- matrix(c(71, 90, 36, 171), nrow = 2, byrow = TRUE)
rownames(conf_matrix) <- c("Predicted No Fire", "Predicted Fire")
colnames(conf_matrix) <- c("Actual No Fire", "Actual Fire")
conf_matrix

# 3. Colorful confusion matrix
install.packages("caret")
library(caret)
confusionMatrix(pred_classes, forest_test$outcome)





                                            # model Two
                                      # Decision tree model
# building a regression tree
# Second Model (Decision Tree)
library(rpart)
library(rpart.plot)

# Fit the decision tree
tree_model <- rpart(outcome ~ month + FFMC + DMC + DC + ISI + Temperature + RH + wind + rain,
                    data = forest_train,
                    method = "class")

# View results
print(tree_model)
summary(tree_model)

# Plot the decision tree
rpart.plot(tree_model, main = "Decision Tree for Forest Fire Prediction")


# Make predictions on test set
tree_pred <- predict(tree_model, newdata = forest_test, type = "class")
tree_pred <- factor(tree_pred, levels = c("0", "1"))

# Evaluate
table(Predicted = tree_pred, Actual = forest_test$outcome)

# Calculate accuracy
mean(tree_pred == forest_test$outcome)

# ROC Curve for Decision Tree
library(pROC)

# Get probabilities
tree_probs <- predict(tree_model, newdata = forest_test, type = "prob")[, "1"]

# Plot ROC curve
roc_tree <- roc(forest_test$outcome, tree_probs)
plot(roc_tree, 
     main = "ROC Curve - Decision Tree",
     col = "green", 
     lwd = 2,
     print.auc = TRUE,
     auc.polygon = TRUE,
     grid = TRUE,
     legacy.axes = TRUE)

auc(roc_tree)

# Confusion Matrix with proper labels
conf_matrix_tree <- table(Predicted = tree_pred, Actual = forest_test$outcome)
rownames(conf_matrix_tree) <- c("Predicted No Fire", "Predicted Fire")
colnames(conf_matrix_tree) <- c("Actual No Fire", "Actual Fire")
print(conf_matrix_tree)


                                          # MODEL THREE
                                        # Random Forest
install.packages("randomForest")
library(randomForest)

# Fit Random Forest
set.seed(123)  # for reproducibility
rf_model <- randomForest(outcome ~ month + FFMC + DMC + DC + ISI + Temperature + RH + wind + rain,
                         data = forest_train,
                         ntree = 500,        # number of trees
                         mtry = 3,           # number of variables tried at each split
                         importance = TRUE)  # calculate variable importance

# View results
print(rf_model)
summary(rf_model)

# Variable importance
importance(rf_model)
varImpPlot(rf_model, main = "Random Forest - Variable Importance")

# Make predictions on test set
rf_pred <- predict(rf_model, newdata = forest_test)
rf_pred <- factor(rf_pred, levels = c("0", "1"))

# Evaluate
table(Predicted = rf_pred, Actual = forest_test$outcome)

# Calculate accuracy
mean(rf_pred == forest_test$outcome)

# ROC Curve for Random Forest
rf_probs <- predict(rf_model, newdata = forest_test, type = "prob")[, "1"]
roc_rf <- roc(forest_test$outcome, rf_probs)
plot(roc_rf, 
     main = "ROC Curve - Random Forest",
     col = "purple", 
     lwd = 2,
     print.auc = TRUE,
     auc.polygon = TRUE,
     grid = TRUE,
     legacy.axes = TRUE)

auc(roc_rf)

# Confusion Matrix with labels
conf_matrix_rf <- table(Predicted = rf_pred, Actual = forest_test$outcome)
rownames(conf_matrix_rf) <- c("Predicted No Fire", "Predicted Fire")
colnames(conf_matrix_rf) <- c("Actual No Fire", "Actual Fire")
print(conf_matrix_rf)







                          # comparison of the models
# Updated comparison with correct Decision Tree AUC
final_comparison <- data.frame(
  Model = c("Logistic Regression (Firth)", "Decision Tree", "Random Forest"),
  Accuracy = c(0.6576, 0.6386, 0.6277),
  AUC = c(0.696, 0.7012, 0.6703),
  Precision = c(0.655, 179/(179+105), 144/(144+74)),
  Recall = c(0.826, 179/(179+28), 144/(144+63)),
  Specificity = c(0.441, 56/(56+105), 87/(87+74))
)

# Calculate Decision Tree metrics
dt_precision <- 179 / (179 + 105)  # 179/284 = 0.6303
dt_recall <- 179 / (179 + 28)      # 179/207 = 0.8647
dt_specificity <- 56 / (56 + 105)  # 56/161 = 0.3478

final_comparison[2, 4:6] <- c(round(dt_precision, 4), round(dt_recall, 4), round(dt_specificity, 4))

print(final_comparison)





summary_table <- data.frame(
  Metric = c("Accuracy", "AUC", "Precision", "Recall", "Specificity"),
  Logistic_Regression = c(0.6576, 0.6960, 0.6552, 0.8261, 0.4410),
  Decision_Tree = c(0.6386, 0.7012, 0.6303, 0.8647, 0.3478),
  Random_Forest = c(0.6277, 0.6703, 0.6606, 0.6957, 0.5404)
)

print(summary_table)

                                # Best model for each metric
cat("\n\nBEST MODEL BY METRIC:\n")
cat("=====================\n")
cat(paste("Accuracy:   ", names(which.max(summary_table[1,2:4])), "\n"))
cat(paste("AUC:        ", names(which.max(summary_table[2,2:4])), "\n"))
cat(paste("Precision:  ", names(which.max(summary_table[3,2:4])), "\n"))
cat(paste("Recall:     ", names(which.max(summary_table[4,2:4])), "\n"))
cat(paste("Specificity:", names(which.max(summary_table[5,2:4])), "\n"))




results <- data.frame(
Model = c("Logistic", "Decision Tree", "Random Forest"),
Accuracy = c(0.6576, 0.6386, 0.6277),
AUC = c(0.696, 0.701, 0.670)
)
print(results)
  
  
  
cat("\n========================================\n")
cat("FINAL MODEL COMPARISON SUMMARY\n")
cat("========================================\n\n")
  
cat("DECISION TREE (BEST OVERALL):\n")
cat("  - Highest AUC: 0.7012\n")
cat("  - Highest recall: 86.5% (best at catching fires)\n")
cat("  - Good balance of accuracy and detection\n\n")
  
cat("LOGISTIC REGRESSION (2nd BEST):\n")
cat("  - Highest accuracy: 65.8%\n")
cat("  - Highest specificity: 44.1% (fewest false alarms)\n")
cat("  - Very close AUC to Decision Tree (0.696 vs 0.701)\n\n")
  
cat("RANDOM FOREST (Most Conservative):\n")
cat("  - Highest precision: 66.1% (when it predicts fire, it's often right)\n")
cat("  - Highest specificity: 54.0% (best at identifying no-fire)\n")
cat("  - Most balanced across metrics\n")
cat("  - But lowest recall: only catches 69.6% of fires\n\n")
  
cat("RECOMMENDATIONS:\n")
cat("  → Use DECISION TREE for maximum fire detection (high recall)\n")
cat("  → Use LOGISTIC REGRESSION for overall balanced performance\n")
cat("  → Use RANDOM FOREST when false alarms are very costly\n")




                    # Plot all three ROC curves with corrected Decision Tree
plot(roc_curve, col = "blue", lwd = 2, 
     main = "Model Comparison - ROC Curves",
     legacy.axes = TRUE,
     print.auc = FALSE)
plot(roc_tree, col = "green", lwd = 2, add = TRUE)
plot(roc_rf, col = "purple", lwd = 2, add = TRUE)



                           # Save to file
png("ROC_Comparison.png", width = 800, height = 600)
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curves - Three Models", legacy.axes = TRUE)
plot(roc_tree, col = "green", lwd = 2, add = TRUE)
plot(roc_rf, col = "purple", lwd = 2, add = TRUE)
legend("bottomright", legend = c("Logistic", "Decision Tree", "Random Forest"), 
       col = c("blue", "green", "purple"), lwd = 2)
dev.off()

# Check your working directory for the saved file
getwd()

