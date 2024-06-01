set.seed(123)

library(glmnet)
library(MASS)
library(dplyr)
library(pscl)
library(ResourceSelection)
library(pROC)

alpha_seq <- seq(0, 1, by = 0.05)

cv_results_list <- lapply(alpha_seq, function(alpha_value) {
  cv.glmnet(xtrain, ytrain, alpha = alpha_value, family = "binomial")
})

cv_means <- sapply(cv_results_list, function(cv_result) min(cv_result$cvm))
optimal_alpha <- alpha_seq[which.min(cv_means)]

optimal_cv_result <- cv_results_list[[which.min(cv_means)]]
lambda_min <- optimal_cv_result$lambda.min

logistic_model_1 <- glmnet(xtrain, ytrain, alpha = optimal_alpha, lambda = lambda_min, family = "binomial")

y_pred_l1 <- predict(logistic_model_1, newx = xtest, type = "response")

predicted_classes <- ifelse(y_pred_l1 > 0.5, 1, 0)
accuracy_l1 <- mean(predicted_classes == ytest)
print(paste("Accuracy:", accuracy_l1))

roc_l1 <- roc(ytest, y_pred_l1)
AUC_l1 <- auc(roc_l1)
print(paste("AUC:", AUC_l1))

print(paste("Optimal alpha:", optimal_alpha))
print(paste("Optimal lambda:", lambda_min))

coefficients <- coef(logistic_model)
print("Coefficients:")
print(coefficients)





MSE_l1 = mean((y_pred_l1 - test$Rent_Mortgage_Hold)^2)
print(MSE_l1)






