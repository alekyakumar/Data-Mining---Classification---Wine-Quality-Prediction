install.packages("ISLR")
install.packages("e1071")
library(e1071)

med <- median(new_red_wine$quality)
med
red_quality <- ifelse(new_red_wine$quality <= med, "NO" , "YES")
cbind(new_red_wine$quality,red_quality)

# create a new data set
svm_red_wine <- data.frame(new_red_wine[,-c(12)], red_quality)
set.seed(1907)

split <- sample(1:nrow(svm_red_wine), 0.33*nrow(svm_red_wine))
test_svm <- svm_red_wine[split,]
train_svm <- svm_red_wine[-split,]

##############################################################################
##Fitting SVM with features selected from Manual Selection method
##############################################################################

#######Fitting SVM with Linear Kernel########
tune_model_svm_man <- tune(svm, red_quality ~ volatile.acidity + citric.acid + density + pH + sulphates + alcohol,data = train_svm, kernel = "linear",
                       ranges = list(cost = c(0.01,0.1, 1, 5, 10, 100)))
summary(tune_model_svm_man)

best_model_man <- tune_model_svm_man$best.model
best_model_man

#predict the test data
y_hat <- predict(best_model_man, newdata = test_svm)
y_true <- test_svm$red_quality

accruacy_svm_kernel_man <- length(which(y_true == y_hat))/length(y_true)
accruacy_svm_kernel_man #87.71%


#########Fitting SVM with Radial Kernel#######
tune_model_svm_radial_man <- tune(svm,red_quality ~ volatile.acidity + citric.acid + density + pH + sulphates + alcohol,data = train_svm, kernel = "radial", 
                              ranges = list(cost = c(0.01, 0.1,1,5,10,100)))
tune_model_svm_radial_man
summary(tune_model_svm_radial_man)

best_model_radial_man <- tune_model_svm_radial_man$best.model
best_model_radial_man

#predict the test data
y_hat_rad <- predict(best_model_radial_man, newdata = test_svm)
accuracy_svm_rad_man <- length(which(y_hat_rad == y_true))/length(y_true)
accuracy_svm_rad_man #89.22%

table(predict = y_hat_rad, truth = y_true)
##############################################################################
##Fitting SVM with features selected from Automatic Selection method
##############################################################################

#######Fitting SVM with Linear Kernel########
tune_model_svm_auto <- tune(svm, red_quality ~ volatile.acidity + citric.acid + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol,data = train_svm, kernel = "linear",
                       ranges = list(cost = c(0.01,0.1, 1, 5, 10, 100)))
summary(tune_model_svm_auto)

best_model_auto <- tune_model_svm_auto$best.model
best_model_auto

#predict the test data
y_hat <- predict(best_model_auto, newdata = test_svm)
y_true <- test_svm$red_quality

accruacy_svm_kernel_auto <- length(which(y_true == y_hat))/length(y_true)
accruacy_svm_kernel_auto #87.71%


#########Fitting SVM with Radial Kernel#######
tune_model_svm_radial_auto <- tune(svm,red_quality ~.,data = train_svm, kernel = "radial", 
                              ranges = list(cost = c(0.01, 0.1,1,5,10,100)))
tune_model_svm_radial_auto
summary(tune_model_svm_radial_auto)

best_model_radial_auto <- tune_model_svm_radial_auto$best.model
best_model_radial_auto

#predict the test data
y_hat_rad <- predict(best_model_radial_auto, newdata = test_svm)
accuracy_svm_rad_auto <- length(which(y_hat_rad == y_true))/length(y_true)
accuracy_svm_rad_auto #90.72%
table(predict = y_hat_rad, truth = y_true)
##############################################################################
##Fitting SVM with all features
##############################################################################

#######Fitting SVM with Linear Kernel########
tune_model_svm_all <- tune(svm, red_quality ~ .,data = train_svm, kernel = "linear",
                       ranges = list(cost = c(0.01,0.1, 1, 5, 10, 100)))
summary(tune_model_svm_all)

best_model_all <- tune_model_svm_all$best.model
best_model_all

#predict the test data
y_hat <- predict(best_model_all, newdata = test_svm)
y_true <- test_svm$red_quality

accruacy_svm_kernel_all <- length(which(y_true == y_hat))/length(y_true)
accruacy_svm_kernel_all #87.71%


#########Fitting SVM with Radial Kernel#######
tune_model_svm_radial_all <- tune(svm,red_quality ~.,data = train_svm, kernel = "radial", 
                              ranges = list(cost = c(0.01, 0.1,1,5,10,100)))
tune_model_svm_radial_all
summary(tune_model_svm_radial_all)

best_model_radial_all <- tune_model_svm_radial_all$best.model
best_model_radial_all

#predict the test data
y_hat_rad <- predict(best_model_radial_all, newdata = test_svm)
accuracy_svm_rad_all <- length(which(y_hat_rad == y_true))/length(y_true)
accuracy_svm_rad_all #91.4%

list_error = c(accruacy_svm_kernel_all,accruacy_svm_kernel_auto,accruacy_svm_kernel_man,accuracy_svm_rad_man,accuracy_svm_rad_auto,accuracy_svm_rad_all)
SVM_models = c("SVM with Linear(ALL)","SVM with Linear(auto)","SVM with Linear(man)","SVM with Radial(Manual)","SVM with Radial(Auto)","SVM with Radial(ALL)")
list_error = c(accruacy_svm_kernel_all,accruacy_svm_kernel_auto,accruacy_svm_kernel_man,accuracy_svm_rad_auto,accuracy_svm_rad_man,accuracy_svm_rad_all)
SVM_ERROR_TB = data.frame(SVM_models,list_error)
x11()
lines(SVM_ERROR_TB$SVM_models,SVM_ERROR_TB$list_error,main = 'Accuracy rates for SVM', xlab = "Models", ylab = "Accuracy")
abline()

table(predict = y_hat_rad, truth = y_true)
