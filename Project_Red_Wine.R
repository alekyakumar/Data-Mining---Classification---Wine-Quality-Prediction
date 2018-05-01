setwd("/Users/Alekya Kumar/Desktop/Data Sciences - Sem 1/Stats/Project")

install.packages("ISLR")
install.packages("e1071")
library(e1071)
install.packages("leaps")
library(leaps)
library(caret)
install.packages("car")
library(car)
library(ggplot2)
library(rpart)
library(randomForest)
library(geneplotter)
library(gbm)

red_wine<- read.delim("C:\\Users\\Alekya Kumar\\Desktop\\Data Sciences - Sem 1\\Stats\\Project\\winequality-red.csv" , sep = ";",header = TRUE)
#red_wine <- data.frame(red_wine)
summary(red_wine)

#Univariate Plot
x11()
hist(red_wine$quality, col = "green",xlim=c(2,8),las=1, breaks=5)
hist(red_wine$alcohol, col = "blue")

install.packages("ggplot2")
library(ggplot2)
pairs(quality ~ volatile.acidity , data = red_wine)
pairs(quality ~ fixed.acidity, data = red_wine)
pairs(quality ~ citric.acid, data = red_wine)
pairs(quality ~ residual.sugar, data = red_wine)
pairs(quality ~ chlorides, data = red_wine)
pairs(quality ~ free.sulfur.dioxide, data = red_wine)
pairs(quality ~ total.sulfur.dioxide, data = red_wine)
pairs(quality ~ density, data = red_wine)
pairs(quality ~ pH, data = red_wine)
pairs(quality ~ sulphates, data = red_wine)
pairs(quality ~ alcohol, data = red_wine)

?boxplot
summary(new_red_wine)
x11()
new_red_wine$quality <- factor(new_red_wine$quality, ordered = TRUE)
ggplot(data = new_red_wine, aes(x =quality, y=fixed.acidity)) + geom_boxplot(aes(color = quality))
ggplot(data = new_red_wine, aes(x =quality, y=volatile.acidity)) + geom_boxplot(aes(color = quality))
ggplot(data = new_red_wine, aes(x =quality, y=citric.acid)) + geom_boxplot(aes(fill = quality))
ggplot(data = new_red_wine, aes(x =quality, y=residual.sugar)) + geom_boxplot(aes(fill = quality))
ggplot(data = new_red_wine, aes(x =quality, y=chlorides)) + geom_boxplot(aes(color = quality))
ggplot(data = new_red_wine, aes(x =quality, y=free.sulfur.dioxide)) + geom_boxplot(aes(color = quality))
ggplot(data = new_red_wine, aes(x =quality, y=total.sulfur.dioxide)) + geom_boxplot(aes(fill = quality))
ggplot(data = new_red_wine, aes(x =quality, y=density)) + geom_boxplot(aes(fill = quality))
ggplot(data = new_red_wine, aes(x =quality, y=pH)) + geom_boxplot(aes(color = quality))
ggplot(data = new_red_wine, aes(x =quality, y=alcohol)) + geom_boxplot(aes(color = quality))
ggplot(data = new_red_wine, aes(x =quality, y=sulphates)) + geom_boxplot(aes(color = quality))

x11()
ggplot(data = new_red_wine, aes(x =fixed.acidity, y=volatile.acidity)) + geom_point(aes(color = quality))+ scale_colour_brewer(type="seq", palette=10) + scale_colour_brewer(type="seq", palette=6)
ggplot(data = new_red_wine, aes(x =residual.sugar, y=sulphates)) + geom_point(aes(color = quality))+ scale_colour_brewer(type="seq", palette=7)
ggplot(data = new_red_wine, aes(x =free.sulfur.dioxide, y=total.sulfur.dioxide)) + geom_point(aes(color = quality)) + scale_colour_brewer(type="seq", palette=3)
ggplot(data = new_red_wine, aes(x =pH, y=alcohol)) + geom_point(aes(color = quality)) + scale_colour_brewer(type="seq", palette=10)
ggplot(data = new_red_wine, aes(x =pH, y=density)) + geom_point(aes(color = quality)) + scale_colour_brewer(type="seq", palette=1)
ggplot(data = new_red_wine, aes(x =alcohol, y=density)) + geom_point(aes(color = quality)) + scale_colour_brewer(type="seq", palette=4)

###########BoxPlot for Outlier Removal#############
x11()
par(mfrow = c(3,4))
boxplot(as.numeric(red_wine$fixed.acidity), horizontal = FALSE, col="slategray2", pch=19)
mtext("Fixed Acidity", cex=0.8, side=1, line=2)
boxplot(as.numeric(red_wine$volatile.acidity), horizontal = FALSE, col="slategray2", pch=19)
mtext("volatile.acidity", cex=0.8, side=1, line=2)
boxplot(as.numeric(red_wine$citric.acid), horizontal = FALSE, col="slategray2", pch=19)
mtext("citric.acid", cex=0.8, side=1, line=2)
boxplot(as.numeric(red_wine$residual.sugar), horizontal = FALSE, col="slategray2", pch=19)
mtext("residual.sugar", cex=0.8, side=1, line=2)
boxplot(as.numeric(red_wine$chlorides), horizontal = FALSE, col="slategray2", pch=19)
mtext("chlorides", cex=0.8, side=1, line=2)
boxplot(as.numeric(red_wine$free.sulfur.dioxide), horizontal = FALSE, col="slategray2", pch=19)
mtext("free.sulfur.dioxide", cex=0.8, side=1, line=2)
boxplot(as.numeric(red_wine$total.sulfur.dioxide), horizontal = FALSE, col="slategray2", pch=19)
mtext("total.sulfur.dioxide", cex=0.8, side=1, line=2)
boxplot(as.numeric(red_wine$density), horizontal = FALSE, col="slategray2", pch=19)
mtext("density", cex=0.8, side=1, line=2)
boxplot(as.numeric(red_wine$pH), horizontal = FALSE, col="slategray2", pch=19)
mtext("pH", cex=0.8, side=1, line=2)
boxplot(as.numeric(red_wine$sulphates), horizontal = FALSE, col="slategray2", pch=19)
mtext("sulphates", cex=0.8, side=1, line=2)
boxplot(as.numeric(red_wine$alcohol), horizontal = FALSE, col="slategray2", pch=19)
mtext("alcohol", cex=0.8, side=1, line=2)


###########Outlier removal###############
outliers <- rep(0,11)

for (i in 1:11){
  t1 <- quantile(red_wine[,i], 0.75)
  t2 <- IQR(red_wine[,i], 0.75)
  outliers[i] <- t1 + 1.5*t2
}
red_wine_index <- matrix(0, 1599, 11)
for (i in 1:1599)
  for (j in 1:11){
    if (red_wine[i,j] > outliers[j]) red_wine_index[i,j] <- 1
  }
r_index <- apply(red_wine_index, 1, sum)
red_wine_data <- cbind(r_index, red_wine)
index <- rep(0)

j <- 1
for (i in 1:1599){
  if (r_index[i] > 0) {index[j]<- i
  j <- j + 1}
  else j <- j
}

new_red_wine <-red_wine[-index,]

#############Box Plot After Outlier Removal#############


x11()
par(mfrow = c(3,4))
boxplot(as.numeric(new_red_wine$fixed.acidity), horizontal = FALSE, col="slategray2", pch=19)
mtext("Fixed Acidity", cex=0.8, side=1, line=2)
boxplot(as.numeric(new_red_wine$volatile.acidity), horizontal = FALSE, col="slategray2", pch=19)
mtext("volatile.acidity", cex=0.8, side=1, line=2)
boxplot(as.numeric(new_red_wine$citric.acid), horizontal = FALSE, col="slategray2", pch=19)
mtext("citric.acid", cex=0.8, side=1, line=2)
boxplot(as.numeric(new_red_wine$residual.sugar), horizontal = FALSE, col="slategray2", pch=19)
mtext("residual.sugar", cex=0.8, side=1, line=2)
boxplot(as.numeric(new_red_wine$chlorides), horizontal = FALSE, col="slategray2", pch=19)
mtext("chlorides", cex=0.8, side=1, line=2)
boxplot(as.numeric(new_red_wine$free.sulfur.dioxide), horizontal = FALSE, col="slategray2", pch=19)
mtext("free.sulfur.dioxide", cex=0.8, side=1, line=2)
boxplot(as.numeric(new_red_wine$total.sulfur.dioxide), horizontal = FALSE, col="slategray2", pch=19)
mtext("total.sulfur.dioxide", cex=0.8, side=1, line=2)
boxplot(as.numeric(new_red_wine$density), horizontal = FALSE, col="slategray2", pch=19)
mtext("density", cex=0.8, side=1, line=2)
boxplot(as.numeric(new_red_wine$pH), horizontal = FALSE, col="slategray2", pch=19)
mtext("pH", cex=0.8, side=1, line=2)
boxplot(as.numeric(new_red_wine$sulphates), horizontal = FALSE, col="slategray2", pch=19)
mtext("sulphates", cex=0.8, side=1, line=2)
boxplot(as.numeric(new_red_wine$alcohol), horizontal = FALSE, col="slategray2", pch=19)
mtext("alcohol", cex=0.8, side=1, line=2)


#############Manual Subset Selection###################
library(corrplot)

cor(new_red_wine)
x11()
corrplot(cor(new_red_wine), method = "number")

#According to Correlation plot, we are selecting the predictors manually for Linear Regression Prediction
red_wine_lm <- lm(quality ~   volatile.acidity + citric.acid + density + pH + sulphates + alcohol, data=new_red_wine)
plot(red_wine_lm)
red_wine_lm
vif(red_wine_lm) # check collinearity - No variable is above 10 . Not correlated 

## Fitting Model using predict and calculating errors 
red_wine_pred <- predict(red_wine_lm, data = new_red_wine)
y_true <- new_red_wine$quality
error_red_wine <- (sum(abs(y_true - red_wine_pred)))/length(y_true) # 47% error rate - not a good predictor




###########Creating Training and Test Set################
train <- sample(1:nrow(new_red_wine), .70*nrow(new_red_wine))
red_wine_train <- new_red_wine[train,]
red_wine_test <- new_red_wine[-train,]
y.train <- red_wine_train$quality
y.test <- red_wine_test$quality



###########Automatic Subset Selection#############
regfit.exh <- regsubsets(quality ~., data = new_red_wine, nvmax = 12, method = "exhaustive")
exh.sum <- summary(regfit.exh)
exh.sum
plot(exh.sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(exh.sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
plot(exh.sum$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
which(exh.sum$cp == min(exh.sum$cp))
which(exh.sum$bic == min(exh.sum$bic))

coef(regfit.exh, 8)
coef(regfit.exh, 11)

##########Bootstrap method to find the best variable model ##############
install.packages("bootstrap")
library(bootstrap)
library(boot)  
library(leaps)

fit <- regsubsets(quality~., data = new_red_wine, method = 
                    "exhaustive", nvmax = 11)

beta.fit <- function(X,Y){
  lsfit(X,Y)	
}

beta.predict <- function(fit, X){
  cbind(1,X)%*%fit$coef
}

sq.error <- function(Y,Yhat){
  (Y-Yhat)^2
}

# Create X and Y
X <- new_red_wine[,1:11]
Y <- new_red_wine[,12]


# Generalize it, and search over the best possible subsets of size "k"
select = summary(fit)$outmat
error_store <- c()
for (i in 1:11){
  # Pull out the model
  temp <- which(select[i,] == "*")
  
  res <- bootpred(X[,temp], Y, nboot = 50, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error) 
  error_store <- c(error_store, res[[3]])
  
}
error_store # According to the bootstrap ethod, 8 variable model is the best



############Linear Model for Automatic Subset Selection#############
red_wine_exh <- lm(quality ~ volatile.acidity + citric.acid + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data = red_wine_train)

prediction_test_red_wine <- predict.lm(red_wine_exh,red_wine_test)
summary(prediction_test_red_wine)
error_data_exh <- mean((prediction_test_red_wine - y.test)^2) #It gives an error of 35%
vif(red_wine_exh) #No variable has VIF factor above 10. 

#######################SVM For Classification####################

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

#####################Decision Trees####################

High <- ifelse(new_red_wine$quality<=6,'No','Yes')
new_data<-subset(new_red_wine,select=-quality)
new_data<-cbind(new_data,High)

set.seed(12345)
test_indis <-sample(1:nrow(new_data),.20*nrow(new_data))
test<-new_data[test_indis,]
training<-new_data[-test_indis,]
y_true<-as.numeric(test$High)-1 #0,No 1,yes(high)

######################################
#Grow a Single Tree
#####################################
model.control<-rpart.control(minsplit=5,xval=10,cp=0)
fit<-rpart(High ~ . ,data=training,method="class",control=model.control)

X11()
plot(fit,uniform=T,compress=T)
text(fit,use.n=TRUE,cex=.5)

#prune the tree back
min_cp=which.min(fit$cptable[,4])

X11()
plot(fit$cptable[,4],main='CP for model selection',ylab='cv error')

pruned_fit<-prune(fit,cp=fit$cptable[min_cp,1])
x11()
plot(pruned_fit)
text(pruned_fit,use.n=TRUE,cex=0.5)

#Compute the error for a single tree
my_pred <- predict(pruned_fit,newdata=test,type='class')
y_hat<-as.numeric(my_pred)-1
misclass_tree<-sum(abs(y_true-y_hat))/length(y_hat)
misclass_tree

#################################
#Random Forest
#################################
rf.fit<-randomForest(High~.,data=training,n.tree=10000)
x11()
varImpPlot(rf.fit)
importance(rf.fit)

y_hat<-predict(rf.fit,newdata=test,type='response')
y_hat<-as.numeric(y_hat)-1
misclass_rf<-sum(abs(y_true-y_hat))/length(y_hat)
misclass_rf

####################################
Bagging
####################################
bag.fit<-randomForest(High~.,data=training,n.tree=10000,mtry=11)
x11()
varImpPlot(bag.fit)
importance(bag.fit)

y_hat<-predict(bag.fit,newdata=test,type='response')
y_hat<-as.numeric(y_hat)-1
misclass_bag<-sum(abs(y_true-y_hat))/length(y_hat)
misclass_bag

########################################
Boosting
########################################
boost.train<-training
boost.train$High<-as.numeric(training$High)-1
boost.test<-test
boost.test$High<-as.numeric(test$High)-1

boost.fit<-gbm(High~.,data=boost.train,n.trees=1000,shrinkage=.1,
               interaction.depth=3,distribution='adaboost')
boost.fit2<-gbm(High~.,data=boost.train,n.trees=1000,shrinkage=.6,
                interaction.depth=3,distribution='adaboost')

summary(boost.fit)

#For shrinkage=.1
y_hat<-predict(boost.fit,newdata=boost.test,n.trees=1000,type='response')
misclass_boost.1<-sum(abs(y_hat-y_true))/length(y_true)
misclass_boost.1

#For shrinkage=.6

y_hat<-predict(boost.fit2,newdata=boost.test,n.trees=1000,type='response')
misclass_boost.6<-sum(abs(y_hat-y_true))/length(y_true)
misclass_boost.6

#For different shrinkages

shrink<-c(.1,.4,.6,.8)
max_iter<-1000
store_error<-c()
for (i in 1:length(shrink)){
  boost.fit<-gbm(High~.,data=boost.train,n.trees=max_iter,shrinkage=shrink[i],
                 interaction.depth=3,distribution='adaboost')
  temp<-c()
  for(j in 1:max_iter){
    y_hat<-predict(boost.fit,newdata=boost.test,n.trees=j,type='response')
    misclass_boost<-sum(abs(y_true-y_hat))/length(y_hat)
    temp<-c(temp,misclass_boost)
  }
  store_error<-cbind(store_error,temp) # max_iter*length(shrink)
}

colnames(store_error)<-paste('shrinkage',shrink,sep=':')
X11()
plot(store_error[,1],type='l',main='Error_Profiles',ylab='error',xlab='boosting')
lines(store_error[,2],col='red')
lines(store_error[,3],col='blue')
lines(store_error[,4],col='green')