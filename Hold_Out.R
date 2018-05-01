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

