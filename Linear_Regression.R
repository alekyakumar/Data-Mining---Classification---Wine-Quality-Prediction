############Linear Model for Automatic Subset Selection#############
red_wine_exh <- lm(quality ~ volatile.acidity + citric.acid + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data = red_wine_train)

prediction_test_red_wine <- predict.lm(red_wine_exh,red_wine_test)
summary(prediction_test_red_wine)
error_data_exh <- mean((prediction_test_red_wine - y.test)^2) #It gives an error of 35%
vif(red_wine_exh) #No variable has VIF factor ablove 10. 
