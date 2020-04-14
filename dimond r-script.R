#load dataset Diamonds
diamond <- read.csv ("C:/Users/HP/Desktop/lab/diamonds.csv")

#checking for missing values
count_missing = function(df) {  sapply(df, FUN=function(col) sum(is.na(col)) ) }
nacounts <- count_missing(diamonds)
hasNA = which(nacounts > 0)
nacounts[hasNA]

#Visualization to Explore dataset
# visualization for the price and the log of price
ggplot(diamonds, aes(price)) + geom_histogram()
ggplot(diamonds, aes(log10(price))) + geom_histogram()

# visualization for the carat and price
ggplot(diamonds, aes(x = carat, y = price)) + geom_point() + geom_smooth()
# visualization for the carat and log10(price)
ggplot(diamonds, aes(x=carat, y=log(price))) + geom_point() + geom_smooth()
       
# visualization for the cut and log10(price)
diamonds %>%
     ggplot(aes(x=cut,y=price, color=cut)) + geom_boxplot()
# visualization for the cut and color
ggplot(diamonds,aes(x=cut, fill = color))+geom_bar()

# to arrange clarity levels
levels (diamond$clarity)
diamond$clarity = ordered(diamond$clarity,levels=c("I1","SI1","SI2","VS1","VVS1","VS2","VVS2","IF"))
levels (diamond$clarity)
summary(diamond$clarity)

#*******************************************************************
#building linear regrssion model with 50/50 split
#******************************************************************* 
 set.seed(235235)
 p <- runif(nrow(diamond))
  dtrain <- subset(diamond, p >= 0.5)
  dtest <- subset(diamond, p = 0.5)
  
  model <- lm(log10(price) ~ carat + clarity +color + cut, data = dtrain)
  dtest$predLogPrice <- predict(model, newdata = dtest)
  dtrain$predLogPrice <- predict(model, newdata = dtrain)
  
#	Plotting log price as a function of predicted log price 
  
  ggplot(data = dtest, aes(x = predLogPrice, y = log10(price))) + geom_point(alpha = 0.2, color = "red") +
    geom_smooth(color = "darkblue")  +
    geom_line(aes(x = log10(price), y = log10(price)),color = "green", linetype = 2)+ 
 coord_cartesian(xlim = c(2.5,5.5), ylim = c(1, 5))
  
#	Plotting residuals price as a function of predicted log price  
  
  ggplot(data = dtest, aes(x = predLogPrice,  y = predLogPrice - log10(price))) +
    geom_point(alpha = 0.2, color = "darkgreen") +
    geom_smooth(color = "red") +
    ylab("residual error (prediction - actual)")
  
# Computing R-squared for both dtrain and dtest
  rsq <- function(y, f) { 1 - sum((y - f)^2)/sum((y - mean(y))^2) }
  rsq(log10(dtrain$price), dtrain$predLogPrice)
  rsq(log10(dtest$price), dtest$predLogPrice) 
  
# Computing RMSE for both dtrain and dtest
  
  rmse <- function (y, f) { sqrt(mean( (y-f)^2 )) }
  rmse (log10(dtrain$price), dtrain$predLogPrice)
  rmse (log10(dtest$price), dtest$predLogPrice)
  
# Computing MAE for both dtrain and dtest
  Mean_Absolute_Error <- function(actual, predicted) {mean(abs(actual - predicted))}
  Mean_Absolute_Error(log10(dtrain$price), dtrain$predLogPrice)
  Mean_Absolute_Error(log10(dtest$price), dtest$predLogPrice) 
# 
   summary(model)
   
   coefficients(model)
#********************************************************************   
   #building linear regrssion model with 30/70 split
#********************************************************************
   set.seed(235235)
   p <- runif(nrow(diamond))
   dtrain <- subset(diamond, p >= 0.3)
   dtest <- subset(diamond, p = 0.7)
   
   model <- lm(log10(price) ~ carat + clarity +color + cut, data = dtrain)
   dtest$predLogPrice <- predict(model, newdata = dtest)
   dtrain$predLogPrice <- predict(model, newdata = dtrain)
   
   #	Plotting log price as a function of predicted log price 
   
   ggplot(data = dtest, aes(x = predLogPrice, y = log10(price))) + geom_point(alpha = 0.2, color = "lightblue") +
     geom_smooth(color = "darkblue")  +
     geom_line(aes(x = log10(price), y = log10(price)),color = "green", linetype = 2)+ 
     coord_cartesian(xlim = c(2.5,5.5), ylim = c(1, 5))
   
   #	Plotting residuals price as a function of predicted log price  
   
   ggplot(data = dtest, aes(x = predLogPrice,  y = predLogPrice - log10(price))) +
     geom_point(alpha = 0.2, color = "lightgreen") +
     geom_smooth(color = "red") +
     ylab("residual error (prediction - actual)")
   
   # Computing R-squared for both dtrain and dtest
   rsq <- function(y, f) { 1 - sum((y - f)^2)/sum((y - mean(y))^2) }
   rsq(log10(dtrain$price), dtrain$predLogPrice)
   rsq(log10(dtest$price), dtest$predLogPrice) 
   
   # Computing RMSE for both dtrain and dtest
   
   rmse <- function (y, f) { sqrt(mean( (y-f)^2 )) }
   rmse (log10(dtrain$price), dtrain$predLogPrice)
   rmse (log10(dtest$price), dtest$predLogPrice)
   
   # Computing MAE for both dtrain and dtest
   Mean_Absolute_Error <- function(actual, predicted) {mean(abs(actual - predicted))}
   Mean_Absolute_Error(log10(dtrain$price), dtrain$predLogPrice)
   Mean_Absolute_Error(log10(dtest$price), dtest$predLogPrice) 
   # 
   #
   
   coefficients(model)
   summary(model)
   
  
   
   