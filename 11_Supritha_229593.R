# ICS Project - 3

# Group Members:
# 1. Ashish Saini
# 2. Dhanunjaya Elluri Thimmaraju
# 3. Harshini Eggoni
# 4. Kunal Kochar
# 5. Naveen Kumar Bhageradhi
# 6. Supritha Palguna


library(xtable)
#Version of R------
R.version.string
#Read Data
df <- read.csv("D:/A lectures/summer 2022/ICS/P3/vw_data.csv", header = TRUE)
df <- subset(df, select = -c(X))

# Check for dimensions
dim (df)

# Number of cars per each model
table(df$model)
# Number of cars per each fueltype
table(df$fuelType)
# Number of cars per each transmission type
table(df$transmission)

# Check for null values
is.null(df)


###########################################################
# Task - 1
# litre per 100kms 
df$lp100=282.48/(df$mpg)
#Removing mpg column
df$mpg <- NULL
###########################################################
# Task - 2
# Car's age
df$age = 2020 -(df$year)
# Removing year column
df$year <- NULL

#log of car price
df$logprice=log(df$price)
#Data Type
str(df)

#Descriptive Statistics
summary(df$price)
summary(df$logprice)
summary(df$mileage)
summary(df$age)
summary(df$lp100)
summary(df$tax)
summary(df$engineSize)

# Summary of each Model
table(df$model)
df_s <- data.frame()
for (i in unique(df$model)){
  
  df_s <- rbind(df_s,summary(df$price[df$model == i]))
  
}
colnames(df_s) <- c("Min","Q1","Median","Mean","Q3","Max")
df_s
#FuelType
table(df$fuelType)
df_t <- data.frame()
for (i in unique(df$fuelType)){
  
  df_t <- rbind(df_t,summary(df$price[df$fuelType == i]))
  
}
colnames(df_t) <- c("Min","Q1","Median","Mean","Q3","Max")
df_t
#Transmission
df_u <- data.frame()
for (i in unique(df$transmission)){
  
  df_u <- rbind(df_u,summary(df$price[df$transmission == i]))
  
}
colnames(df_u) <- c("Min","Q1","Median","Mean","Q3","Max")
df_u




###########################################################
#Task - 3

#using price as response variable
fit_1 = lm(price ~ model+mileage+fuelType+engineSize+tax+transmission+lp100+age, data = df)
residuals <- df$price - fit_1$fitted.values
plot(fit_1$fitted.values, residuals, main = "Residuals Plot",xlab=("Fitted values"), ylab=("Residuals"))
abline(0,0)
qqnorm(residuals, xlab=("Quantiles"), ylab=("Residuals"))
qqline(residuals)


#using log price as response variable
fit_2 = lm(logprice ~ model+mileage+fuelType+engineSize+tax+transmission+lp100+age, data = df)
residuals <- df$logprice - fit_2$fitted.values
plot(fit_2$fitted.values, residuals, main = "Residuals Plot",xlab=("Fitted values"), ylab=("Residuals"))
abline(0,0)
qqnorm(residuals, xlab=("Quantiles"), ylab=("Residuals"))
qqline(residuals)

# Checking multi-collinearity
library(car)
vif(fit_2)

##variables to be as.factor##----
df$model <- as.factor(df$model)
df$fuelType <- as.factor(df$fuelType)
df$transmission <- as.factor(df$transmission)

#removing price, mpg
df$price <- NULL


##AIC Computation##----


predictors <- as.data.frame(colnames(df[-9]))
colnames(predictors) <- "regressor"
SubSetResult <- vector()

for (predictorsCounter in 1:nrow(predictors)) {
  
  allMCombn <- combn(x = predictors$regressor, m = predictorsCounter)
  
  for (mCombnCounter in 1:ncol(allMCombn)) {
    
    modelPredictors <- allMCombn[,mCombnCounter]
    
    betaFormula <- character()
    
    for (subPredictorsCounter in 1:predictorsCounter) {
      
      betaFormula <- paste(betaFormula, modelPredictors[subPredictorsCounter], sep = "+")
      
    }
    
    formula <- paste("logprice ~",sub(".","",betaFormula))
    
    reg.lm <- lm(as.formula(formula), df)
    
    
    aic <- AIC(reg.lm)
    
    SubSetResult <- rbind(SubSetResult,c(formula,round(aic, digits = 2), predictorsCounter))
    
  }
}

SubSetResult <- as.data.frame(SubSetResult)

colnames(SubSetResult) <- c("model","aic","CountOfPredictors")
aicmin <- which.min(SubSetResult$aic)


print(SubSetResult[aicmin,])


###########################################################
# Task - 4
library(xtable)
mdl <- lm(formula =logprice ~ model+transmission+mileage+fuelType+tax+engineSize+lp100+age , data = df)
summary(mdl)
print(xtable(summary(mdl), type = "latex"), file = "sum_model1.tex")
##Residual vs Fitted value Plot----
residuals <- df$logprice - mdl$fitted.values
plot(mdl$fitted.values, residuals, main = "Residuals Plot",xlab=("Fitted values"), ylab=("Residuals"))
abline(0,0)

##QQ Plot for Normality Assumption----

qqnorm(residuals, xlab=("Quantiles"), ylab=("Residuals"))
qqline(residuals)

##ConfidenceInterval----
confint(mdl, level = 0.95)
print(xtable(confint(mdl, level = 0.95), type = "latex"), file = "sum_model2.tex")
