library(psych) #for improved scatterplot matrix
library(stats)

setwd("C:/Users/re_pr/Desktop/R_ML")

#read the dataset, include strings as factors for nominal variables.
d <- read.csv("insurance.csv", stringsAsFactors=TRUE)

#check if the dataset is loaded.
str(d)

# 'data.frame':	1338 obs. of  7 variables:
# $ age     : int  19 18 28 33 32 31 46 37 37 60 ...
# $ sex     : Factor w/ 2 levels "female","male": 1 2 2 2 2 1 1 1 2 1 ...
# $ bmi     : num  27.9 33.8 33 22.7 28.9 25.7 33.4 27.7 29.8 25.8 ...
# $ children: int  0 1 3 0 0 0 1 3 2 0 ...
# $ smoker  : Factor w/ 2 levels "no","yes": 2 1 1 1 1 1 1 1 1 1 ...
# $ region  : Factor w/ 4 levels "northeast","northwest",..: 4 3 3 2 2 3 3 2 1 2 ...
# $ expenses: num  16885 1726 4449 21984 3867 ...

# dependent variable = expenses
summary(d$expenses)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1122    4740    9382   13270   16640   63770 
# Qu. for Quarter

# histogram: bar graph for expenses.
hist(d$expenses)
# right-skewed distribution.

#check the table of region.
table(d$region)
# northeast northwest southeast southwest 
# 324       325       364       325 

#correlation matrix.
cor(d[c("age", "bmi", "children", "expenses")])
#               age        bmi   children   expenses
# age      1.0000000 0.10934101 0.04246900 0.29900819
# bmi      0.1093410 1.00000000 0.01264471 0.19857626
# children 0.0424690 0.01264471 1.00000000 0.06799823
# expenses 0.2990082 0.19857626 0.06799823 1.00000000

#scatter plot matrix for visualizing relationships among features.
pairs(d[c("age","bmi","children","expenses")])

#improved scatterplot matrix.
pairs.panels(d[c("age","bmi","children","expenses")])

#building the model
ins_model = lm(expenses~.,data=d)

ins_model
# Call:
# lm(formula = expenses ~ ., data = d)
# 
# Coefficients:
# (Intercept)            age          sexmale            bmi  
# -11941.6            256.8           -131.4            339.3  
# children        smokeryes  regionnorthwest  regionsoutheast  
# 475.7          23847.5           -352.8          -1035.6  
# regionsouthwest  
# -959.3  

#evaluate model performance.
summary(ins_model)

# Call:
#   lm(formula = expenses ~ ., data = d)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -11302.7  -2850.9   -979.6   1383.9  29981.7 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)     -11941.6      987.8 -12.089  < 2e-16 ***
#   age                256.8       11.9  21.586  < 2e-16 ***
#   sexmale           -131.3      332.9  -0.395 0.693255    
#   bmi                339.3       28.6  11.864  < 2e-16 ***
#   children           475.7      137.8   3.452 0.000574 ***
#   smokeryes        23847.5      413.1  57.723  < 2e-16 ***
#   regionnorthwest   -352.8      476.3  -0.741 0.458976    
#   regionsoutheast  -1035.6      478.7  -2.163 0.030685 *  
#   regionsouthwest   -959.3      477.9  -2.007 0.044921 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6062 on 1329 degrees of freedom
# Multiple R-squared:  0.7509,	Adjusted R-squared:  0.7494 
# F-statistic: 500.9 on 8 and 1329 DF,  p-value: < 2.2e-16

#enhance the performance of this model.
#add non-lineal relationship.
d$age2 <- d$age^2

#numeric value to binary factor.
#bmi threshold: 30
d$bmi30 <- ifelse(d$bmi>=30, 1, 0)

#adding interaction effects: bmi and smoker.
d~bmi30 + smokeryes + bmi30:smokeryes

#make a new model.
ins_model2 <- lm(expenses~.+bmi30*smoker, data=d)
ins_model2
# Call:
#   lm(formula = expenses ~ . + bmi30 * smoker, data = d)
# 
# Coefficients:
# (Intercept)         age          sexmale              bmi  
# 139.005          -32.618         -496.769          119.771  
# children        smokeryes  regionnorthwest  regionsoutheast  
# 678.602        13404.595         -279.166         -828.035  
# regionsouthwest             age2            bmi30  smokeryes:bmi30  
# -1222.162            3.731         -997.935        19810.153  

#predict with new model.
d$pred <- predict(ins_model2,d)

#check the correlation value for pred and expenses.
cor(d$pred, d$expenses)

#draw a plot.
plot(d$pred, d$expenses)
abline(a=0,b=1,col="lightgreen",lwd=3,lty=2)

#predict.
predict(ins_model2,
        data.frame(age=40, age2=40^2, children=1,
                   bmi=30, sex="male", bmi30=1,
                   smoker="no", region="northeast"))
#7580.512 

predict(ins_model2,
        data.frame(age=40, age2=40^2, children=1,
                   bmi=30, sex="female", bmi30=1,
                   smoker="no", region="northeast"))
#8077.281 female expenses are more than male.

7580.512 - 8077.281
# -496.769 : It is the same as the regression model coefficient for sexmale.



