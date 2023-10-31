setwd("C:/Users/re_pr/Desktop/R_ML")

# Read csv file and sava it in 'launch.'
launch <- read.csv("challenger.csv")

# calculate b and a from yhat = bxhat + a.
b <- cov(launch$temperature, launch$distress_ct)/var(launch$temperature)
b #[1] -0.04753968
a <- mean(launch$distress_ct)-b*mean(launch$temperature)
a #[1] 3.698413

# Pearson's correlation coefficient(Correlation)
corr <- cov(launch$temperature, launch$distress_ct)/(sd(launch$temperature)*sd(launch$distress_ct))
corr #[1] -0.5111264

# function for correlation
cor(launch$temperature, launch$distress_ct) #[1] -0.5111264
# inversely proportional relationship.

# define multiple regression function.
reg <- function(y,x) {
  x <- as.matrix(x) #dataframe x to matrix
  x <- cbind(Intercept = 1,x) #add column named "Intercept" with value 1 left from x.
  b <- solve(t(x) %*% x) %*% t(x) %*% y #pseudo inverse of given matrices.
  colnames(b) <- "estimate" #name the column name of b.
  print(b) #print b.
}

str(launch)
# 'data.frame':	23 obs. of  4 variables:
# $ distress_ct         : int  0 1 0 0 0 0 0 0 1 1 ...
# $ temperature         : int  66 70 69 68 67 72 73 70 57 63 ...
# $ field_check_pressure: int  50 50 50 50 50 50 100 100 200 200 ...
# $ flight_num          : int  1 2 3 4 5 6 7 8 9 10 ...

#apply function 'reg'.
reg(y=launch$distress_ct, x=launch$temperature)
# estimate
# Intercept    3.69841270
# temperature -0.04753968

# we can also use launch[2] instead of launch$temperature because in function x is turned to matrix.
reg(y=launch$distress_ct, x=launch[2])
# estimate
# Intercept    3.69841270
# temperature -0.04753968

# add more variables from dataframe.
reg(y=launch$distress_ct, x=launch[2:4])
# estimate
# Intercept             3.527093383
# temperature          -0.051385940
# field_check_pressure  0.001757009
# flight_num            0.014292843

# check correlation for all variables.
cor(launch$distress_ct, launch[2:4]) 
# temperature       field_check_pressure flight_num
# [1,]  -0.5111264            0.2846663  0.1735779
# strong [0.5+] corr. btw temperature and ct. 
# weak [0.1~0.3] corr. btw pressure/num and ct.


