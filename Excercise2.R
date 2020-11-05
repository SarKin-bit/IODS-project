learning2014<- read.delim("learning2014.txt")

dim(learning2014)
str(learning2014)
#There are 166 observations (rows) and 7 variables (columns). Columns are called gender, age,
#attitude, deep, stra, surf and points. All columns include data in numerical form, except column 
#"gender", which includes only characters.

#graphical overview of the data:
library(ggplot2)
# initialize plot with data and aesthetic mapping
p1 <- ggplot(learning2014, aes(x = Attitude, y = Points, col = gender))

# define the visualization type (points)
p2 <- p1 + geom_point()
# draw the plot
p2

# add a regression line
p3 <- p2 + geom_smooth(method = "lm")
p3

#summaries of the variables in the data
summary(learning2014)

#2.	Show a graphical overview of the data and show summaries of the variables in the data. 
#Describe and interpret the outputs, commenting on the distributions of the variables and the relationships between them. 

#There seems to be a somewhat positive correlation here but distributions of the variables are scattered quite a bit and
  #some are quite far from the regression line, meaning that the fit of the model is not perfect.

#gender               Age           Attitude          deep            stra            surf           Points     
#Length:166         Min.   :17.00   Min.   :14.00   Min.   :1.583   Min.   :1.250   Min.   :1.583   Min.   : 7.00  
#Class :character   1st Qu.:21.00   1st Qu.:26.00   1st Qu.:3.333   1st Qu.:2.625   1st Qu.:2.417   1st Qu.:19.00  
#Mode  :character   Median :22.00   Median :32.00   Median :3.667   Median :3.188   Median :2.833   Median :23.00  
#                   Mean   :25.51   Mean   :31.43   Mean   :3.680   Mean   :3.121   Mean   :2.787   Mean   :22.72  
#                   3rd Qu.:27.00   3rd Qu.:37.00   3rd Qu.:4.083   3rd Qu.:3.625   3rd Qu.:3.167   3rd Qu.:27.75  
#                   Max.   :55.00   Max.   :50.00   Max.   :4.917   Max.   :5.000   Max.   :4.333   Max.   :33.00  


#Setting three variables as explanatory variables and fitting a regression model where exam points is the (dependent) variable

# create an plot matrix with ggpairs()
library(ggplot2)

ggpairs(learning2014, lower = list(combo = wrap("facethist", bins = 20)))

# create a regression model with multiple explanatory variables
my_model2 <- lm(Points ~ Attitude + stra + deep, data = learning2014)

# print out a summary of the model
summary(my_model2)

#Summary of the fitted model:
#Call:
#lm(formula = points ~ Attitude + stra + deep, data = learning2014)

#Residuals:
# Min       1Q   Median       3Q      Max 
#-17.5239  -3.4276   0.5474   3.8220  11.5112 

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  11.3915     3.4077   3.343  0.00103 ** 
#Attitude      3.5254     0.5683   6.203 4.44e-09 ***
#stra          0.9621     0.5367   1.793  0.07489 .  
#deep         -0.7492     0.7507  -0.998  0.31974    
---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 5.289 on 162 degrees of freedom
#Multiple R-squared:  0.2097,	Adjusted R-squared:  0.195 
#F-statistic: 14.33 on 3 and 162 DF,  p-value: 2.521e-08
  
#Explanatory variable "Attitude" is statistically significant (p<0.05). 
#Explanatory variables "stra" and "deep" are not statistically significant (p>0.05).


#Redo the model without the variables that had no statistical significance. I did a simple regression as two 
  #of the explanatory variables had no significant effect.

# a scatter plot of points versus attitude
qplot(Attitude, Points, data = learning2014) + geom_smooth(method = "lm")

# fit a linear model
my_model3 <- lm(Points ~ Attitude, data = learning2014)
summary(my_model3)

#Call:
#lm(formula = Points ~ Attitude, data = learning2014)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-16.9763  -3.2119   0.4339   4.1534  10.6645 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 11.63715    1.83035   6.358 1.95e-09 ***
#  Attitude     0.35255    0.05674   6.214 4.12e-09 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 5.32 on 164 degrees of freedom
#Multiple R-squared:  0.1906,	Adjusted R-squared:  0.1856 
#F-statistic: 38.61 on 1 and 164 DF,  p-value: 4.119e-09

#Relationship between the target variable (Points) and the explanatory variable (Attitude)
  #is statistically significant (p<0.05).
#Multiple R squared is 0.1906. It is quite low (compared to 1),
  #which means that the data is not that close to the fitted regression line.
  #This indicates that the model explains only some of the variability of the response data around its mean.
  #The model does not seem to fit the data very well.
  
#Draw diagnostic plots Residuals vs Fitted values, Normal QQ-plot and Residuals vs Leverage.
par(mfrow = c(2,2))
plot(my_model2, which = c(1,2,5))

#Assumptions of the model are of that of linear regression:Linear relationship, 
  #multivariate normality, no or little multicollinearity, no auto-correlation and homoscedasticity.

#In the Residuals vs Fitted values the red line is not perfectly flat, which indicates that there is 
  #discernible non-linear trend to the residuals. The residuals do not appear to be equally distributed 
  #across the entire range of the fitted values.

#In the Normal QQ-plot the we can see thet the residuals are not normally distributed as the residual devide from the 
  #diagonal line.

#In the Residuals vs Leverage there are no cases beyond the Cook’s distance lines which means that 
  #there is not any influential cases, i.e. there are no influential outliers.

