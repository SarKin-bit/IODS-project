alc<- read.delim("alc.txt")
alc<-read.table(file = "alc.txt", sep="\t", header=TRUE)

dim(alc)
str(alc)

# access the tidyverse libraries tidyr, dplyr, ggplot2
library(tidyr); library(dplyr); library(ggplot2)

# glimpse at the alc data
glimpse(alc) 

# use gather() to gather columns into key-value pairs and then glimpse() at the resulting data
gather(alc) %>% glimpse

# draw a bar plot of each variable
gather(alc) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar()


#There are 382 observations (rows) and 35 variables (columns). Variables in the data are for example school, sex, age, adress, family size, family educational support, free time after school and internet access at home.
#For more detailed explanation of the variables, please visit: https://archive.ics.uci.edu/ml/datasets/Student+Performance

#3.	The purpose of your analysis is to study the relationships between high/low alcohol
#consumption and some of the other variables in the data. To do this, choose 4 interesting
#variables in the data and for each of them, present your personal hypothesis about their 
#relationships with alcohol consumption. 

#Chosen interesting variables: student's grade, school, home address type and student's health.

#Hypothesis 1: There is a relationship between alcohol use and student's grade. 
#Hypothesis 2: There is a relationship between alcohol use and student's absences.
#Hypothesis 3: There is a relationship between alcohol use and student's health.
#Hypothesis 4: There is a relationship between alcohol use and student's quality of family relationships.

#24 famrel - quality of family relationships (numeric: from 1 - very bad to 5 - excellent). 
#Having poor family relationship I would imagine predicts high alcohol use. I would guess this to have the strongest
#relation to high alcohol use of my variable selections.

#4.	Numerically and graphically explore the distributions of your chosen variables and their 
#relationships with alcohol consumption (use for example cross-tabulations, bar plots and box plots).
#Comment on your findings and compare the results of your exploration to your previously stated hypotheses.



#Hypothesis 1: There is a relationship between alcohol use and student's grade. 
attach(alc)
table(G3,high_use,sex)

library(ggplot2)

# initialise a plot of high_use and G3
g1 <- ggplot(alc, aes(x = high_use, y = G3, col = sex))

# define the plot as a boxplot and draw it
g1 + geom_boxplot() + ylab("grade")

#Initialise a plot of high use and sex.
g2<-ggplot(data = alc, aes(x = high_use, fill = sex))

#Define the plot as a bar lot and draw it.
g2 + geom_bar()+facet_wrap("sex")

#Men who have high usage of alcohol (use a lot of alcohol) have lower grades than men who do not use a lot of alcohol. For women, the high use of alcohol does not affect the grade. 


#Hypothesis 2: There is a relationship between alcohol use and student's absences.
attach(alc)
table(G3,absences,sex)

library(ggplot2)

# initialise a plot of high_use and absences
g3 <- ggplot(alc, aes(x = high_use, y = absences, col = sex))

# define the plot as a boxplot and draw it
g3 + geom_boxplot() + ggtitle("Student absences by alcohol consumption")

#For male students, high users of alcohol have more absences from school. For female student, the number of absences is quite similar weather they use high amount of alcohol or not.


#Hypothesis 3: There is a relationship between alcohol use and student's health.
# initialise a plot of high_use and health
g4 <- ggplot(alc, aes(x = high_use, y = health, col = sex))

# define the plot as a boxplot and draw it
g4 + geom_boxplot() + ggtitle("Student health by alcohol consumption")


#For male students, their health score was similar weather they were high users of alcohol or not. For female students, the health score was higher for those students who were high users of alcohol, surprisingly. In high alcohol users there was a lot more variation though.

#Hypothesis 4: There is a relationship between alcohol use and student's quality of family relationships
# initialise a plot of high_use and health
g4 <- ggplot(alc, aes(x = high_use, y = goout, col = sex))

# define the plot as a boxplot and draw it
g4 + geom_boxplot() + ggtitle("Going out with friends by alcohol consumption")

#Both male and female high alcohol users go out with friends more than low alcohol users. 


#Find the model with glm().
m <- glm(high_use ~ absences + G3 + health + goout, data = alc, family = "binomial")

# print out a summary of the model
summary(m)

# print out the coefficients of the model
coef(m)

#Absences and going out with friends are highly significant predictor of the probability of being a high user of alcohol. 
#Health and grade are not significant predictor of the probability of being a high user of alcohol. 

###GHQ score is a highly significant predictor of the probability of being judged a case


#Present and interpret the coefficients of the model as odds ratios and provide confidence intervals for them. 

# find the model with glm()
m <- glm(high_use ~ absences + G3 + health + goout, data = alc, family = "binomial")

# compute odds ratios (OR)
OR <- coef(m) %>% exp

# compute confidence intervals (CI)
CI <- confint(m) %>% exp

# print out the odds ratios with their confidence intervals
cbind(OR, CI)


#6.	Using the variables which, according to your logistic regression model, had a statistical relationship with
#high/low alcohol consumption, explore the predictive power of you model. 



#Absences, health, and going out with friends are all positively associated with high use of alcohol whereas grades is negatively associated with high alcohol use.
#Every absence causes the student 8 % more likely to be a high user of alcohol.
#For every increase in the health score the student is 13.5% more likely to be a high user of alcohol.
#Each time the student goes out with friends, they are 107 % more likely to be a high user of alcohol.
#For every increase in the grade the student is 4 % less likely to be a high user of alcohol

#An OR of 1.2 means there is a 20% increase in the odds of an outcome with a given exposure.
#An OR of 0.2 means there is an 80% decrease in the odds of an outcome with a given exposure.
#An OR of 2 means there is a 100% increase in the odds of an outcome with a given exposure. 




#Exploring the predictive power of the model.

#Fit the model using only the variables that had a statistical relationship with high/low alcohol consumption .
m <- glm(high_use ~ absences + goout, data = alc, family = "binomial")

#Predict() the probability of high_use.
probabilities <- predict(m, type = "response")

#Add the predicted probabilities to 'alc'.
alc <- mutate(alc, probability = probabilities)

#Use the probabilities to make a prediction of high_use.
alc <- mutate(alc, prediction = probability > 0.5)

#See the last ten original classes, predicted probabilities, and class predictions
select(alc, failures, absences, sex, high_use, probability, prediction) %>% tail(10)

#Tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction)

#23 times the prediction is "high alcohol use" when the variable is not "high alcohol use".
#67 times the prediction is not "high alcohol use" when the variable is "high alcohol use".


#Access dplyr and ggplot2.
library(dplyr); library(ggplot2)

#A graphic visualizing both the actual values and the predictions. 
#Initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(alc, aes(x = probability, y = high_use, col = prediction))

#Define the geom as points and draw the plot
g + geom_point()

#Tabulate the target variable versus the predictions.
table(high_use = alc$high_use, prediction = alc$prediction) %>% prop.table %>% addmargins

#According to the prediction, 82% of all the students are not high alcohol users.
#According to the actual values 70% of all the students are not high alcohol users. There is a quite big difference between the prediction and the actual model.



#Compute the total proportion of inaccurately classified individuals (the training error).
#Define a loss function (average prediction error).
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

#Call loss_func to compute the average number of wrong predictions in the (training) data.
loss_func(class = alc$high_use, prob = alc$probability)

#The training error is about 24%, which shows that the accuracy of the model is about 76%.

