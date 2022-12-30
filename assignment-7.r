### Stats with R Exercise sheet 7

##############################################################################
#Week8: Checking Assumptions underlying ANOVA and linear regression
##############################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, January 1. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number. 
## Name: Mhd Jawad Al Rahwanji
## Matriculation number: 7038980
## Name: Ali Salaheldin Ali Ahmed
## Matriculation number: 7043295
## Name: Muhammed Saeed
## Matriculation number: 7030400

###############################################################################
###############################################################################

# The following line of code clears your workspace.

rm(list = ls())


########
### Exercise 1
########

########
### Please, use ggplot to make plots in all exercises unless specified differently!
########


##a) Load the dataset Salaries from package carData and store it in a variable called data. 
# Familiarize yourself with the content of the dataset: 
# https://r-data.pmagunia.com/dataset/r-dataset-package-car-salaries

library(carData)
library(ggplot2)
data <- Salaries

## b) Run a simple regression, just including 'years since PhD' as predictor and salary as the dependent variable
##  Store it in lm1

lm1 <- lm(salary ~ yrs.since.phd, data=data)

## c) Report and explain the effect of 'years since PhD'

# We can see that the effect is underwhelming and unreliable to predict the salary
# since the R2 score was 0.1758

## d) Make a scatterplot of salary by 'years since PhD', including the regression line

ggplot(data, aes(yrs.since.phd, salary)) + geom_point() + geom_smooth(se=FALSE, method="lm")

## e) Next, fit a model of salary including 'years since PhD' and discipline as predictors, store it in lm2

lm2 <- lm(salary ~ yrs.since.phd + discipline, data=data)

## f) Report and explain the effects of 'years since PhD' and discipline.

# This combination explained more of the variance in the data but is still lackluster
# having an R2 score of 0.2401

##  Next we want to plot a model where both predictors are shown. For those we first store the predicted values
## of our model:
data$sal_pred = fitted(lm2)

## g) Now, plot the original data (salary by 'years since PhD' with different colors for disicpline), but use the 
## fitted values (sal_pred) inside geom_smooth() or geom_line(), otherwise, it will display regression lines 
## assuming an interaction
## The resulting plot should show the data points in different colors and two parallel regression lines.

ggplot(data, aes(yrs.since.phd, salary, color=discipline)) + geom_point() + geom_line(aes(yrs.since.phd, sal_pred))

## h) Run a regression model that includes also the interaction 
# between 'years since PhD' and discipline and store it as lm3

lm3 <- lm(salary ~ yrs.since.phd * discipline, data=data) 

## i) Plot the results of the model! (This time no need to specify the pred data set)

ggplot(data, aes(yrs.since.phd, salary, color=discipline)) + geom_point() + geom_smooth(se=FALSE, method="lm")

## j) Report the results of lm3 and interpret with the help of the graph in i)

# The regression is still poor as there still remains a lot of error in the predictions
# the interaction's effect isn't statistically significant and lastly, we can see that
# discipline B has a similar trend as A but with a positive shift in salary

## k) Do model checking on your model lm3, i.e. inspect the standard model plots provided by R (no ggplot, 
## see lecture notes for syntax)

par(mfcol=c(2,3))
plot(lm3, which=seq(1,6))

## l) Interpret what you see in k) and possibly suggest further steps

# 1- The residuals are normal
# 2- We have heterogeneous variance, thus, Kruskal-Wallis H test is recommended instead
# or a Welch's ANOVA
# 3- We have high leverage points caused massive skew in datapoints with indices:
# 132, 283 and 318 which may need removing and trying again.
