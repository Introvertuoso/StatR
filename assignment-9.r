### Stats with R Exercise sheet 9

##################################################################################
# Week 11: Model Families and Logistic Regression
##################################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, January 8. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number. 
## Name: Mhd Jawad Al Rahwanji
## Matriculation number: 7038980
## Name: Ali Salaheldin Ali Ahmed
## Matriculation number: 7043295
## Name: Muhammed Saeed
## Matriculation number: 7030400

##################################################################################
##################################################################################

# The following line of code clears your workspace:
rm(list = ls())

library(rstudioapi)
# Set the path to source file location:
setwd(dirname(getActiveDocumentContext()$path)) 

##################################################################################
## Exercise 1: Logistic regression
##################################################################################

require(carData)
require(dplyr)
require(lme4)
require(ggplot2)

## Look at the dataset TitanicSurvival from the carData package.
## a) Build a simple logistic regression model that models the probability of survival 
##    (binary) based on sex (categorical) and  passengerClass (categorical) without 
##    an interaction and store it in mSurv. 
##    You have to use the glm() function and specify the family correctly.
str(TitanicSurvival)
mSurv = glm(survived ~ sex + passengerClass, data=TitanicSurvival, family="binomial")

## b) Look at the summary. What group does the intercept correspond to?
summary(mSurv)
# Females in the First class

## c) Were men more likely to survive than women? Is the effect significant?

# No, on the contrary. Yes, having z = -17.145 and p < 0.001.

## d) Imagine two passengers: Rose (female, 1st class passenger) and Jack (male, 3rd class passenger).
##    Calculate their expected survival on the logit scale (i.e. the scale of the model) 
##    either by hand or using predict() with a new data.frame
rose = predict(mSurv, data.frame(sex= 'female', passengerClass = '1st')) # = 2.109133
jack = predict(mSurv, data.frame(sex= 'male', passengerClass = '3rd'))  # = -2.129

## e) Transform your results from d to the probability scale, using the formula given on the slides. 
##    You can check your calculation by asserting the probabilities lie in the 0-1 range. 
##    For whom does the model predict the higher probability of survival?
exp(rose) / (1+exp(rose)) # = 0.8917877
exp(jack) / (1+exp(jack)) # = 0.10631
# Rose had a higher probability of survival.

##################################################################################
## Exercise 2: Generalized Linear Mixed effect models
##################################################################################

## In this exercise, we will again look at connections between coffee consumption 
## and sleep (among others). The data set "coffee.csv" contains data from 10 students, 
## who reported on 10 randomly chosen days of the year: 
##  sleep:  how many hours of sleep they had in the previous night
##  mood:   how happy they felt on a scale from 1 (very unhappy)-10 (extremely happy)
##  coffee: how many cups of coffee they had on that day
## In addition, the maximal temperature on that day was entered into the dataset.

## Our research hypotheses are: 
## students consume more coffee, when they are tired
## students consume more coffee, if they don't feel well
## students consume more coffee, when it is cold outside

## a) Download the data set from cms and read it in, store it in a variable called: coffeedat
coffeedat = read.csv('coffee.csv')

## b) Plot the number of consumed cups of coffee in three individual scatterplots 
##    by sleep, mood, and temperature. 
##    You can use geom_jitter() to get a nicer plot
ggplot(coffeedat, aes(c(sleep), coffee)) + geom_jitter()
ggplot(coffeedat, aes(c(mood), coffee)) + geom_jitter()
ggplot(coffeedat, aes(c(temperature), coffee)) + geom_jitter()

## c) Can you detect an obvious relationship in any of the plots?

# No, we were unable to detect any apparent relationships

## d) Fit a simple linear regression model with all three predictors and store it in linmod

linmod = glm(coffee ~ sleep + mood + temperature, data=coffeedat)

## e) Fit a generalized linear model with the appropriate family 
##    (hint: coffee is a count variable) and store it in poimod
poimod = glm(coffee ~ sleep + mood + temperature, data=coffeedat, family="poisson")

## f) Look at the two summaries of the models and write what changed?
summary(linmod)
summary(poimod)

# The estimates changed along with their significance status

## g) In fact, we have repeated measures in our design, so refit the model 
##    including a random intercept for subject using glmer() with the correct 
##    family specification and store it in mixedpoi
mixedpoi = glmer(coffee ~ sleep + mood + temperature + (1|subj), data = coffeedat, family="poisson")

## h) Look at the summary and report what changed in comparison to both linmod and poimod.
summary(mixedpoi)

# Compared to poimod, the estimates decreased but the significance status among the predictors remained the same
# Compared to linmod, the estimates increased drastically while significance status changed for each predictor

## i) Finally, to make it complete, also run a mixed model using the gaussian family and store it in mixedlin
mixedlin = lmer(coffee ~ sleep + mood + temperature + (1|subj), data = coffeedat)

## j) Compare the AIC for all four models. Which one has the best fit?
linmod$aic
poimod$aic
anova(mixedlin, mixedpoi)
# mixedpoi seems to have the lowest AIC

## k) And which model is conceptually the appropriate one? Explain why.

# Either poimod or mixedpoi since we're dealing with count data
# this can be confirmed by plotting the coffee histogram
ggplot(coffeedat, aes(coffee)) + geom_histogram(bins=9)
# since we have repeated measures so mixedpoi is the way to go

## l) Finally, report on the effects of interest in light of our research hypotheses 
##    specified above for the model you chose in k)
summary(mixedpoi)
# in mixedpoi we find a negative slope for sleep which goes in line with our hypothesis
# saying that more sleep less coffee or in other words less sleep (more tired) more coffee
# z = -2.697 and p <0,01
# also we find another negative slope for mood which also goes in line with our hypothesis
# better mood leads to less coffee in other words worse mood leads to more coffee
# z = -4.014 and p < 0.001
# lastly, we find a positive slope for temperature which means the higher the temperature 
# the more coffee students consume which doesn't go in line with our hypothesis but 
# as it happens the effect is NOT significant and thus we cannot confirm or deny this effect.
# as far as we're concerned the 3rd hypotheses may be true but we couldnt confirm it for sure.
