### Stats with R Exercise sheet 3

#################################################
#Tests for Categorical Data and cleaning data
#################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 27th. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Make sure you answered ALL subquestions and that your code actually runs before submitting!


## Please write below your (and your teammates) name, matriculation number. 
## Name:
## Matriculation number:
## Name:
## Matriculation number:
## Name:
## Matriculation number:

## Only 1 member needs to submit! 

#################################################################################
##  Exercise 1: Cleaning data
#################################################################################

## download the file insomnia22.csv from cms
## The made-up dataset insomnia contains data of a survey on 60 students. 
## They are asked two questions: whether they regularly encounter sleep problems
## and what their preferred and most consumed drink is.

## a. Load the libraries stringr, dplyr, tidyr and forcats

## b. read in the data

## c. get a summary of the dataset

## d. the variable sleepProblem should be a numerical variable and have 0 for no Problem 
##    and 1 for sleep problems.
##    Make sure that this is the case

## e. how many students encounter sleep problems?

## f. how many different drinks do students name? (transform the variable into a 
## factor first)

## g. collapse factor levels which were spelled wrong. Make sure you first handle
## case and whitespace incongruencies, before you fix individual misspellings

## You realize that most students had multiple exams in the week from Feb 22 to 
## Feb 26. As students had to learn a lot and were possibly worried, they might 
## misjudge or exaggerate their sleep problems as occurring "regularly"
## We want to exclude all data that was collected between and including Feb 15 
## and Feb 26!

## h.  First show how many data points will be concerned, you need to transform
##     the date column to a Date object first!

## i. Now filter out this part of the data and assign the result to clean

####################################################################
### Exercise 2: Chi-squared test
####################################################################

## consider the data set from above. If you had problems performing the
## required cleaning steps, note that you can also do them by hand
## Now consider we want to see whether the preferred drink influences sleep problems

## a. formulate in plain English what the Null hypothesis is in this context

## b. conduct a chisquare test to test this hypothesis using the function chisq.test()
##    and assign the result to chi

## c. the last call produced a warning. To understand why this warning arises, look
##    at observed and expected frequencies of chi

## d. What are the expected frequencies? Do we need to look at expected or 
##    observed frequencies?

## e. a possible solution is to sample more participants. Given that the smallest 
##    admissible value is 5, from which group(s) in terms of preferred drinks do
##    we have to sample more?

## f. Assume we don't have the possibility to sample more students. Which test do
##    you have to run instead? How does it work roughly? Perform a suitable test

## g. Lastly, what is the conclusion of your test? What have you learned and what 
##    have you not learned? 


#########################################
## Exercise 3. Binomial distribution
#########################################
##  In a board game, you have to roll a fair die. You will get a point, 
##  each time the number is higher than 4. You roll 20 times

## a) What is the chance in a single roll of earning a point?

## b) Please calculate the probability of getting exactly 3 points.
##    Calculate this using the dbinom() function.

## c) Next please calculate the probability of getting less than 6 points

## d) What is the difference between density function and distribution function?


#########################################
## Exercise 4
#########################################

##  In order to better understand the relationship between sleeping problems and 
##  consumed drinks, we set up a better controlled experiment: 
##  For two weeks, students are asked to drink mostly coffee and are then asked
##  whether they encountered sleep problems. For another two weeks, the same students
##  are asked to switch to tea and then again asked for sleeping problems.

## a) Can you use the ChiSquare test in this situation? Explain and motivate
##  your answer

## b) Is there an alternative test you could use? Why would this be appropriate?





