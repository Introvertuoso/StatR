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
## Name: Mhd Jawad Al Rahwanji
## Matriculation number: 7038980
## Name: Ali Salaheldin Ali Ahmed
## Matriculation number: 7043295
## Name: Muhammed Saeed
## Matriculation number: 7030400

## Only 1 member needs to submit! 

#################################################################################
##  Exercise 1: Cleaning data
#################################################################################

## download the file insomnia22.csv from cms
## The made-up dataset insomnia contains data of a survey on 60 students. 
## They are asked two questions: whether they regularly encounter sleep problems
## and what their preferred and most consumed drink is.

## a. Load the libraries stringr, dplyr, tidyr and forcats
install.packages(c("stringr", "dplyr", "tidyr", "forcats"))
require(stringr)
require(dplyr)
require(tidyr)
require(forcats)
## b. read in the data
data <- read.csv("insomnia22.csv")
## c. get a summary of the dataset
summary(data)
## d. the variable sleepProblem should be a numerical variable and have 0 for no Problem 
##    and 1 for sleep problems.
##    Make sure that this is the case
data <- data %>% mutate(adj_sleepProblem = ifelse(sleepProblem %in% c("1", "11", "yes"), 1, 0))
## e. how many students encounter sleep problems?
sum(data$adj_sleepProblem) # = 25
## f. how many different drinks do students name? (transform the variable into a 
## factor first)
levels(factor(data$drink)) # = 10 (should be 3)
## g. collapse factor levels which were spelled wrong. Make sure you first handle
## case and whitespace incongruencies, before you fix individual misspellings
data <- data %>% mutate(adj_drink = str_to_lower(str_trim(drink))) %>% 
  mutate(adj_drink = replace(adj_drink, adj_drink %in% c("cofee", "coffe", "koffee"), "coffee")) %>%
  mutate(adj_drink = replace(adj_drink, adj_drink == "tee", "tea"))
## You realize that most students had multiple exams in the week from Feb 22 to 
## Feb 26. As students had to learn a lot and were possibly worried, they might 
## misjudge or exaggerate their sleep problems as occurring "regularly"
## We want to exclude all data that was collected between and including Feb 15 
## and Feb 26!
## h.  First show how many data points will be concerned, you need to transform
##     the date column to a Date object first!
data <- data %>% mutate(adj_date = as.Date(date))
sum(data$adj_date >= as.Date("2021-02-15") & data$adj_date <= as.Date("2021-02-26")) # = 10
## i. Now filter out this part of the data and assign the result to clean
clean <- data %>% filter(adj_date < as.Date("2021-02-15") | adj_date > as.Date("2021-02-26"))
####################################################################
### Exercise 2: Chi-squared test
####################################################################

## consider the data set from above. If you had problems performing the
## required cleaning steps, note that you can also do them by hand
## Now consider we want to see whether the preferred drink influences sleep problems

## a. formulate in plain English what the Null hypothesis is in this context

# The drink has no effect on the sleeping pattern of the individuals.

## b. conduct a chisquare test to test this hypothesis using the function chisq.test()
##    and assign the result to chi
chi <- chisq.test(x=clean$adj_drink, y=clean$adj_sleepProblem)
## c. the last call produced a warning. To understand why this warning arises, look
##    at observed and expected frequencies of chi
chi['expected']
chi['observed']
## d. What are the expected frequencies? Do we need to look at expected or 
##    observed frequencies?

# They are the frequencies of each combination of classes we expect to observe 
# if the H0 is TRUE
# We need to look at E to make sure the assumption that p>5 holds (sufficiently large).

## e. a possible solution is to sample more participants. Given that the smallest 
##    admissible value is 5, from which group(s) in terms of preferred drinks do
##    we have to sample more?

# We need to sample more Tea enjoyers.

## f. Assume we don't have the possibility to sample more students. Which test do
##    you have to run instead? How does it work roughly? Perform a suitable test

# Fisher's exact test solves the problem.
# - Take all possible 2 x 2 tables that could be formed from the fixed set of marginal totals.
# - Determine the sum of the probabilities of those tables whose results were as extreme
#       or more extreme than the table obtained in the data.
# - if this sum is less than a, reject the null hypothesis
fisher.test(x=clean$adj_drink, y=clean$adj_sleepProblem)

## g. Lastly, what is the conclusion of your test? What have you learned and what 
##    have you not learned? 

# We performed the Fisher's test and got p<0.05 rejecting the H0 which means that
# drinks do have an effect on sleeping patterns.
# What we haven't learned is, do all drinks affect sleep or only a subset? If so, 
# which of our drinks is responsible has an effect and which hasn't.
# This however can be guessed at by studying the contingency table.

#########################################
## Exercise 3. Binomial distribution
#########################################
##  In a board game, you have to roll a fair die. You will get a point, 
##  each time the number is higher than 4. You roll 20 times

## a) What is the chance in a single roll of earning a point?

# It is the probability P(X > 4) = 2 / 6

## b) Please calculate the probability of getting exactly 3 points.
##    Calculate this using the dbinom() function.
dbinom(3, 20, 2/6) # = 0.04285383
## c) Next please calculate the probability of getting less than 6 points
pbinom(6, 20, 2/6) # = 0.4793427
## d) What is the difference between density function and distribution function?

# The density function calculates the area under the curve at value x with either
# an upper or a lower tail. This gives the probability of getting any values above or
# below a given x.
# The distribution function calculates the probability which is the corresponding y
# of any given event x. This gives the probability of getting exactly a value x.

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

# No, we cannot because this violates the assumption that the observations are independent.
# Since the same person is being monitored across all weeks, we have a case of 
# dependence between observations and that we are studying the DIFFERENCE instead.

## b) Is there an alternative test you could use? Why would this be appropriate?

# The solution in this case is using the McNemar's test. Because it focuses on the
# changes the occur throughout the weeks for each of the participants and not the
# individual observations eliminating the dependency.




