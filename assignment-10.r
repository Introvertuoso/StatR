### Stats with R Exercise sheet 10

###############################################################################
# Week 12: Model Selection, Transformations, Power
###############################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, January 22. Write the code below the questions. 
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

# The following line of code clears your workspace:
rm(list = ls())


###############################################################################
### Exercise 1 Simplifying random effect structures
###############################################################################

library(lme4)
library(languageR)

##  Using the lexdec data set again, you want to fit the model that tests for 
##  effects of Frequency, the type of the previous Word and the native 
##  language of the participant:

m = lmer(RT ~ PrevType + Frequency + NativeLanguage + 
              (PrevType + Frequency | Subject) + (PrevType + NativeLanguage | Word), 
        data = lexdec, REML = F)

## a) Unfortunately, the maximal model given above gives a warning that indicates 
##    that the model is too complex for the data. In order to get a model that converges 
##    without warnings, try to use backwards selection on the random effects. 
##    First exclude the random effect that is least contributing to the model fit and so on 
##    (this may require multiple steps and a large number of fitted models!). 
##    Use model comparison to decide which effects can be excluded.
##    You may exclude random effects only, if they don't contribute significantly with alpha set to 0.1
m1 = lmer(RT ~ PrevType + Frequency + NativeLanguage + 
           (Frequency | Subject) + (PrevType + NativeLanguage | Word), 
         data = lexdec, REML = F)
anova(m1, m)
# PrevType by-subject random slope gave Chi(14, 17)= 11.243; p < 0.1 and no convergence

m2 = lmer(RT ~ PrevType + Frequency + NativeLanguage + 
            (PrevType | Subject) + (PrevType + NativeLanguage | Word), 
          data = lexdec, REML = F)
anova(m2, m)
# Frequency by-subject random slope gave Chi(14, 17)= 19.156; p < 0.1 and no convergence

m3 = lmer(RT ~ PrevType + Frequency + NativeLanguage + 
            (PrevType + Frequency | Subject) + (NativeLanguage | Word), 
          data = lexdec, REML = F)
anova(m3, m) 
# PrevType by-word random slope gave Chi(14, 17)= 0; p NS and did converge

m4 = lmer(RT ~ PrevType + Frequency + NativeLanguage + 
            (PrevType + Frequency | Subject) + (PrevType | Word), 
          data = lexdec, REML = F)
anova(m4, m) 
# NativeLanguage by-word random slope gave Chi(14, 17) = 0; p NS but no convergence

## b) Comment on your result in a): were you able to produce a suitable model without convergence problems?

# Yes, m3 (excluded by-word PrevType random slope) achieved convergence while not
# affecting the model significantly

## c) Another approach is to simplify the random effect structure by excluding correlations. 
##    Try out whether this would have solved the problem.
m = lmer(RT ~ PrevType + Frequency + NativeLanguage + 
           (PrevType + Frequency || Subject) + (PrevType + NativeLanguage || Word), 
         data = lexdec, REML = F)
# Excluding correlations does not solve the problem.

###############################################################################
### Exercise 2 Simulations and power
###############################################################################

## In the following we provide you with code for simulations. 
## The goal of the exercise is for you to try out the code and understand what it does.
## Please always execute the code at least 5 times for each subquestion, to see how stable 
## the results are -- this is necessary because we are sampling the data randomly, 
## so it could be that we sometimes get more or less "lucky" draws. 

n        <- 200 # number of observations to be simulated
predA    <- rnorm(n, 80, 20)
predB    <- rnorm(n, 30, 30)
interact <- 0.08*(predA*predB) 
error    <- rnorm(n, 0, 50)

resp     <- 42 + 0.2*predA - 5.3*predB + interact + error
d        <- data.frame(predA, predB, resp)

## a) Write down what values you would hope for the model to estimate in the ideal case:
##    i)   intercept   = 42
##    ii)  predA       = 0.2
##    iii) predB       = -5.3
##    iv)  predA:predB = 0.08

## b) Can the model recover the original model structure and estimate correct coefficients 
##    for the predictors?
lm(resp ~ predA * predB, data=d)
# No, theoretically and empirically, it will not be able to estimate correct coefficients.
# as was said, we may get lucky draws but it isn't stable.
# lastly, we will always be unable to estimate the error.

## c) What happens if you change the number of subjects? (specify the numbers you tried out!)

# Decreasing n (to 100 for example) increased uncertainty and randomness to the extent that
# the numbers have no coherence or resemblance to the original model.
# increasing n (to 1000 for example) resulted in a well behaved estimation that oscillates/varies
# less around the expected values (more certainty) so less SE and a smaller CI for the estimates

## d) What happens if you change the variance of the error term? (specify the numbers you tried out!)

# A lower variance (say 10) reduces the noise and decreases the signal to noise ratio enabling the model
# to capture more of the signal resulting in more confident estimates (near perfect)
# Meanwhile increasing the variance (to 100) leads to astronomical SE and low confidence and overall
# randomness leading to the model's inability to stabilize or settle on any close estimates

## e) What happens if you change the effect sizes? (specify the numbers you tried out!)

# Nothing happens in terms of coefficient estimation only that the model will instead
# try to estimate different coefficients (effect sizes) but it doesn't really affect
# confidence/randomness/certainty/SE
# hence any numbers we've tried only changed coeffcient estimates (0.8 for PredA, -2 for PredB)

## Next we include the above code into a loop to calculate the power of the experiment 
## number of simulated data sets
sim = 1000 # number of simulations
n   = 50  # number of participants in each simulation

## results matrix
results = matrix(nrow=sim, ncol=4)

colnames(results) <- c("Intercept", "predA", "predB", "interaction")
for(i in c(1:sim)){
  predA    <- rnorm(n, 80, 20)
  predB    <- rnorm(n, 30, 30)
  interact <- 0.08*(predA*predB)
  error    <- rnorm(n, 0, 50)
  resp     <- 42 + 0.2*predA - 5.3*predB + interact + error
  d        <- data.frame(predA, predB, resp)
  m1       <- lm(resp~predA*predB, data=d)
  
  ## store the resulting p-values in the results matrix
  results[i,] = summary(m1)$coefficients[,4]
}


## f) We use the above code and the results matrix to calculate power. Recall that the power is 
##    the probability of rejecting the Null hypothesis, given a specific effect size.
##    We can approximate this by calculating the proportion of simulated datasets, 
##    where the effect comes out significant, i.e. below 0.05. 
##    Calculate the power based on the simulations for all three effects of interest 
##    (i.e., predA, predB and the interaction) individually.
sum(results[,2] < 0.05) / sim # 0.067
sum(results[,3] < 0.05) / sim # 1
sum(results[,4] < 0.05) / sim # 1

## g) How does power change when you decrease your alpha level to 0.01?
sum(results[,2] < 0.01) / sim # 0.018
sum(results[,3] < 0.01) / sim # 1
sum(results[,4] < 0.01) / sim # 1

## h) How does power change, when you decrease the number of participants in each simulated data 
##    set to 50? (alpha-level = 0.05)
sum(results[,2] < 0.05) / sim # 0.057
sum(results[,3] < 0.05) / sim # 0.99
sum(results[,4] < 0.05) / sim # 1
