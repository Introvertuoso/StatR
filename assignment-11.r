### Stats with R Exercise sheet 11

###############################################################################
#Week 14: Bayesian statistics 2
###############################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, February 5. Write the code below the questions. 
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

## The following line of code clears your workspace.
rm(list = ls())


###############################################################################
### Exercise 1
###############################################################################

##  We will again be using the lexdec dataset from library languageR. 
##  In previous sheets, we used different ways to analyse this data. 
##  This time, we will run a multiple regression model, and repeat it 
##  as a Bayesian analysis using package 'brms'.


## a) Load the dataset lexdec from package languageR and store it in a variable called data
library(languageR)
data = lexdec

## b) Load the package brms
install.packages("brms")
library("brms")

## c) Fit a (frequentist) linear model of RT (lm function) including Frequency and PrevType 
##    as predictors, store it in lm1
lm1 = lm(RT ~ PrevType + Frequency, data=data)

## d) Fit the same model as a Bayesian regression using the function brm() 
##    and using only defaults (you don't need to specify priors or fitting 
##    parameters like chains and iterations). Store it in bm1
bm1 = brm(RT ~ PrevType + Frequency, data=data)

## e) Look at the summaries of bm1 and lm1
summary(lm1)
summary(bm1)

## f) How do the parameter estimates compare?

# They are almost identical

## g) Store the posterior samples of b_Frequency in the variable ps_freq. 
##    Use the function as_draws_df()
ps_freq = as_draws_df(bm1)$b_Frequency

## h) Your colleague claims that the effect of frequency has to be smaller 
##    (meaning more negative) than -0.03. What is the probability of the 
##    frequency effect being more negative than -0.03 given your posterior samples?
##    Do you agree with your colleague?
sum(ps_freq < -0.03) / length(ps_freq) # 0.9985

## i) Derive 95% and 80% credible intervals from ps_freq. Compare to the results above.
quantile(ps_freq, c(0.025, 0.975)) # -0.05177540 -0.03454635
quantile(ps_freq, c(0.1, 0.9)) # -0.04872102 -0.03743115

## j) What is the meaning of a credible interval compared to the confidence interval 
##    in the frequentist's approach?

# The credible interval is an interval within which an unobserved parameter value falls
# with a particular probability
# The confidence interval determines the upper and lower bounds of an observed variable
# with varying samples

## k) Plot the model using the default 'plot' function. This will give you the posteriors 
##   of the model parameters as well as the trace plot, which give you an indication 
##   of the convergence of your model. The trace plot is supposed to look like a 
##   "fat hairy caterpillar", i.e. the different chains should not be separated in 
##   any part of the plot and there should not be a general pattern. Is this the case?
plot(bm1)

# Yes this is the case your honor

## l) We want the model to run quicker. Change the settings such that each chain 
##    only has 120 iterations with 1/3 of them as warmup. Store the result in bm2 
##    and look at summary and trace plots. Use the provided seed to be able to better 
##    compare your results (or try a different one, but provide it together with your answer!)
set.seed(1111)
bm2 = brm(RT ~ PrevType + Frequency, data=data, iter = 120, warmup = 40)
summary(bm2)
plot(bm2)

## m) Do you think reducing the iterations was a good idea? Give reasons!

# No it was not a good idea!
# ESS values are too low and hence we now have a model that hasn't converged.
# Our trace plots show separated chains.
# We also have off posteriors as they haven't converged.
# Our Rhats are all above 1.05 and hence we have bad estimates.

## n) Another colleague of yours said 2 months ago to you that the effect of frequency 
##    is most likely at -0.01 +-0.005. Use these numbers for a normal prior of Frequency 
##    (with 0.005 as sd). Assign the model to bm3. 
bm3 = brm(RT ~ PrevType + Frequency, data=data, prior = prior(normal(-0.01, 0.005), class=b, coef=Frequency))
summary(bm3)
plot(bm3)

## o) How did the estimate and credible interval of frequency change?

# Frequency estimate changed from -0.04 to -0.03, while the CI changed from
# [-0.05, -0.03] to [-0.03, -0.02] so it became tighter around the estimate
# We also did notice a change in the intercept as well.

## p) What class of priors does the above one belong to? 

# class is b because its a prior for a predictor and not intercept or sigma
