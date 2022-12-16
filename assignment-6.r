### Stats with R Exercise sheet 6

##########################
# ANOVA
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 18. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number. 
## Name: Mhd Jawad Al Rahwanji
## Matriculation number: 7038980
## Name: Ali Salaheldin Ali Ahmed
## Matriculation number: 7043295
## Name: Muhammed Saeed
## Matriculation number: 7030400

###########################################################################################



#################################
### Exercise 1: One-way ANOVA
#################################

library(ggplot2)
library(dplyr)
library(MASS)
library(car)
library(tidyr)

## This time we will be working with the "anorexia" data frame (package 'MASS') 

## This is a data set of a clinical study with 3 conditions: Two groups received an active treatment,
## while the control group did not receive treatment. The study population is anorexia patients
## and the recorded response is the weight before the study and the weight after the study for
## for each patient.


## a) Load the dataset, store it into a variable called "data", and briefly inspect it. 
## Feel free to make some plots and calculate some statistics in order to understand 
## the data.
data <- anorexia
summary(data)
str(data)
## b) In a first step, we will concentrate on the dependent variable Postwt and
##  Treat as the predictor variable (we will assume that the weight before treatment is comparable between groups). 
##  Please formulate a sensible research hypothesis.

# H0 = Postwt doesn't differ between treatment groups

## c) Build a boxplot of Postwt depending on "Treat". Please use ggplot here and below!
ggplot(data, aes(Treat, Postwt)) + geom_boxplot()
## d) Looking at the boxplots, is there a difference between the weight between the
##  3 treatment groups?

# Yes the medians and IQRs differ in value and size. CBT and Cont have overlapping
# IQRs whereas FT doesn't overlap with either

## e) Now we are ready to perform 1-way ANOVA: please use the function aov() on 
## Postwt depending on Treat and assign the result to aov1way
aov1way <- aov(Postwt ~ Treat, data=data)
## Before we interpret the results, let's check the ANOVA assumptions and whether 
## they are violated or not and why:

## f) Independence assumption
## (Figure out the best way to check this assumption and give a detailed justified 
## answer to whether it is violated or not.)

# Assuming that none of the patients was in multipel groups and they were separated
# then this assumption is satisfied

## g) Normality of residuals (figure out the best way to check this assumption)
shapiro.test(data[data$Treat == 'CBT',]$Postwt) # p-value = 0.2057 -> Normal
shapiro.test(data[data$Treat == 'FT',]$Postwt) # p-value = 0.007391 -> Not normal
shapiro.test(data[data$Treat == 'Cont',]$Postwt) # p-value = 0.5431 -> Normal
shapiro.test(data$Postwt) # p-value = 0.05781 -> strictly speaking "Normal"
## h) What do you conclude from your results in g? (give a detailed justified answer to whether it is violated or not)

# We think that the assumption isn't satisfied for the groups aren't entirely normal

## i) Homogeneity of variance of residuals (figure out the best way to check this assumption)
bartlett.test(Postwt ~ Treat, data=data) # p-value = 0.01212 -> Heterogeneous variance
## j) What do you conclude from i? (give a detailed justified answer to whether it is violated or not)

# That the assumption is not satisfied either

## k) What are your options if you detect that the data violates the ANOVA assumptions? 

# We use the non-parametric equivalent of ANOVA being Kruskal-Wallis test

## l) Now we turn to the results. Look at the summary of aov1way
summary(aov1way)
## m) State your conclusion

# Ignoring the fact that ANOVA is not suitable and rpetending it is
# We conclude that there exists a stastically significant difference among the treatment
# groups in terms of Postwt's

## n) Use paired.t.test in order to test which levels of Treat are actually different. Use
## "bonferroni" as the method of p-value adjustment.
pairwise.t.test(data$Postwt, data$Treat, p.adjust.method = 'bonferroni')
## o) Bonferroni is known to be a conservative method: it preserves the nominal alpha level,
##  but lacks power to detect effects. An alternative is the "holm" method, which also
##  preserves the overall alpha level, but is less conservative. Try this method.
pairwise.t.test(data$Postwt, data$Treat, p.adjust.method = 'holm')
## p) State your conclusions.

# Conclusion 1: Avoiding Type I error bonferroni only found a significant difference 
# between the FT and Cont groups meaning FT was a successful treatment
# Conclusion 2: Holm being the less conservative (but more prone to Type II error) method 
# found singificant difference between every group pairing meaning FT and CBT were 
# successful treatments and that FT was even better than CBT

##################################
### Exercise 2: 2-way ANOVA
##################################

## Above, we have only looked at post treatment weights. If the sample is big and
## patients were randomly assigned to treatment groups, this is fine to measure the
## success of the treatment as we can assume that weight before the treatment is 
## similar between groups.

## a) Create a graph to see whether prewt is similar between Treat groups.
ggplot(data, aes(Treat, Prewt)) + geom_boxplot()
## b) What is your conclusion?

# The weights are seemingly similar between groups

## Next, we will transform the data set, such that we have one variable combining
## both Prewt and Postwt values and an additional factor coding for Time. This will allow us
## to directly address the change in weight under different treatments in a factorial
## ANOVA.
## Please run the following command.

data_long = anorexia%>% pivot_longer(c(Postwt,Prewt), names_to = "Time", values_to = "Weight") %>%
  mutate(Time = factor(Time, levels=c("Prewt","Postwt")))
summary(data_long)

## c) Plot boxplots for the distribution of `Weight` for each of the `Time` 
## values for data_long. Build 3 plots (each containing 2 boxplots) side by side depending on the 
## `Treat` variable.
ggplot(data_long, aes(Time, Weight)) + geom_boxplot() + facet_grid(cols=vars(Treat))
## d) Describe the pattern you observe in c)

# We can see that the weights in the control group remained unchaged
# The weights in the CBT group slightly increased but we're unsure if it's significant as 
# there remains an overlap
# Lastly, the FT group exhibited an obvious increase that is most probably significant

## e) build a two-way ANOVA including Time and Treat as predictors and their interaction
##  and assign it to aov2way.
aov2way <- aov(Weight ~ Treat * Time, data=data_long)
## f) Report your results in line with the research question.
summary(aov2way)
# Treat groups were significantly different. Weight was significantly different over time.
# The interaction, meaning, there exists a combination of Time x Treat that is superior/inferior
# a.k.a significantly different from the rest.

## g) In order to evaluate the interaction, we will use pairwise tests again. The
## function, we are going to use here is TukeyHSD. Please call the function on the 
##  two-way anova
TukeyHSD(aov2way)
## h) The interaction between Time and Treat produces 15 (!) different comparisons,
##  but not all of them are meaningful to us. Please select three comparisons to report, 
##  which conceptually make most sense! Explain your choice!

# We pick:
# 1- FT-Postwt x Cont-Postwt (p-val = 0.0000730): This shows that patients in FT treatment 
# group had significantly higher weights from the patients who didn't receive treatment. 
# But that's not enough!
# 2- FT-Prewt x Cont-Prewt (p-val = 0.9582327): This shows that the FT and Cont treatment 
#groups had NO significantly different weights to begin with. So we're getting somewhere!
# 3- FT-Postwt x FT-Prewt (p-val = 0.0133651): This shows that the patients in FT group 
# experienced a significant increase in weight over time. 
# So the treatment was infact successful! Hooray!

#################################################
### Exercise 3: independence assumption
#################################################

## The two-way ANOVA above violates the independence assumption.
##  a) Explain why.

# Because for each patient we have a weight value per time value

##  b) Can you think of a way to conduct an ANOVA on this dataset without violating
##  the independence assumption, but taking into account differences between groups 
##  prior to treatment?

# We instead use a column named Weight diff. that substitutes Time and Weight columns
# It is the difference in weight over time per patient.