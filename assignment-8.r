### Stats with R Exercise sheet 8

##############################################################################
# Week 10: Linear Mixed Effects Models
##############################################################################

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

###############################################################################
###############################################################################

# The following line of code clears your workspace:
rm(list = ls())

library(languageR)
library(lme4)
library(lattice)
library(Matrix)
library(ggplot2)
library(dplyr)


###############################################################################
### 1. Linear mixed model for chicken growth 
###############################################################################

## a) We will first look at the dataset ChickWeight, which is already 
##    loaded in base R. Check out the help page of the data set to understand 
##    how the data was collected and look at the summary.
help("ChickWeight")

## b) Let's plot the data. 
##    1) Group the data by Diet and Time. Use a function summarySE() 
##       from Rmisc library to get the mean and se of weight. 
##       Assign resulting dataset to aggData.
library(Rmisc)
?summarySE()
stats = summarySE(data=ChickWeight, "weight")
aggData = ChickWeight %>% group_by(Diet, Time)

##    2) Create a line plot of aggData, plotting weight on the y axis, time on the x-axis and color by Diet. 
##       Also add errorbars (mean+/-1.96*se)
ggplot(aggData, aes(Time, weight, color=Diet)) + geom_line(aes(group = 1))
  geom_errorbar(aes(ymin=stats$weight-stats$se*1.96, ymax=stats$weight+stats$se*1.96), width=0.2) + geom_point(size = 2)

## c) The above plot ignored that the data comes from a fixed set of chicks. Let's look at individual growth
##    by coloring by Chick instead of by Diet, but faceting by Diet (side by side). You have to use ChickWeight 
##    instead of aggData here! Also you don't need error bars, because you are not plotting the mean, but the
##    actual data
ggplot(ChickWeight, aes(Time, weight, color=Chick)) + geom_line(aes(group = 1)) + facet_wrap(vars(Diet))

## d) What do you observe, looking at c?

# We observe that not all Chicks tried all diets and that chicks experienced an increase in
# their weights over time no matter the diet but at varying rates which may be attributed
# to either biological growth or growth & diet together

## e) We want to investigate whether the type of diet has an effect on the chick's growth, i.e. we are
##    looking for an interaction between time after birth and the diet type. Before running the model,
##    specify:

##    1) What fixed effect(s) do you enter into the model?

# Time is a fixed effect

##    2) what random effect(s) should be included to account for the repeated measures structure of the data?

# Diet and Chick are random effects

##    3) In addition to random intercept(s), which random slope(s) should you add to get a maximal model?

# random slope for diet-time interaction

## f) Run the model you specified in e) using lmer() and assign it to chickmod
chickmod = lmer(weight ~ Time*(1|Diet)+(1+Time|Chick), data=ChickWeight)

## g) Rerun the model leaving out the interaction between Time and Diet and assign it to chicknull
chicknull = lmer(weight ~ Time+(1|Diet)+(1|Chick), data=ChickWeight)

## h) compare the two models using the anova() function, which performs a likelihood ratio test
anova(chickmod, chicknull)

## i) Report the p-value (from h) and the conclusion with respect to the research hypothesis

# We used R (R Core Team, 2012) and lme4 (Bates, Maechler & Bolker,
# 2012) to perform a linear mixed effects analysis of the relationship
# between weight and diet over time in chicks. As fixed effects, we entered time into the model.
# As random effects, we had intercepts for chicks and diets, as well as by-chick
# random slopes for the effect of time. P-values were obtained by likelihood ratio tests of the full
# model with the interaction against the model without the interaction.
#  X2 (2, N = 578) = 789.4, p < .001

## j) The following code creates a plot of all chick specific intercepts and slopes. What do you see?
print(dotplot(ranef(chickmod,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["Chick"]])

# Chick's have different intercepts as well as different slopes w.r.t time when explaining weight
# as each grows biologically differently from the rest same thing with the initial weight at birth
# and then comes the effect of the diet which is constant over time

#####################################################
### 2. Random effect structures 
#####################################################

## a) Let's return to the lexdec data set from Sheet 4 and suppose, we want to look 
##    at effects of the word type of the previously presented word (each subject saw a 
##    different randomized sequence) and effects of the complexity of the word itself, while 
##    taking into account the dependence between data points collected on the same word and from the same subject. 
##    Which of the following models has a maximal random effect structure given the experimental design?
##    Motivate your choice.

m1 = lmer(RT ~ PrevType + Complex + (PrevType        |Subject) + (         Complex| Word), lexdec)
m2 = lmer(RT ~ PrevType + Complex + (PrevType+Complex|Subject) + (PrevType        | Word), lexdec)
m3 = lmer(RT ~ PrevType + Complex + (PrevType+Complex|Subject) + (PrevType+Complex| Word), lexdec)
m4 = lmer(RT ~ PrevType + Complex + (         Complex|Subject) + (PrevType        | Word), lexdec)
m5 = lmer(RT ~ PrevType + Complex + (PrevType+Complex|Subject) + (1               | Word), lexdec)

# m3, because it includes all the possible by-subject and by-word random slopes that may affect
# the RT of a given word as previous words alongside current ones can affect the subject
# since they may leave lingering / accumulating effects throughout the sequence

## b) You want to relate students' performance in the advanced algebra course in a summer school in Saarbrücken
##    to their final math grade in school. The summer school course has 200 participants, coming from 8 different
##    partner Universities from all over Germany. These 200 participants were randomly split into 10 tutorial groups,
##    where each tutorial was held by a different tutor.
##    Given the design of your study, what random effects should you add to the following model:
##    NOTE: We accept only the answers with explanations!

## lmer(advancedalgebrascore ~ mathGrade, someData)

# we would add a Participant (subject) random effect since each is different
# we would add a University random effect since different participants were subjected to
# different curricula at their university (hierarchical?)
# we would also add a Tutor (item) random effect since different participants are split into
# different tutorial groups where they are subjected to different teaching styles

# we would add a by-Participant and by-Tutor random slopes for the effect of MathGrade.
# since a student may be predisposed in math and thus more or less receptive
# similarly a tutor may be predisposed in math and thus more or less affect the pupils


