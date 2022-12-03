### Stats with R Exercise sheet 4

##########################
#Week 5: t-test and friends
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 4. Write the code below the questions. 
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
###########################################################################################

#####################################################
### 1. Restructuring, plotting, and first t test
#####################################################

library(lsr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(languageR)

## We will be working with the dataset lexdec from the package languageR
## In short, this data set contains reaction times from an experiment, where participants had 
## to decide whether something is a word or not (nonword). Only responses for real words are
## included and there were 79 measurements per participant.
## 
## Variables we will be interested in include 
## Subject (code for a participant)
## Complex (whether the word is a compound (e.g.blackberry) or not (e.g.cherry))
## RT (log reaction time)
## Sex (of the participant)
## Frequency (log-transformed frequency of the word in the CELEX corpus per million words)


## a. Create the dataset lex, which is a copy of lexdec, but only includes the columns 
##  indicated above
lex <- lexdec %>% select(c(Subject, Complex, RT, Sex, Frequency))
## Say you are interested in the influence of the frequency of a word on lexical decision time.
## In particular, you want to compare high frequency words to low frequency words using a t-test.

## b. Why is this not possible with the data as it is?

# Because t.test requires variables to be categorical x continuous but in this case we have continuous x continuous

## Run the following line to prepare the dataset for later steps:
lex = lex %>% mutate(Freq = as.factor(ifelse(Frequency > 4.75, "high", "low")))

## c. Look at the new variable. Describe how frequency was transformed and why.

# Frequency variable was factorized and is now categorical. This allows us
# to apply the t.test

## Before we start testing, we want to get an impression of the data and create a barplot of 
## the mean by Freq, including error bars that show the 95% CI.
## Here, we define a function to calculate the standard error, which is needed for the CI:
## (just execute the next line, as you will need the function in 2.)
se = function(x){sd(x)/sqrt(length(x))}

## d. To start, we need to summarize the data. Use the functions group_by() in combination with
##  summarise(). In particular, you need to group by Freq and get the mean as well as the
##  se of RT. Store the result to summaryByFreq
##  You will find examples of how the summarizing can be done here:
##  https://datacarpentry.org/R-genomics/04-dplyr.html#split-apply-combine_data_analysis_and_the_summarize()_function
summaryByFreq <- lex %>% 
  group_by(Freq) %>%
  summarize(m = mean(RT), std_e = se(RT))
## e. Describe the resulting data set (summaryByPrevType) in your own words

# It shows a summary of the mean and standard error of RT for each Frequency category

## f. Now use summaryByFreq to create the barplot with error bars denoting the 95% CI
##  (i.e. mean +/-1.96 * se)
ggplot(summaryByFreq, aes(x=Freq, y=m, fill = Freq)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=m-1.96*std_e, ymax=m+1.96*std_e))
## g. The barplot always starts at zero, which makes the portion of the graph, we are most 
##  interested in (i.e. the spread of the error bars) hard to perceive. As an alternative,
##  construct a line plot of the same data, again including error bars.
##  Hint: if you get a complaint, try to add group = 1 to your aes
ggplot(summaryByFreq, aes(x = Freq, y = m, fill = Freq, group = 1)) + 
  geom_line() +
  geom_errorbar(aes(ymin=m-1.96*std_e, ymax=m+1.96*std_e), width = 0.2)
## h. Gauging from the plot, does it look like there's an important difference in mean RT 
##  for low and high frequency words?

# Yes, because neither of the means lies within the other mean's 95% CI

## i. Let's go back to the original data frame "lex".
##  Now that you've taken a look at the data, you want to get into the stats.
##  You want to compute a t-test for the average RT for low vs high frequency nouns.
##  Why can't you compute a t-test on the data as they are now? 
##  Hint: Which assumption is violated?

# The assumption that the observations are independent

## j. We need to restructure the data to only one observation (average RT) per subject 
##  and low/high condition (Freq). We will again use group_by and summarize, but
##  this time we have to group by Subject and Freq, while we only need the mean to be 
##  stored, not the se. Assign the result to bySubj
bySubj <- lex %>%
  group_by(Subject, Freq) %>%
  summarize(m = mean(RT))
## k. Create histograms of the RT data in bySubj depending on the frequency category 
##  and display them side by side. Set the binwidth to 0.08
ggplot(bySubj, aes(x = m)) +
  geom_histogram(binwidth=0.08) +
  facet_grid(.~Freq)
## l. Display the same data in density plots. 
ggplot(bySubj, aes(x = m)) +
  geom_density() +
  facet_grid(.~Freq)
## m. Based on the histograms and the density plots - are these data likely coming
## from a normal distribution?

# No, we don't think so

## n. Create boxplots of the mean RT in bySubj by Freq
ggplot(bySubj, aes(x = Freq, y = m)) +
  geom_boxplot()
## o. We want to compute a t-test to compare the mean RT between lexical decisions on low
##  frequency words vs high frequency words using the data in bySubj.
##  Do you need a paired t-test or independent sample t-test? why?

# We need a paired test since each subject has an observation (average RT) for each
# group in the dependent variable

## p. Compute the t-test you specified above
t.test(m ~ Freq, data = bySubj, paired=TRUE)
## q. What does the output tell you? What conclusions do you draw?

# There exists a significant difference between the 2 groups. When frequency is low 
# RT is significantly higher

## r. Compute the effect size using Cohen's D.
cohensD(m ~ Freq, data = bySubj) 
## s.  Which effect size do we get? How do you interpret this result?

# = 0.7592481, as a general rule of thumb 0.8 is considered a large effect so our
# calculated 0.75 is close to that so we'd say we have a large effect size

## t. Why would you report the effect size in addition to the p-value?
# p-value on its own only reports significance but if the effect is small then our
# findings aren't as impressive
#####################################################
### 2. T-tests on wide format
#####################################################

   
##  In exercise 1, we have worked on long-format data, where each line represents one observation.
##  In the context of t-tests, we may also encounter data sets in a wide format 
##  (this is the format we have been using in class examples.)
##  Let's look at the same dataset as in 1., but at a different variable, namely the morphological
##  complexity (Complex) of the target word. 

## a. Again, summarize the dataset to obtain the mean RT by "Subject" and "Complex" and transform 
##  the dataset to a wide format. 
##  In addition to group_by() and summarize(), you will need the function spread(). 
##  Assign the result to wide
wide <- lex %>% 
  group_by(Subject, Complex) %>% 
  summarize(m = mean(RT)) %>% 
  spread(Complex, m)
## b. Compute a t-test on the wide format data - note that for wide-format 
##  data you need to use a different syntax inside t.test()
t.test(x=wide$complex, y=wide$simplex, paired=TRUE)
## c. What do you conclude from this?
# p-value of > 0.05 denotes that the subjects showed no significant difference in RTs
# between simplex and complex words

#####################################################
### 3. Another T-test
#####################################################


## a. Now let's look at yet another question, namely whether the Sex of the participant 
##  influences their reaction time. Check out the variable Sex. Can you use a t-test to pursue this 
##  question and which type of t-test would you use? 

# Sex is a categorical variable so it's suitable to perform a t.test with RT. we would use
# a normal t-test since the observations are independent

## b. Use again group_by and summarize to obtain by subject means of RT, but
## this time with regard to Sex and assign it to bySubjSex
## Perform the t-test you decided for.
bySubjSex <- lex %>%
  group_by(Subject, Sex) %>%
  summarize(m = mean(RT))
t.test(m ~ Sex, data = bySubjSex)
## c. What do you conclude?

# We conclude that the means are very close and there isn't a statistical significance
# between the male and female subjects in terms of RT

## d. Choose an appropriate plot to visualize the result
ggplot(bySubjSex, aes(x = Sex, y = m)) +
  geom_boxplot()
#############################################
### 4. T-Tests for different sample sizes
#############################################

## In this exercise we will again use simulation to explore the independent samples t-test 
## with different samples. 
## We will take a similar example as discussed in the lecture. A class has two tutors, and we want 
## to find out which tutor is better by comparing the performance of the students in the final 
## exam by tutor group. First set a seed to make sure your results can be reproduced

set.seed(9273)
## a. Generate 10 random samples from a normal distribution with mean 20 and sd 8 and save it in a variable 
##  called "tutor1_grades"
tutor1_grades <- rnorm(10, 20, 8)

## b. Now we generate our second sample of size 10 ("tutor2_grades), this time for tutor 2 
##  and with mean 28 and sd 10
tutor2_grades <- rnorm(10, 28, 10)
## c. Combine the two samples and store the result into one vector called "score" (it should 
##    first show all scores from tutor1 followed by the scores of tutor2)
score <- c(tutor1_grades, tutor2_grades)
## d. Create a vector called tutor indicating which tutor the score belongs to: it should show 
##   "tutor1" 10 times followed by "tutor2" 10 times
tutor <- factor(c(rep('tutor1',10), rep('tutor2',10)))
## e. Create a data frame named "data_frame" having 2 columns "tutor", "score" created above.
data_frame <- data.frame(tutor = tutor, score = score)
## f. run the independent samples TTest (independentSamplesTTest) and formulate the findings as discussed 
###  in the lecture. 
independentSamplesTTest(score ~ tutor, data=data_frame)
# t(15.897)=-1.996, p = 0.063, CI95 = [-14.605, 0.442], d = 0.893

## Time to play around!

## g. Repeat the whole experiment you performed above with different sample size 
##  (the number of samples drawn from each tutor group). How big does your sample need to be in order
##  for the t test to be significant when keeping mean and sd constant?
## make sure to set the seed again before you run your code to be able to reproduce results
set.seed(9273)
gp = c()
gseq = seq(4,50)
for (i in gseq) {
  tutor1_grades <- rnorm(i, 20, 8)
  tutor2_grades <- rnorm(i, 28, 10)
  score <- c(tutor1_grades, tutor2_grades)
  tutor <- factor(c(rep('tutor1',i), rep('tutor2',i)))
  data_frame <- data.frame(tutor = tutor, score = score)
  t <- independentSamplesTTest(score ~ tutor, data=data_frame)
  gp <- append(gp, t$p.value)
}
plot(gseq, gp)
# We need a sample size of > 26, given the m and sd we chose

## h.	repeat the whole experiment you performed in a-f with different means.
##   What do you find? When is the test more likely to come out significant?
set.seed(9273)
hp = c()
hseq = seq(0, 40)
for (j in hseq) {
  tutor1_grades <- rnorm(10, 20, 8)
  tutor2_grades <- rnorm(10, j, 10)
  score <- c(tutor1_grades, tutor2_grades)
  tutor <- factor(c(rep('tutor1', 10), rep('tutor2', 10)))
  data_frame <- data.frame(tutor = tutor, score = score)
  t <- independentSamplesTTest(score ~ tutor, data=data_frame)
  hp <- append(hp, t$p.value)
}
plot(hseq, hp)
# The test is more likely to come out significant when the means are far apart
# Approximately, when one mean is outside the other mean's "mean ± pooled sd" zone

## i.	Now, vary the standard deviation, keeping means and sample size constant!
##   What do you find? When is the test more likely to come out significant?
set.seed(9273)
ip=c()
iseq = seq(1, 30, 0.5)
for (k in iseq) {
  tutor1_grades <- rnorm(10, 20, k)
  tutor2_grades <- rnorm(10, 28, k)
  score <- c(tutor1_grades, tutor2_grades)
  tutor <- factor(c(rep('tutor1', 10), rep('tutor2',10)))
  data_frame <- data.frame(tutor = tutor, score = score)
  t <- independentSamplesTTest(score ~ tutor, data=data_frame, var.equal=TRUE)
  ip <- append(ip, t$p.value)
}
plot(iseq, ip)
# The test is more likely to come out significant when the standard deviations
# are small compared to the mean
