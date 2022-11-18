### Stats with R Exercise sheet 2

###############################################################
# Deriving sampling distribution and confidence intervals
###############################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 20th. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms
## Remember to answer each subquestion and test your code, before you submit.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Mhd Jawad Al Rahwanji
## Matriculation number: 7038980
## Name: Ali Salaheldin Ali Ahmed
## Matriculation number: 7043295
## Name: Muhammed Saeed
## Matriculation number: 7030400

## Only 1 member needs to submit! 

###############################################################
### Exercise 1: Deriving sampling distributions
###############################################################
## In this exercise, we're going to derive sampling distributions for the mean with 
## different sizes.

## a) We will not use data from a normal distribution, but work with the poisson distribution, which is 
## often used for count data. We will use the dataset discoveries, please find out what it is about
help("discoveries")
# Yearly Numbers of Important Discoveries:
# The numbers of “great” inventions and scientific discoveries in each year from 1860 to 1959.

## please run the following line to convert the data into numeric
discoveries = as.numeric(discoveries)

## b) Take a look at the dataset using the table() function and histogram and boxplot. 
table(discoveries)
hist(discoveries)
boxplot(discoveries)
## c) Compare mean and median of the dataset
m <- mean(discoveries)
med <- median(discoveries)
m > med # = TRUE
## d) Create a sample of a normal distribution of equal length and with the same mean and 
##   standard deviation as the observed sample, assign it to norm_disc
sd <- sd(discoveries)
norm_disc <- rnorm(length(discoveries), m, sd)
## e) Make a histogram of norm_disc and compare this to the histogram of the original dataset from b
hist(norm_disc)
# We think that the subquestion here is identical to (f)

## f) Describe the differences observed in e)

# The histogram of the original discoveries data is exponential in nature
# Whereas, the one of norm_disc follows a normal distribution (bell shaped)

## g) Now, we are going to draw a smaller sample from discoveries.
### Use the function sample() to create a sample of four instances from discoveries
### assign it to sample4
sample4 <- sample(discoveries, 4)
## h) draw another 2 samples of 4 called sample4b and sample4c
sample4b <- sample(discoveries, 4)
sample4c <- sample(discoveries, 4)
## i) calculate the mean for each of the three samples and store them in the vector means4
means4 <- unlist(lapply(c(list(sample4), list(sample4b), list(sample4c)), mean))
## j) Are the values different? Why?

# The values differ. That's because we have a new sample each time that's from a
# different seed and therefore we get a new set of values each time from the original
# population. Also, the sample size is small (=4) which in turn provides us with 
# a sample that isn't representative of the population. So we have more room for ERROR

## k)   In order to draw a distribution of such a sample, we want to calculate the
###   mean of 1000 samples, each of size 4. However, we don't want to repeat 
###   question h and i 1000 times. Use a for loop to draw 1000 samples of size 4
###   and store the mean of each sample in the vector means4.
for (i in seq(1000)) {
  means4[i] <- mean(sample(discoveries, 4))
}

## l) Repeat the for-loop in question k, but use a sample size of 40. 
##    Assign this to 'means40' instead of 'means4'.
means40 <- c()
for (i in seq(1000)) {
  means40[i] <- mean(sample(discoveries, 40))
}
## m) Explain in your own words what 'means4' and 'means40' now contain. 
##    How do they differ?

# They now contain estimates of the actual mean of the discoveries population
# They differ in the size of the samples they contain where 40 may be more 
# representative of the main population (discoveries) mean than the samples of size 4

## n) Draw histograms of means4 and means40. Describe in what way they differ
hist(means4)
hist(means40)
# The one with sample size = 40 has a lower standard error and resembles a normal
# distribution more than means4

## o) Why do you observe a skew for means4, but not for means40?

# Based on the central limit theorem, the larger the sample size the more accurate
# will our estimate be with higher certainty and the lower it gets the more it 
# will resemble the original distribution of the population. Since the original 
# was an exponential distribution therefore we perceive a positive skew

###############################################################
### Exercise 2: Confidence interval
###############################################################

## A confidence interval is a range of values that is likely to contain an 
## unknown population parameter.
## The population parameter is what we're trying to find out. 
## Navarro discusses this in more depth in chapter 10.

## a) What does a confidence interval mean from the perspective of experiment replication?

# In experiment replication our goal is to estimate the population mean. The confidence
# interval tells us where the real population mean would lie with the level of
# certainty given by the confidence level.

## b) please install and load packages sciplot and lsr
install.packages("sciplot")
install.packages("lsr")
require(sciplot)
require(lsr)
## c) calculate 95% Confidence Intervals for discoveries, sample4 and sample4c. You can
##    use the function ciMean()
ciMean(discoveries)
ciMean(sample4)
ciMean(sample4c)
## d) Why are these intervals so different?

# Based on the law of large numbers, the more data we have the more confident we
# are in the mean. That is exactly why discoveries has a small CI

## e) Is the true mean contained in each interval?

# Yes. The mean of 3.1 is contained within all intervals

## f) In the following, we will check whether the CI behaves as expected.
##   What is the true mean in our example?

m # = 3.1

## g) Change your for loop from above (subquestion 1l) to calculate the confidence interval 
##  instead of the mean for 1000 samples of size 15. Then check whether the confidence 
##  interval contains the true mean and save the result in a vector called TrueMeanContained.
##  Hint: You will need to compare the mean to the lower and the upper bound of the 
##  confidence interval
## ciMean(YOURSAMPLE)[1] gives you the lower bound and ciMean(YOURSAMPLE)[2] the upper bound
TrueMeanContained <- c()
for (i in seq(1000)) {
  ci <- ciMean(sample(discoveries, 15))
  TrueMeanContained[i] <- ci[1] <= m && m <= ci[2]
}

## h) Given your results in TrueMeanContained, you now need to check, whether the interval really contains
##   the mean 95% of the time. Does it?
sum(TrueMeanContained) / 1000
# No. It only contains it 92.7% of the time

## i) Confidence intervals are often used in plots. Lets construct a barplot with confidence intervals for
## the dataset chickwts, which contains weight of chickens after being administered different kinds of 
## food for 6 weeks.
## Use the function bargraph.CI to plot weight by feed, using the arguments response and x.factor
bargraph.CI(chickwts$feed, chickwts$weight)
## j) Now additionally set the optional argument ci.fun to ciMean. How does the graph change and why?
##  Hint: Look into the documentation of bargraph.CI.
bargraph.CI(chickwts$feed, chickwts$weight, ci.fun=ciMean)
# The confidence intervals become larger. After taking a look at the docs,
# the default implementation of bargraph.CI uses ±1 standard error from the mean (68% coverage)
# Whereas, ciMean defaults to a 95% coverage which is technically achieved using
# ±2 standard error from the mean
