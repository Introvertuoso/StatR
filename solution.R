### Stats with R Exercise sheet 1 

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 13. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms
## You are required to work together in groups of three students.
## You need to provide a serious attempt at solving each exercise in order to have
## the assignment graded as complete, please do not leave out subquestions!

## Please write below your (and your teammates') name and matriculation number. 
## Name: Mhd Jawad Al Rahwanji
## Matriculation number: 7038980
## Name: Ali Salaheldin Ali Ahmed
## Matriculation number: 7043295
## Name: Muhammed Saeed
## Matriculation number: 7030400


## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## cms discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.
getwd()
## b) Get help with this function.
?getwd
## c) Change your working directory to another directory.
setwd("/Users/introvertuoso/Desktop")
###############
### Exercise 2: Normal distribution plotting
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the range for which you want to plot the 
##    normal distribution (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.75. Assign this to the 
##    variable x.
?seq
x <- seq(-5, 5, 0.75)
## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution. Use the defaults for mean and sd (standard normal distribution)
help(dnorm)
y <- dnorm(x)
## c) Now use plot() to plot the normal distribution for z values of "x". Specify
## the type to be line using the suitable argument of plot()
plot(x, y, type='l')
## d) This plot does not look like a smooth normal distribution. Change the vector
##  x to have smaller increments and plot again (you also need to update y)
x <- seq(-5, 5, 0.25)
y <- dnorm(x)
plot(x, y, type='l')
## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the mean of x using
##    the argument 'v'.
##    In order to get a dotted line, set the argument 'lty' to 3.
help(abline)
abline(v = mean(x), lty=3)
## f) Take a look at the trees dataset (You can see it by typing "trees"), which 
##    has height, diameter and volume.
##    Then select only the Height part and store it in a variable "treesHeight".
trees
str(trees)
treesHeight <- trees$Height
## g) Calculate the mean and standard deviation of treesHeight and plot a normal
##    distribution with these parameters (NB:you should not use the same x range 
##    as above!)
m <- mean(treesHeight)
sd <- sd(treesHeight)
r <- seq(m-4*sd, m+4*sd, 0.5)
plot(r, dnorm(r, m, sd), type='l')
## h) We observe two additional tree height values (62 and 86). What's the 
##    likelihood that these heights (or more extreme ones) respectively 
##    come from the normal distribution from g)?
pnorm(62, m, sd)
pnorm(86, m, sd, lower.tail = FALSE)
## i) What do you conclude from the p-values? (informal)

# Using an alpha cutoff of 0.05, we can determine that those heights and 
# more extreme ones are significantly unlikely to occur

## j) Use the random sampling function in R to generate 25 random samples from
##    the normal distribution from g), and draw a histogram based on this sample.
##    Repeat 5 times. Set the range of the x-axis between 58 to 94 using xlim. 
##    Fix the number of breaks to 11 using breaks
sample <- rnorm(25, m, sd)
hist(sample)
for (i in seq(1, 5, 1)) {
  sample <- rnorm(25, m, sd)
  hist(sample, breaks = 11, xlim=c(58, 94))
}
## k) What do you observe in j?

# Each time we get a new seed therefore we have a new sample that's different
# from the one before. Since the sample size is small we don't get any plots that 
# resemble a normal distribution


###############
### Exercise 3: data exploration and more histograms
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.
install.packages(c('languageR', 'dplyr'))
require(languageR)
require(dplyr)
## b) Specifically, we will deal with the dataset 'lexdec'. 
##    This dataset should be available to you once you've loaded languageR.
##    Find out what experiment the data comes from

# Lexical decision latencies for 79 English nouns:
# Lexical decision latencies elicited from 21 subjects for 79 English
# concrete nouns, with variables linked to subject or word.


## c) Inspect 'lexdec'. Look at the head, tail, 
##    and summary. 
head(lexdec)
tail(lexdec)
summary(lexdec)
## d) What do head and tail show you?

# head() shows the first 6 rows in the dataset
# tail() shows the last 6 rows in the dataset

## e) Look at the first 15 rows of the data.frame
head(lexdec, 15)
## f) The file contains multiple observations for each participant. Create a 
##   subset only including subject number M2 and assign it to M2
##   How many observations are there for this participant, i.e. how many rows 
##   are in your subset?
M2 <- filter(lexdec, Subject == 'M2')
nrow(M2)
## g) looking at the summary of M2, what can you find out about the demographic 
##    parameters of this participant?
summary(M2)
# We can infer that she is a non-native English speaking female

## h) Create a histogram (using hist()) of "RT" (logarithm of reaction time) 
##    for M2
hist(M2$RT)

## i) Create a kernel density plot for this data using density()
d <- density(M2$RT)
plot(d[[1]], d[[2]], type='l')
## j) What is the difference between the two?

# The histogram is discrete, relies on binning for resolution, whereas, the kernel
# density plot is continuous and smooth
# Additionally, the histogram y-axis corresponds to the frequency at each bin value
# and the density y-axis is normalized such that the area under the curve amounts to
# 1

## k) Is this data likely from a normal distribution? How would you check ?
##    (describe in words, remember to comment out text)

# Yes it seems to be from a normal distribution.

## l) Looking at the graph, do you think the data is skewed? In which direction?

# It is positively skewed

#############################################
### Exercise 4: Dataframes and boxplots
#############################################
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 26 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18	15	18	19	23	17	18	24	17	14	16	16	17	21	22	18	20	21	20	20	
# 16	17	17	18	20	26


## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why.

# The count is a discrete variable and the scale of which is the ratio scale

## b) The researcher is also interested in whether story telling is related to 
##    their reading habits. As a proxy, she asked the children, whether they have 
##    a library card. The following line codes that the first 13 observations are
##    from children with library card (Y) and the remaining 13 from children 
##    without (N). What measurements scale does this variable have?
lib = c(rep("Y",13),rep("N",13))
# This variable is discrete and has the nominal (binary) scale

## c) You will now create a dataframe of this data. Start by creating a vector 
##    with participant IDs. Your vector should be named 'pid', and your 
##    participants should be labeled from 1 to 26
pid <- seq(1, 26, 1)
## d) Next, create a vector containing all the observations. Name this vector 'obs'.
obs <- c(18, 15, 18, 19, 23, 17, 18, 24, 17, 14, 16, 16, 17, 21, 22, 18, 20, 21,
         20, 20,  16, 17, 17, 18, 20, 26)
## e) Create a dataframe including pid, obs and lib. Assign this to 'stories'. 
stories <- data.frame(pid = pid, obs = obs, lib = lib)
## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class are the variable 'pid' and 'lib'?
summary(stories)
# pid: numeric
# lib: character

## g) Change the class of 'pid' and 'lib' to factor. Why is factor a better
##     class for these variables? (answer for both separately)
stories$pid <- factor(stories$pid)
stories$lib <- factor(stories$lib)
# We use Factors for categorical variables, which suits our variables
# Dealing with numbers is easier than dealing with characters

## h) Create a boxplot of obs for the two lib groups
boxplot(obs ~ lib, data = stories)
## i) Are there outliers in one of the lib groups?

# There are outliers in both lib groups

## j) Which group shows the larger interquartile range? 

# The 'N' lib group exhibits a larger ICR

## k) Which one has the greater overall range?

# They have the same range

## l) What is a whisker? Why is the upper whisker of group "Y" so short?

# A whisker is the difference between the 4th and 3rd quartile (upper whisker) 
# or the difference between the 2nd and the 1st quartile (lower whisker) in a 
# boxplot
# We attribute it to the negative skew of group Y

## m) Compare the median of group Y with the mean - which one is plotted in your
##    boxplot? Why are they different?

# The mean is greater than the median. The median is plotted as it is the 2nd 
# quartile (50% quantile). Group Y is negatively skewed therefore the median is 
# different from the mean.