### Stats with R Exercise sheet 5

##########################
#Correlation and Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December X. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms
## Please use the ggplot2 library for all graphs in this homework.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Mhd Jawad Al Rahwanji
## Matriculation number: 7038980
## Name: Ali Salaheldin Ali Ahmed
## Matriculation number: 7043295
## Name: Muhammed Saeed
## Matriculation number: 7030400

###########################################################################################
###########################################################################################

library(languageR)
library(ggplot2)
library(dplyr)
library(carData)
#######################
### Exercise 1: Correlation
#######################

## We will use the dataset UN98 from the package carData. 
## a) Load the package and inspect the data set
str(UN98)
UN98
## b) create the dataset AsiaMale, containing the variables educationMale lifeMale GDPperCapita 
##    economicActivityMale and illiteracyMale and the subset of Asian countries.
AsiaMale <- UN98 %>%
  filter(region == "Asia") %>%
  select(c("educationMale", "lifeMale", "GDPperCapita", "economicActivityMale", "illiteracyMale"))
## c) Let's say you're interested in whether there is a linear relationship between 
## illiteracy percentage and life expectancy of males in the different countries. 
## Take a look at the relationship between the two variables by 
## means of a scatterplot (use the ggplot library for this).
ggplot(AsiaMale, aes(illiteracyMale, lifeMale)) + geom_point()
## d) Judging from the graph, do you think that the two variables are 
## in any way correlated with one another?

# Yes, we can see a negative correlation.

## e) Get the correlations between all variables in the data set using cor().
## Tell R to only include complete pairs of observations. 
cor(AsiaMale, use="pairwise.complete.obs")
## f) Concentrate on the row for the life expectancy in males. Interpret the five numbers you see there
##   explaining for each number which direction the correlation takes and how strong it is.

# The correlation between life expectancy and:
# - education: is low and positive
# - life expectancy: naturally very high and positive
# - GDP per capita: moderate and positive
# - economic activity: negligible and negative
# - illiteracy: moderate and negative

## g) Is the correlation between life expectancy and GDPperCapita significant? Use cor.test()
cor.test(AsiaMale$lifeMale, AsiaMale$GDPperCapita) # cor = 0.6171201
# It is significant with: t(44)= 5.2023, p-value = 4.93e-06, CI95 = [0.3981501, 0.7695536]

## h) Calculate the Spearman rank correlation between life expectancy and GDPperCapita and compare
## it to the pearson correlation calculated above.
cor.test(AsiaMale$lifeMale, AsiaMale$GDPperCapita, method="spearman") # rho = 0.676135
# It is slightly higher

## i) make a scatterplot of this relationship.
ggplot(AsiaMale, aes(GDPperCapita, lifeMale)) + geom_point()
## j) Looking at the graph, why do you think Spearman's rho is better suited than the Pearson 
## correlation to describe the relationship between the two variables?

# Because the data is non-linear (logarithmic) and hence we should use Spearmna's
# rank correlation

## k) Using the function paired.r from the package psych, compare the correlations between life expectancy 
##  and economic activity on the one hand, and life expectancy and illiteracy on the other hand.
##  Hint: the degrees of freedom in a correlation test are equal to N-2
install.packages("psych")
library(psych)
paired.r(cor(AsiaMale$lifeMale, AsiaMale$economicActivityMale, use="pairwise.complete.obs"), 
         cor(AsiaMale$lifeMale, AsiaMale$illiteracyMale, use="pairwise.complete.obs"),
         cor(AsiaMale$economicActivityMale, AsiaMale$illiteracyMale, use="pairwise.complete.obs"),
         n=nrow(AsiaMale)-2)
## l) What do you conclude from k?

# t = 3.67  With probability =  0, from that we conclude that they are statistically 
# significantly different

## m) What would be the result, if the two variables would be independent?

# z = 2.67  With probability =  0.01, they would still be statistically significantly 
# different but with less certainty perhaps

################################
### Exercise 2: Regression
################################


## We will use the same dataset as above, but first scale the GDP to be in the unit of
## thousand dollars
AsiaMale$GDPt = AsiaMale$GDPperCapita/1000

## a) Run a regression model of life expectancy by GDPt and look at the summary.
## General form: 
## "modelname <- lm(outcome ~ predictor, data = dataFrame)"
## "summary(modelname)"
rm <- lm(lifeMale ~ GDPt, data=AsiaMale)
summary(rm)
## b) Interpret the model from a. What do intercept and the coefficient of GDPt tell you?

# The intercept tells us that a country with 0 GDPt (reliant on international support perhaps)
# has a male life expectancy of 62.65268 years.
# The coefficient of GDPt tells us that for each unit increase in GDPt, life expectancy
# increases by 0.49902 years

## c) What about the model fit: What proportion of the total variance is explained by your model?

# Adjusted R-squared:  0.3668, only 36.68% of the variance was explained.

## d) Now let's turn to the relationship between life expectancy and illiteracy.  Run the regression and 
# interpret.
rm2 <- lm(lifeMale ~ illiteracyMale, data=AsiaMale)
summary(rm2)
# The intercept tells us that a country with 0 illiterate men (everybody went to school)
# has a male life expectancy of 69.22702 years.
# The coefficient of illiteracyMale tells us that for each unit (1%) increase in illiteracyMale,
# life expectancy decreases by 0.24542 years
# Adjusted R-squared:  0.3361, only 33.61% of the variance was explained.

## e) Plot lifeMale by illiteracyMale and add a regression line to your plot
ggplot(AsiaMale, aes(illiteracyMale, lifeMale)) + geom_point()

###################################
### Exercise 3: Multiple Regression
###################################

## We will use the same data set as in 2. This time, we will look at the effect of illiteracy rate
## and GDP on life expectancy simultaneously. 

## a) Run a multiple regression model with illiteracyMale and GDPt as predictors
## General form: 
## "modelname <- lm(outcome ~ predictor1+predictor2+.., data = dataFrame, na.action = an action)"
## "summary(modelname)"
mrm <- lm(lifeMale ~ GDPt+illiteracyMale, data=AsiaMale)
summary(mrm)
## b) Interpret the model: what do intercept and the 2 coefficients tell you? What about significance?

# The intercept tells us that a country with 0 illiterate men and 0 GDPt
# has a male life expectancy of 65.88472 years.
# The coefficient of illiteracyMale tells us that for each unit (1%) increase in illiteracyMale,
# life expectancy decreases by 0.20627 years
# The coefficient of GDPt tells us that for each unit increase in GDPt, life expectancy
# increases by 0.53611 years
# Significance means that having each of the predictors is useful for the model's
# prediction compared to not including them
# Adjusted R-squared:  0.3361, only 33.61% of the variance was explained.

## c) Compare to the model in 2a (only including GDP), has the model fit improved? How about
## the model in 2d (only including illiteracy)?

# Yes, the model did improve as it now explains almost double the variance either of
# the ones with 1 predictor did.

## d) Look up the GDP and illiteracyMale for United.States and Brazil in the original data set (UN98)
imUS <- UN98['United.States','illiteracyMale']
imB <- UN98['Brazil','illiteracyMale']
gdpUS <- UN98['United.States','GDPperCapita']
gdpB <- UN98['Brazil','GDPperCapita']
leUS <- UN98['United.States','lifeMale']
leB <- UN98['Brazil','lifeMale']

## e) Using the model from 3a:  What is the predicted life expectancy for United.States and Brazil?
##  Calculate "by hand", i.e. do not use predict() and show your calculation. Don't forget to divide
##  the GDPperCapita by 1000 first!
us_LE <- 65.88472 + 0.53611*gdpUS/1000 - 0.20627*imUS
b_LE <- 65.88472 + 0.53611*gdpB/1000 - 0.20627*imB
# The predicted life expectancy for Brazil wasn't that far off 64.85 to the original 63.4
# As for the US, it overestimated by predicting 79.38 compared to the original 73.4

## f) Run an additional model of life expectancy for the AsiaMale data set including also economicActivityMale
mrm2 <- lm(lifeMale ~ GDPt+illiteracyMale+economicActivityMale, data=AsiaMale)
summary(mrm2)
## g) Do you think inclusion of economicActivity into the model is a good idea?

# No, the inclusion must have caused confusion because this way the model is 
# unable to explain as much variance in the data as mrm did
# Adjusted R-squared:  0.5854
