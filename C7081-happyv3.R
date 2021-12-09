## HEADER ####
## Who: Sophie Constant
## https://dsgarage.netlify.app/
## What: C7081 - Assignment 1 - What influences happiness?
## Last edited: <DATE TODAY in 2021-12-09 format)


## CONTENTS ####
# 00 Setup
# 01 dataframes
# 02 Question
# 03 EDA
# 04 Correlation plots 
# 05 Assumption testing for Multiple linear regression
# 06 Log transformed variables and assumption testing for Multiple linear regression 
# 07 Cube root transformed variables (EDA)
# 08 Square root transformed variable
# 09 Square transformed variable
# 10 multiple Linear Regression 
# 11 Possible ANOVA because post hoc test/effect attribution
# 12 Variable Selection (choice depending on p value from multiple linear regression) because why not? 
# 13 Evaluation of Variable selection (possibly root mean square? Partial least squares)
# 14 Some kind of tree


# 00 Setup ####

setwd("C:/Users/Sophi/OneDrive/Documents/Data Science/Module 1/Assignment 1.2")


install.packages("gclus")
install.packages("PerformanceAnalytics")
install.packages("psych")
install.packages("leaps")

library(openxlsx)
library(dplyr)
library(car)
library(gclus)
library(PerformanceAnalytics)
library(psych)
library(MASS)
library(leaps)
library(splines)
library(wesanderson)

names(wes_palettes)

happy_19 <- read.csv("2019.csv")

summary(happy_19)

happy_19 <- as.data.frame(lapply(happy_19, as.numeric))


# 01 Dataframes and arguments ####

# Dataframes

# All the data for 20119

happy_19 <- read.csv("2019.csv")

summary(happy_19)

# Scores, and independant variables from 2019

happy_192 <- happy_19[3:9]

happy_192 <- happy_192 <- as.data.frame(lapply(happy_19, as.numeric))

head(happy_192)


# Including log transformed variables (Here for reference)

happy_192_new <- happy_192

happy_192_new[,c(5,7)] <- log(happy_192_new[,c(5,7)])

# Squared variables



# Exclusion of the data that does not have Gaussian or homoscedastatic residuals

happy_19_ex <- happy_192[ , c(1,2,4,6)]

head(happy_19_ex)

# Arguments

# All the linear regression arguments

GDP_lm <- lm(formula = happy_192$Score ~ happy_192$GDP.per.capita)

SS_lm <- lm(formula = happy_192$Score ~ happy_192$Social.support)

HLE_lm <- lm(formula = happy_192$Score ~ happy_192$Healthy.life.expectancy)

FLC_lm <- lm(formula = happy_192$Score ~ happy_192$Freedom.to.make.life.choices)

G_lm <- lm(formula = happy_192$Score ~ happy_192$Generosity)

PC_lm <- lm(formula = happy_192$Score ~ happy_192$Perceptions.of.corruption)


# 02 Question and Exploration ####

# What predictors can be attributed as having the effect in the happiest scores and least happy scores 

# Hypothesis: One or more of the predictors will have a significant effect on the higher happiness scores
# Null Hypothesis: One or more of the predictors will have no effect on the higher happiness scores

# 03 EDA ####

# Including: 
# Histograms with mean lines exploring the data 
# Means, standard deviation, standard error (tried Aggregate function, but got NAs)
# The above being as scores dependant on 
  #*GDP per capita, 
  #*social support, 
  #*healthy life expectancy, 
  #*freedom to make life coices, 
  #*generosity, 
  #*perceptions of corruption, 

# Testing that variables are Gaussian 
  #* q-q plots
  #* Shapiro-wilk
# Testing for Gaussian residuals
# Testing for Homoscedasticty

plot(happy_19)

par(mfrow = c(1,3))

hist(happy_19$Overall.rank)

# Score, dependant variable 
# Not far off gaussian, with a slight right skew. 
# High frequency (over 25) of scores between 6 and 6.5,  
# Considerable drop off above 6.5, being under 10, 7 being under 15, and 8 being under 5
# scored of 4-5 are th emost common, frequency being 20 - under 25
# 3 to 4  
# Mean is 5.407096, reasonably central
# SD is 1.1, so a bit high
# Standard error is 0.09, pretty small

hist(happy_19$Score,
     main = "Score",
     xlab = "Cantril Ladder scores")

abline(v = mean(happy_19$Score), col = "orchid", lty = 2, lwd = 2)

curve(dnorm(x, mean = mean(happy_19$Score),
            sd = sd(happy_19$Score)),
      add = T,
      col = "seagreen", lty = 3, lwd = 3)

mean(happy_19$Score)
sd(happy_19$Score, na.rm=TRUE)
sd(happy_19$Score)/sqrt(length(happy_19$Score))

is.numeric(happy_19$Score)


# GDP per capita, Predictor
# Skewed to th eleft
# sharp drop in 1.5 and over

hist(happy_19$GDP.per.capita,
     main = "GDP per capita",
     xlab = "GDP per capita")

abline(v = mean(happy_19$GDP.per.capita), col = "orchid", lty = 2, lwd = 2)

aggregate(x = happy_19$Score, by = list(GDP.per.capita = happy_19$GDP.per.capita), 
          FUN = function(x){ c(mean = mean(x), 
                               sd = sd(x), na.rm=TRUE,  
                               SEM = sd(x)/sqrt(length(x)))})

mean(happy_19$GDP.per.capita)
sd(happy_19$GDP.per.capita, na.rm=TRUE)
sd(happy_19$GDP.per.capita)/sqrt(length(happy_19$GDP.per.capita))


# social support, independant variable
# Sharp skew to left
# 1.5 occuring over 50 times
# mean is 1.2
# sd is 0.3 - low
# SEM is 0.02 which is low

hist(happy_19$Social.support, 
     main = "Social Support",
     xlab = "Scores for social support")

abline(v = mean(happy_19$Social.support), col = "orchid", lty = 2, lwd = 2)

mean(happy_19$Social.support)
sd(happy_19$Social.support, na.rm=TRUE)
sd(happy_19$Social.support)/sqrt(length(happy_19$Social.support))

# Life expectancy, independant variable
# Skewed to the left with expectancies being around 80
# mean is 0.7
# sd is 0.2 - low
# SEM 0.02 which is low

hist(happy_19$Healthy.life.expectancy,  
     main = "Healthy life expectancy",
     xlab = "Healthy Life expectancy")

abline(v = mean(happy_19$Healthy.life.expectancy), col = "orchid", lty = 2, lwd = 2)

mean(happy_19$Healthy.life.expectancy)
sd(happy_19$Healthy.life.expectancy, na.rm=TRUE)
sd(happy_19$Healthy.life.expectancy)/sqrt(length(happy_19$Healthy.life.expectancy))

# Freedom to make life choices, independant variable
# slight skew to the left, though likely Gaussian
# mean 0.4
# sd 0.14 - low
# SEM is 0.011 - very low

hist(happy_19$Freedom.to.make.life.choices,  
     main = "Freedom to make life choices",
     xlab = "Freedom to make life choices")

abline(v = mean(happy_19$Freedom.to.make.life.choices), col = "orchid", lty = 2, lwd = 2)

mean(happy_19$Freedom.to.make.life.choices)
sd(happy_19$Freedom.to.make.life.choices, na.rm=TRUE)
sd(happy_19$Freedom.to.make.life.choices)/sqrt(length(happy_19$Freedom.to.make.life.choices))


# Generosity, independant variable
# Interesting skew to right
# Clustered around 1.5
# mean is 0.18
# sd is 0.1 - low
# SEM is 0.007 - very low

hist(happy_19$Generosity,  
     main = "Generosity",
     xlab = "Generosity scores")

abline(v = mean(happy_19$Generosity), col = "orchid", lty = 2, lwd = 2)

mean(happy_19$Generosity)
sd(happy_19$Generosity, na.rm=TRUE)
sd(happy_19$Generosity)/sqrt(length(happy_19$Generosity))


# Perception of corruption
# Heavily skewed to right
# Indicated comparativly low frequency of perceptions of corruption
# mean is 0.1
# sd is 0.094 very low
# SEM 0.008 very low

hist(happy_19$Perceptions.of.corruption,  
     main = "Perception of corruption",
     xlab = "Perception of corruption")

abline(v = mean(happy_19$Perceptions.of.corruption), col = "orchid", lty = 2, lwd = 2)

mean(happy_19$Perceptions.of.corruption)
sd(happy_19$Perceptions.of.corruption, na.rm=TRUE)
sd(happy_19$Perceptions.of.corruption)/sqrt(length(happy_19$Perceptions.of.corruption))


# qq-plots and Shapiro Wilk testing of variables for Gaussian

# Score, dependant variable
# NH - There is no difference between the distribution of Scores and Gaussian
# H - There is a difference between the distribution of Scores and gaussian
# p-value is 0.1633, comfortable
# it's Gaussian!

qqPlot(x = happy_19$Score, 
       dist = "norm", 
       main = "Is score gaussian?")

shapiro.test(happy_19$Score)

# GDP per capita, Predictor
# NH - There is no difference between the distribution of GDP per capita and Gaussian
# H - There is a difference between the distribution of GDP per capita and gaussian
# p-value = 0.0005821
#it's not Gaussian! 

qqPlot(x = happy_19$GDP.per.capita, 
       dist = "norm", 
       main = "Is gdp per capital gaussian?")

shapiro.test(happy_19$GDP.per.capita)

# social support, independant variable
# NH - There is no difference between the distribution of social support and Gaussian
# H - There is a difference between the distribution of social support and gaussian
# p-value = 2.158e-08
#it's not Gaussian? 

qqPlot(x = happy_19$Social.support, 
       dist = "norm", 
       main = "Is Social support gaussian?")

shapiro.test(happy_19$Social.support)

# Life expectancy, independant variable
# NH - There is no difference between the distribution of Life Expectancy and Gaussian
# H - There is a difference between the distribution of Life expectancy and gaussian
# p-value = 4.498e-05
#it's not Gaussian? 

qqPlot(x = happy_19$Healthy.life.expectancy, 
       dist = "norm", 
       main = "Is Life expectancy gaussian?")

shapiro.test(happy_19$Healthy.life.expectancy)

# Freedom to make life choices, independant variable
# NH - There is no difference between the distribution of Freedom to make life choices and Gaussian
# H - There is a difference between the distribution of Freedom to make life choices and gaussian
# p-value = 5.386e-05
#it's not Gaussian? 

qqPlot(x = happy_19$Freedom.to.make.life.choices, 
       dist = "norm", 
       main = "Is Freedom to make life choices support gaussian?")

shapiro.test(happy_19$Freedom.to.make.life.choices)

# Generosity, independant variable
# NH - There is no difference between the distribution of Life Expectancy and Gaussian
# H - There is a difference between the distribution of Life expectancy and gaussian
# p-value = 0.0004274
#it's not Gaussian? 

qqPlot(x = happy_19$Generosity, 
       dist = "norm", 
       main = "Is Generosity gaussian?")

shapiro.test(happy_19$Generosity)

# Perceptions of corruption, independant variable
# NH - There is no difference between the distribution of Life Expectancy and Gaussian
# H - There is a difference between the distribution of Life expectancy and gaussian
# p-value = 1.813e-12
#it's not Gaussian? 

qqPlot(x = happy_19$Perceptions.of.corruption, 
       dist = "norm", 
       main = "Are perceptions of corruption Gaussian?")

shapiro.test(happy_19$Perceptions.of.corruption)

# Pairs Plots

par(mfrow = c(1,2))

plot(happy_192,
     col = happy_192$Score)

lapply(seq(happy_192), function(x)
  plot(x = happy_192[[x]], xlab = names(happy_192)[x], col = "orchid", main=paste("Scatterplot", names(happy_192)[x])))

pairs(happy_192[ , 1:7],
     col = happy_192$Score)

# 04 Correlation Exploration #### 
# if no stars, the variable is not statistically significant, 
# one stars, corresponding variable is significant at 10%, 
# two stars, corresponding variable is significant at 5%
# three stars, corresponding variable is significant at 1% 
# Spearman's rank chosen due to majority of variables not being Gaussian

# Correlation matrix for all the independant and dependant variables

pairs.panels(happy_192,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             hist.col = 5,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# Correlation matrix for the dependant variable (score) and first four independant variables

pairs.panels(happy_192[ , 1:5],
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             hist.col = 5,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

cor(happy_192$Score, happy_192$GDP.per.capita, method = "spearman")

# Score seems very positively correlated with GDP per capita, Social support, and Healthy life expectancy
# 0.81, 0.82, and 0.81 respectively
# Score is also very correlated with freedom to make life choices at 0.55
# Score is a bit correlated with perceptions of corruption at 0.22 but this is weaker than the others
# Generosity was surprisingly not correlated with score
# Interesting to note that Generosity only correlated with Freedom to make life choices - may be worth reviewing
# In terms of correlations between independent variables -  
# GDP per capita, Social support and Life expectancy were all highly correlated
# GDP was a bit correlated with perception of corruption
# GDP, social support and life expectancy were all very correlated with freedom to make choices but not as much
# Perceptions of corruption were correlated with Generosity (0.29) and freedom to make life choices (0.40)
# Perceptions of corruption was also a bit correlated with life expectancy and GDP
# Confidence intervals were quite large in all the correlations involving freedom to make life choices
# Confidence intervals were also quite large in healthy life expectancy against social support


# Having ascertained some correlations, moving onto Multivariate Linear regression
# To explore the relationship, between the dependent variable and most significantly correlated independant variables
# To explore any possible effect they may have together, and test for possible interdependancies

# 05 Assumption testing for Multiple linear regression (EDA) ####

# Assumption testing
  #* Gaussian Residuals
  #* Homoscedasticity 


# Score, y, and GDP per capita, x
# Frequency Histogram is fairly bell shaped with an outlier in the -2 bin
# Frequency Histogram has a right skew because of the outlier
# Q-Q plot, two outliers 12 and 148, both smaller than expected observations
# Density Histogram has lower than expected residuals than the theoretical Gaussian at the top of the curve
# Other than that is actually fairly close to the Theoretical Gaussian curve
# There's one bin expected in Gaussian that's 'missing' in residuals
# Other than this residuals seems fairly symmetrical around mean

plot(y = happy_192$Score, x = happy_192$GDP.per.capita,
     ylab = "Score", xlab = "GDP per capita",
     main = "Happines residuals (Score and GDP per Captita)",
     pch = 20, col = "orchid", cex = 1)

GDP_lm <- lm(formula = happy_192$Score ~ happy_192$GDP.per.capita)

abline(reg = GDP_lm)

text(x = 0.3, y = 7, labels = "y = 3.339, x = 2.218") 

arrows(x0 = happy_192$GDP.per.capita,
       x1 = happy_192$GDP.per.capita,
       y0 = predict(GDP_lm),
       y1 = predict(GDP_lm) + residuals(GDP_lm),
       length = 0)

par(mfrow = c(1,2))

# Frequency Histogram and q-q plot for residuals

hist(residuals(GDP_lm), main = "")
qqPlot(residuals(GDP_lm))

par(mfrow = c(1,1))

# Density Histogram, with theoretical Gaussian for residuals

hist(residuals(GDP_lm), 
     xlim = c(-2, 2), ylim = c(0,.9),
     main = "",
     prob = T)

lines(density(residuals(GDP_lm)),   
  col = "seagreen3", lty = 1, lwd = 3) 

x <- seq(-1,+1,by=0.02) 

curve(dnorm(x, mean = mean(residuals(GDP_lm)),
            sd = sd(residuals(GDP_lm))),
      add = T,
      col = "purple2", lty = 3, lwd = 3)

abline(v = 0,
       freq = F,
       col = "darkblue", lty = 2, lwd = 3)

legend(x = .6, y = .8,
       legend = c("Residuals", "Gaussian", "Mean"),
       lty = c(1,3,2),
       col = c("seagreen3", "purple2","darkblue"), lwd = c(3,3,3))

# Statistical Test of Gaussian Residuals
# NH: There's no difference between the distrubution of the residuals and Gaussian
# H: Thsi is a difference between the distribution of the residuals and Gaussian

shapiro.test(residuals(GDP_lm))

# Having conducted the  Shapiro wilk test on the residuals for GDP per capita, 
# the p-value given is  0.4201, indicating no difference between the distribution
# of the residuals and a Gaussian distribution


# Score, y, Social Support x
# From the plot, there seems to be a bit of variation in residuals
# Frequency histogram seems skewed to the left, with the bins generally larger on right
# q-q plot has a lot of points in the middle
# the q-q plot also has two named outliers, 151 and 148
# there's a little cluster off at the top of the q-q plot
# In the density histogram, the residuals on the left (less than 0) follow expected Gaussian closely
# The peak of the residuals is fairly close to the expected gaussian
# On the right hand side the residuals deviate and little more from expected gaussian but not wildly
# The bin just below 0 is a lot larger than those farther left. 


plot(y = happy_192$Score, x = happy_192$Social.support,
     ylab = "Score", xlab = "Social Support",
     main = "Happines residuals (Score and social support)",
     pch = 20, col = "orchid", cex = 1)

SS_lm <- lm(formula = happy_192$Score ~ happy_192$Social.support)

SS_lm

abline(reg = SS_lm)

text(x = 0.3, y = 7, labels = "y = 1.912, x = 2.891") 

arrows(x0 = happy_192$Social.support,
       x1 = happy_192$Social.support,
       y0 = predict(SS_lm),
       y1 = predict(SS_lm) + residuals(SS_lm),
       length = 0)

par(mfrow = c(1,2))

# Frequency Histogram and q-q plot for residuals

hist(residuals(SS_lm), main = "")
qqPlot(residuals(SS_lm))

par(mfrow = c(1,1))

# Density Histogram, with theoretical Gaussian for residuals

hist(residuals(SS_lm), 
     xlim = c(-2, 2), ylim = c(0,.9),
     main = "",
     prob = T)

lines(density(residuals(SS_lm)),   
      col = "seagreen3", lty = 1, lwd = 3) 

x <- seq(-1,+1,by=0.02) 

curve(dnorm(x, mean = mean(residuals(SS_lm)),
            sd = sd(residuals(SS_lm))),
      add = T,
      col = "purple2", lty = 3, lwd = 3)

abline(v = 0,
       freq = F,
       col = "darkblue", lty = 2, lwd = 3)

legend(x = .6, y = .8,
       legend = c("Residuals", "Gaussian", "Mean"),
       lty = c(1,3,2),
       col = c("seagreen3", "purple2","darkblue"), lwd = c(3,3,3))


# Statistical Test of Gaussian Residuals
# NH: There's no difference between the distrubution of the residuals and Gaussian
# H: Thsi is a difference between the distribution of the residuals and Gaussian

shapiro.test(residuals(SS_lm))

# Having conducted the  Shapiro wilk test on the residuals for Social support, 
# the p-value given is  0.4201, indicating no difference between the distribution
# of the residuals and a Gaussian distribution


# Score, y, healthy life expectancy, x
# There's a bit of a dip in the middle of the plot 
# The frequency histogram s slightly skewed to the left with one bin having a lot
# The q-q- plot has a lot of data points in the middle
# the q-q plot has labled the outliers 85 as low, and 152 as high
# there's a little skew and the top and bottom, but it's not huge
# The density histogram is also a little skewed to the left
# In the density histogram, the curve for our residuals wasn't hugely flush with 
# the expected gaussian leading up to and a little beyond the mean
# the second half of the residual curve follows the gaussian better
# The peak of the residual curve is a little less, a little narrower 
# and more skewed than expected Gaussian

plot(y = happy_192$Score, x = happy_192$Healthy.life.expectancy,
     ylab = "Score", xlab = "Healthy life expectancy",
     main = "Happiness residuals (Score and Healthy life expectancy)",
     pch = 20, col = "orchid", cex = 1)

HLE_lm <- lm(formula = happy_192$Score ~ happy_192$Healthy.life.expectancy)

HLE_lm

abline(reg = HLE_lm)

text(x = 0.2, y = 7, labels = "y = 2.807, x = 3.585") 

arrows(x0 = happy_192$Healthy.life.expectancy,
       x1 = happy_192$Healthy.life.expectancy,
       y0 = predict(HLE_lm),
       y1 = predict(HLE_lm) + residuals(HLE_lm),
       length = 0)

par(mfrow = c(1,2))

# Frequency Histogram and q-q plot for residuals

hist(residuals(HLE_lm), main = "")
qqPlot(residuals(HLE_lm))

par(mfrow = c(1,1))

# Density Histogram, with theoretical Gaussian for residuals

hist(residuals(HLE_lm), 
     xlim = c(-2, 2), ylim = c(0,.9),
     main = "",
     prob = T)

lines(density(residuals(HLE_lm)),   
      col = "seagreen3", lty = 1, lwd = 3) 

x <- seq(-1,+1,by=0.02) 

curve(dnorm(x, mean = mean(residuals(HLE_lm)),
            sd = sd(residuals(HLE_lm))),
      add = T,
      col = "purple2", lty = 3, lwd = 3)

abline(v = 0,
       freq = F,
       col = "darkblue", lty = 2, lwd = 3)

legend(x = .6, y = .8,
       legend = c("Residuals", "Gaussian", "Mean"),
       lty = c(1,3,2),
       col = c("seagreen3", "purple2","darkblue"), lwd = c(3,3,3))


# Statistical Test of Gaussian Residuals
# NH: There's no difference between the distrubution of the residuals and Gaussian
# H: There is a difference between the distribution of the residuals and Gaussian

shapiro.test(residuals(HLE_lm))

# Having conducted the  Shapiro wilk test on the residuals for healthy life expectancy, 
# the p-value given is  0.1321, indicating no difference between the distribution
# of the residuals and a Gaussian distribution, though this is less than the
# p values for the previous variables


# Score, y, Freedom to make life choices, x
# The data points look a bit bunched up to the right
# in the frequency histogram there is a skew to the left, and a dip in the bin 1
# In the q-q plot, there's alot more poitns bunched to the toward the positive numbers
# The two outliers identified are 153 and 152
# In the density histogram, the residuals line is much less smooth than gaussian
# The residual line also has more peaks
# May need to look at transforming data or discarding for Multi linear regression 


plot(y = happy_192$Score, x = happy_192$Freedom.to.make.life.choices,
     ylab = "Score", xlab = "Freedom to make life choices",
     main = "Happiness residuals (Score and Freedom to make life choices)",
     pch = 20, col = "orchid", cex = 1)

FLC_lm <- lm(formula = happy_192$Score ~ happy_192$Freedom.to.make.life.choices)

FLC_lm

abline(reg = FLC_lm)

text(x = 0.2, y = 7, labels = "y = 3.679, x = 4.403") 

arrows(x0 = happy_192$Freedom.to.make.life.choices,
       x1 = happy_192$Freedom.to.make.life.choices,
       y0 = predict(FLC_lm),
       y1 = predict(FLC_lm) + residuals(FLC_lm),
       length = 0)

par(mfrow = c(1,2))

# Frequency Histogram and q-q plot for residuals

hist(residuals(FLC_lm), main = "")
qqPlot(residuals(FLC_lm))

par(mfrow = c(1,1))

# Density Histogram, with theoretical Gaussian for residuals

hist(residuals(FLC_lm), 
     xlim = c(-2, 2), ylim = c(0,.9),
     main = "",
     prob = T)

lines(density(residuals(FLC_lm)),   
      col = "seagreen3", lty = 1, lwd = 3) 

x <- seq(-1,+1,by=0.02) 

curve(dnorm(x, mean = mean(residuals(FLC_lm)),
            sd = sd(residuals(FLC_lm))),
      add = T,
      col = "purple2", lty = 3, lwd = 3)

abline(v = 0,
       freq = F,
       col = "darkblue", lty = 2, lwd = 3)

legend(x = .6, y = .8,
       legend = c("Residuals", "Gaussian", "Mean"),
       lty = c(1,3,2),
       col = c("seagreen3", "purple2","darkblue"), lwd = c(3,3,3))


# Statistical Test of Gaussian Residuals
# NH: There's no difference between the distrubution of the residuals and Gaussian
# H: There is a difference between the distribution of the residuals and Gaussian

shapiro.test(residuals(FLC_lm))

# Having conducted the  Shapiro wilk test on the residuals for healthy life expectancy, 
# the p-value given is  0.008465, indicating a difference between the distribution
# of the residuals and a Gaussian distribution.


# Score, y, Generosity, x
# The plot shows the data points congregating to the left of the plot, with a few poitns to the right
# The frequency histagram looks better, a little bit more gaussian but still with some extremities
# The q-q plot looks reasonably even, but there is a slith s shape to it. 
# the two outliers highlighted are 1, and 156
# On the density histogram, the residual line does follow the gaussian fairly closely, 
# However there isn't really the peak expected, it's a bit more flattened 
# there's maybe a very soft peak betwen 0.5 and 1


plot(y = happy_192$Score, x = happy_192$Generosity,
     ylab = "Score", xlab = "Generosity",
     main = "Happiness residuals (Score and Generosity)",
     pch = 20, col = "orchid", cex = 1)

G_lm <- lm(formula = happy_192$Score ~ happy_192$Generosity)

G_lm

abline(reg = G_lm)

text(x = 0.5, y = 7, labels = "y = 5.2433, x = 0.8861") 

arrows(x0 = happy_192$Generosity,
       x1 = happy_192$Generosity,
       y0 = predict(G_lm),
       y1 = predict(G_lm) + residuals(G_lm),
       length = 0)

par(mfrow = c(1,2))

# Frequency Histogram and q-q plot for residuals

hist(residuals(G_lm), main = "")
qqPlot(residuals(G_lm))

par(mfrow = c(1,1))

# Density Histogram, with theoretical Gaussian for residuals

hist(residuals(G_lm), 
     xlim = c(-2, 2), ylim = c(0,.9),
     main = "",
     prob = T)

lines(density(residuals(G_lm)),   
      col = "seagreen3", lty = 1, lwd = 3) 

x <- seq(-1,+1,by=0.02) 

curve(dnorm(x, mean = mean(residuals(G_lm)),
            sd = sd(residuals(G_lm))),
      add = T,
      col = "purple2", lty = 3, lwd = 3)

abline(v = 0,
       freq = F,
       col = "darkblue", lty = 2, lwd = 3)

legend(x = .6, y = .8,
       legend = c("Residuals", "Gaussian", "Mean"),
       lty = c(1,3,2),
       col = c("seagreen3", "purple2","darkblue"), lwd = c(3,3,3))

# Statistical Test of Gaussian Residuals
# NH: There's no difference between the distrubution of the residuals and Gaussian
# H: Thsi is a difference between the distribution of the residuals and Gaussian

shapiro.test(residuals(G_lm))

# Having conducted the  Shapiro wilk test on the residuals for Generosity, 
# the p-value given is  0.1778, indicating there's no difference between the distribution
# of the residuals and a Gaussian distribution.


# Score, y, Perceptions of corruption, x
# Not looking great, very bunched up on one side
# The frequency histogram is also a bit all over the place, there's a bell shape, 
# The sizes of the bins are a big more irregular
# The q-q plot looks a bit wavy, and has a slight S shape to it
# The outliers identified are 152, and 156
# From the Density plot, definatley not Gaussian. There's two fairly emphatic peaks o nthe residual line
# May need to remove, or transform


plot(y = happy_192$Score, x = happy_192$Perceptions.of.corruption,
     ylab = "Score", xlab = "Perceptions of Corruption",
     main = "Happiness residuals (Score and Perceptions of corruption)",
     pch = 20, col = "orchid", cex = 1)

PC_lm <- lm(formula = happy_192$Score ~ happy_192$Perceptions.of.corruption)

PC_lm

abline(reg = PC_lm)

text(x = 0.35, y = 5, labels = "y = 4.905, x = 4.540") 

arrows(x0 = happy_192$Perceptions.of.corruption,
       x1 = happy_192$Perceptions.of.corruption,
       y0 = predict(PC_lm),
       y1 = predict(PC_lm) + residuals(PC_lm),
       length = 0)

par(mfrow = c(1,2))

# Frequency Histogram and q-q plot for residuals

hist(residuals(PC_lm), main = "")
qqPlot(residuals(PC_lm))

par(mfrow = c(1,1))

# Density Histogram, with theoretical Gaussian for residuals

hist(residuals(PC_lm), 
     xlim = c(-2, 2), ylim = c(0,.9),
     main = "",
     prob = T)

lines(density(residuals(PC_lm)),   
      col = "seagreen3", lty = 1, lwd = 3) 

x <- seq(-1,+1,by=0.02) 

curve(dnorm(x, mean = mean(residuals(PC_lm)),
            sd = sd(residuals(PC_lm))),
      add = T,
      col = "purple2", lty = 3, lwd = 3)

abline(v = 0,
       freq = F,
       col = "darkblue", lty = 2, lwd = 3)

legend(x = .6, y = .8,
       legend = c("Residuals", "Gaussian", "Mean"),
       lty = c(1,3,2),
       col = c("seagreen3", "purple2","darkblue"), lwd = c(3,3,3))

# Statistical Test of Gaussian Residuals
# NH: There's no difference between the distrubution of the residuals and Gaussian
# H: Thsi is a difference between the distribution of the residuals and Gaussian

shapiro.test(residuals(PC_lm))

# Having conducted the  Shapiro wilk test on the residuals for Perceptions of corruption, 
# the p-value given is  0.0005501, indicating there's a clear difference between the distribution
# of the residuals and a Gaussian distribution.


# Homosedasticity testing

# GDP
# Slight clustering towards the right of the plot
# No distinguishable pattern 
# Majority of data poitns are within the confidence intervals
# Inclined to proceed

# Plot of residuals for GDP

plot(y = residuals(GDP_lm), x = fitted(GDP_lm),
     pch = 16, cex = .8) 

# The residual standard error

summary(GDP_lm)$sigma 

(GDP_uci <- summary(GDP_lm)$sigma*1.96) 
(GDP_lci <- -summary(GDP_lm)$sigma*1.96) 


abline(h = c(0, GDP_uci, GDP_lci),
       lwd = c(2,2,2),
       lty = c(2,3,3),
       col = c("darkblue", "orchid", "orchid"))


# Social Support
# There's some clustering to the right

# Plot of residuals for social support

plot(y = residuals(SS_lm), x = fitted(SS_lm),
     pch = 16, cex = .8) 

# The residual standard error

summary(SS_lm)$sigma 

(SS_uci <- summary(SS_lm)$sigma*1.96) 
(SS_lci <- -summary(SS_lm)$sigma*1.96) 


abline(h = c(0, SS_uci, SS_lci),
       lwd = c(2,2,2),
       lty = c(2,3,3),
       col = c("darkblue", "orchid", "orchid"))

# Healthy Life expectancy
# Some clustering on the right of the plot
# a few outliers outside the confidence intervals
# points scattered and observations look independant
# It isn't cone shaped

plot(y = residuals(HLE_lm), x = fitted(HLE_lm),
     pch = 16, cex = .8) 

# The residual standard error

summary(HLE_lm)$sigma 

(HLE_uci <- summary(HLE_lm)$sigma*1.96) 
(HLE_lci <- -summary(HLE_lm)$sigma*1.96) 


abline(h = c(0, HLE_uci, HLE_lci),
       lwd = c(2,2,2),
       lty = c(2,3,3),
       col = c("darkblue", "orchid", "orchid"))

# Freedom to make choices
# There are a few outliers
#There isn't much clustering
# Looks fine for homoscedasticity

plot(y = residuals(FLC_lm), x = fitted(FLC_lm),
     pch = 16, cex = .8) 

# The residual standard error

summary(FLC_lm)$sigma 

(FLC_uci <- summary(FLC_lm)$sigma*1.96) 
(FLC_lci <- -summary(FLC_lm)$sigma*1.96) 


abline(h = c(0, FLC_uci, FLC_lci),
       lwd = c(2,2,2),
       lty = c(2,3,3),
       col = c("darkblue", "orchid", "orchid"))


# Generosity
# Bunched to the left
# Not cone shaped

plot(y = residuals(G_lm), x = fitted(G_lm),
     pch = 16, cex = .8) 

# The residual standard error

summary(G_lm)$sigma 

(G_uci <- summary(G_lm)$sigma*1.96) 
(G_lci <- -summary(G_lm)$sigma*1.96) 


abline(h = c(0, G_uci, G_lci),
       lwd = c(2,2,2),
       lty = c(2,3,3),
       col = c("darkblue", "orchid", "orchid"))


# Perception of corruption
# Bunched to the left
# weighted towards the top.

plot(y = residuals(PC_lm), x = fitted(PC_lm),
     pch = 16, cex = .8) 

# The residual standard error

summary(PC_lm)$sigma 

(PC_uci <- summary(PC_lm)$sigma*1.96) 
(PC_lci <- -summary(PC_lm)$sigma*1.96) 


abline(h = c(0, PC_uci, PC_lci),
       lwd = c(2,2,2),
       lty = c(2,3,3),
       col = c("darkblue", "orchid", "orchid"))


# 06 Log transformed variables (EDA) ####

# Duplicating dataframe

happy_192_new <- happy_192

head(happy_192)

head(happy_192_new)

# Natural log transforming columns 3,5,7 and saving them back to dataframe
happy_192_new[,c(5,7)] <- log(happy_192_new[,c(5,7)])

head(happy_192)

# Fixing -Inf values in natural log transformed columns 

happy_192_new$Perceptions.of.corruption
happy_192_new$Freedom.to.make.life.choices
happy_192_new$Social.support

happy_192_new$Perceptions.of.corruption

is.na(happy_192_new) <- sapply(happy_192_new, is.infinite)

# Exploration of natural log transformed residuals 
# Have removed for tidiness however and recycled code for log 10
# with natural log, Perception of corruption was not Gaussian but was homoscedastic
# Social support was Gaussian but not Homoscedastic, 
# Freedom to make life choices was Gaussian but not Homoscedastic,

# Perceptions of corruption 

#Log 10
# frequency histogram looks skewed to left
# q-q plot looks slightly s shaped with outliers identified as 152 and 156
# q-q plot is also a little wavy around the middle
# Density histogram has two soft peaks, and deviates a little from expected gaussian on the right

plot(y = happy_192_new$Score, x = happy_192_new$Perceptions.of.corruption,
     ylab = "Score", xlab = "Perceptions of Corruption log",
     main = "Happiness residuals (Score and Perceptions of corruption log)",
     pch = 20, col = "orchid", cex = 1)

PC_lm_log <- lm(formula = happy_192_new$Score ~ happy_192_new$Perceptions.of.corruption)

PC_lm_log

abline(reg = PC_lm_log)

text(x = -2, y = 7, labels = "y = 6.1578, x = 0.6817") 

arrows(x0 = happy_192_new$Perceptions.of.corruption,
       x1 = happy_192_new$Perceptions.of.corruption,
       y0 = predict(PC_lm_log),
       y1 = predict(PC_lm_log) + residuals(PC_lm_log),
       length = 0)

par(mfrow = c(1,2))

# Frequency Histogram and q-q plot for residuals

hist(residuals(PC_lm_log), main = "")
qqPlot(residuals(PC_lm_log))

par(mfrow = c(1,1))

# Density Histogram, with theoretical Gaussian for residuals

hist(residuals(PC_lm_log), 
     xlim = c(-2, 2), ylim = c(0,.9),
     main = "",
     prob = T)

lines(density(residuals(PC_lm_log)),   
      col = "seagreen3", lty = 1, lwd = 3) 

x <- seq(-1,+1,by=0.02) 

curve(dnorm(x, mean = mean(residuals(PC_lm_log)),
            sd = sd(residuals(PC_lm_log))),
      add = T,
      col = "purple2", lty = 3, lwd = 3)

abline(v = 0,
       freq = F,
       col = "darkblue", lty = 2, lwd = 3)

legend(x = .6, y = .8,
       legend = c("Residuals", "Gaussian", "Mean"),
       lty = c(1,3,2),
       col = c("seagreen3", "purple2","darkblue"), lwd = c(3,3,3))

# Statistical Test of Gaussian Residuals
# NH: There's no difference between the distrubution of the residuals and Gaussian
# H: Thsi is a difference between the distribution of the residuals and Gaussian

shapiro.test(residuals(PC_lm_log))

# Having conducted the  Shapiro wilk test on the residuals for Perceptions of corruption, 
# the p-value given is  0.004926, indicating there's a clear difference between the distribution
# of the residuals and a Gaussian distribution.
# log transformation did not encourage residuals to conform to Gaussian


# Freedom to make life choices

# Natural Log transformed
# Plot looks a bit bunched up to the right/skewed to the left
# q-q plot looks a bit wavy with outliers at 152 and 153
# q-q plot also has a little skew to the top

# Log10 transformed
# Plot still skewed to the left
# frequency histogram has one large bin, but other than this seems reasonably bell shaped
# q-q plot has outliers 152, and 153, and a small skew at the top
# Density histogram residuals line has two soft peaks, but followed the Gaussian line with  asmal ldeviation on the right 

plot(y = happy_192_new$Score, x = happy_192_new$Freedom.to.make.life.choices,
     ylab = "Score", xlab = "Freedom to make life choices log",
     main = "Happiness residuals (Score and Freedom to make life choices log)",
     pch = 20, col = "orchid", cex = 1)

FLC_lm_log <- lm(formula = happy_192_new$Score ~ happy_192_new$Freedom.to.make.life.choices)

FLC_lm_log

abline(reg = FLC_lm_log)

text(x = -4, y = 7, labels = "y = 6.3364, x = 0.8762") 

arrows(x0 = happy_192_new$Freedom.to.make.life.choices,
       x1 = happy_192_new$Freedom.to.make.life.choices,
       y0 = predict(FLC_lm_log),
       y1 = predict(FLC_lm_log) + residuals(FLC_lm_log),
       length = 0)

par(mfrow = c(1,2))

# Frequency Histogram and q-q plot for residuals

hist(residuals(FLC_lm_log), main = "")
qqPlot(residuals(FLC_lm_log))

par(mfrow = c(1,1))

# Density Histogram, with theoretical Gaussian for residuals

hist(residuals(FLC_lm_log), 
     xlim = c(-2, 2), ylim = c(0,.9),
     main = "",
     prob = T)

lines(density(residuals(FLC_lm_log)),   
      col = "seagreen3", lty = 1, lwd = 3) 

x <- seq(-1,+1,by=0.02) 

curve(dnorm(x, mean = mean(residuals(FLC_lm_log)),
            sd = sd(residuals(FLC_lm_log))),
      add = T,
      col = "purple2", lty = 3, lwd = 3)

abline(v = 0,
       freq = F,
       col = "darkblue", lty = 2, lwd = 3)

legend(x = .6, y = .8,
       legend = c("Residuals", "Gaussian", "Mean"),
       lty = c(1,3,2),
       col = c("seagreen3", "purple2","darkblue"), lwd = c(3,3,3))

# Statistical Test of Gaussian Residuals
# NH: There's no difference between the distrubution of the residuals and Gaussian
# H: Thsi is a difference between the distribution of the residuals and Gaussian

shapiro.test(residuals(FLC_lm_log))

# Having conducted the  Shapiro wilk test on the residuals for Freedom to make choices, 
# the p-value given is  0.08922, indicating there's no difference between the distribution
# of the residuals and a Gaussian distribution.


# Social support

# Natural log transformed
# Plot is quite skewed, more points below the line and quite bunched to the right
# Outliers on qq plot are 102 151, bunched up in the middle and a little skew to the top
# Frequency histogram, not quite bell shaped
# Density histogram looks ok, slight deviation of residuals in the positive bins

# Log10 transformed 
# plot is really bunched to the right, with a few points on the left
# Frequency histogram looks reasonably bell shaped, some very small bins on the extremes of the x axis
# q-q plot follows the line fairly closely, with a very small skew at the top
# Outliers 151, and 102 
# Density histogram, the residuals follow the expected gaussian line very closely on the left
# The expected residuals line deviates a bit on the right.

plot(y = happy_192_new$Score, x = happy_192_new$Social.support,
     ylab = "Score", xlab = "Social Support log",
     main = "Happiness residuals (Score and Social support)",
     pch = 20, col = "orchid", cex = 1)

SS_lm_log <- lm(formula = happy_192_new$Score ~ happy_192_new$Social.support)

SS_lm_log

abline(reg = SS_lm_log)

text(x = -0.5, y = 7, labels = "y = 4.970, x = 2.802") 

arrows(x0 = happy_192_new$Social.support,
       x1 = happy_192_new$Social.support,
       y0 = predict(SS_lm_log),
       y1 = predict(SS_lm_log) + residuals(SS_lm_log),
       length = 0)

par(mfrow = c(1,2))

# Frequency Histogram and q-q plot for residuals

hist(residuals(SS_lm_log), main = "")
qqPlot(residuals(SS_lm_log))

par(mfrow = c(1,1))

# Density Histogram, with theoretical Gaussian for residuals

hist(residuals(SS_lm_log), 
     xlim = c(-2, 2), ylim = c(0,.9),
     main = "",
     prob = T)

lines(density(residuals(SS_lm_log)),   
      col = "seagreen3", lty = 1, lwd = 3) 

x <- seq(-1,+1,by=0.02) 

curve(dnorm(x, mean = mean(residuals(SS_lm_log)),
            sd = sd(residuals(SS_lm_log))),
      add = T,
      col = "purple2", lty = 3, lwd = 3)

abline(v = 0,
       freq = F,
       col = "darkblue", lty = 2, lwd = 3)

legend(x = .6, y = .8,
       legend = c("Residuals", "Gaussian", "Mean"),
       lty = c(1,3,2),
       col = c("seagreen3", "purple2","darkblue"), lwd = c(3,3,3))

# Statistical Test of Gaussian Residuals
# NH: There's no difference between the distrubution of the residuals and Gaussian
# H: Thsi is a difference between the distribution of the residuals and Gaussian

shapiro.test(residuals(SS_lm_log))

# Having conducted the  Shapiro wilk test on the residuals for Social support, 
# the p-value given is  0.8815, indicating there's no difference between the distribution
# of the residuals and a Gaussian distribution.


# Homoscedasticity testing on natural log and Log 10 transformed variables

# log transformed Social Support
# Still skewed to the right
# In a side by side comparison looks worse than the SS_lm when not log transformed
# Human/Neurodiverse error - previous homoscedasticity plot included upper and lower confidence interval from wrong argument

# Log10 transformed social support
# Still cone shaped. 
# Data points are bunched to the right :( 

# Plot of residuals for natural log transformed social support

plot(y = residuals(SS_lm_log), x = fitted(SS_lm_log),
     pch = 16, cex = .8) 

# The residual standard error

summary(SS_lm_log)$sigma 

(SS_log_uci <- summary(SS_lm_log)$sigma*1.96) 
(SS_log_lci <- -summary(SS_lm_log)$sigma*1.96) 


abline(h = c(0, SS_log_uci, SS_log_lci),
       lwd = c(2,2,2),
       lty = c(2,3,3),
       col = c("darkblue", "orchid", "orchid"))


# Perception of corruption natural log and log 10 transformed


# Plot of residuals for natural log transformed Perception of corruption
# Improvement, with points looking more evenly distributed,
# A few scattered outliers but will accept

# Log 10 transformed
# Looks fine, data points are a little more centred than natural log but still
# reasonably evenly distributed


plot(y = residuals(PC_lm_log), x = fitted(PC_lm_log),
     pch = 16, cex = .8) 

# The residual standard error

summary(PC_lm_log)$sigma 

(PC_log_uci <- summary(PC_lm_log)$sigma*1.96) 
(PC_log_lci <- -summary(PC_lm_log)$sigma*1.96) 


abline(h = c(0, PC_log_uci, PC_log_lci),
       lwd = c(2,2,2),
       lty = c(2,3,3),
       col = c("darkblue", "orchid", "orchid"))


# Plot of residuals for natural log transformed freedom to make life choices

# Natural log
# Cone shaped

# Log 10
# Still cone shaped

plot(y = residuals(FLC_lm_log), x = fitted(FLC_lm_log),
     pch = 16, cex = .8) 

# The residual standard error

summary(FLC_lm_log)$sigma 

(FLC_log_uci <- summary(FLC_lm_log)$sigma*1.96) 
(FLC_log_lci <- -summary(FLC_lm_log)$sigma*1.96) 


abline(h = c(0, FLC_log_uci, FLC_log_lci),
       lwd = c(2,2,2),
       lty = c(2,3,3),
       col = c("darkblue", "orchid", "orchid"))


# 07 Square root transformed variables (EDA) ####

# After trying log tranformation with Social support with little improvement to 
# homoscedasticity, will try to apply squareroot function and add to the 
# variables as an additional term, to allow the multiple linear regression 
# to accept a curve. So allowing a layer of complexity to the model to enable 
# more detailed observation of the behavior of the data available around social support 

# Duplicating data frame

happy_192_sqrt[,c(3, 5, 7)] <- sqrt(happy_192_sqrt[,c(3, 5, 7)])

happy_192_sqrt <- happy_192

head(happy_192_sqrt)

# Transforming social support 


SS_sqrt <- sqrt(happy_192_sqrt$Social.support)

summary(SS_sqrt)

head(SS_sqrt)

mutate(happy_192_sqrt, Social.support.sqrt = SS_sqrt)

head(happy_192_sqrt)

# Examination of sqrt residuals as gaussian
# a bit bunched to the right, and heave below the regression line
# Frequency histogram seems fairly gaussian but there's a small bin edging out to the right
# q-q plot, seems reasonably uniform along the line, small wave to the top, with the outliers identified at 155, and 151
# Density histogram, looks good, residual line follows gaussian line fairly closely, with the deviation on the right seen previously even smaller


plot(y = happy_192_sqrt$Score, x = SS_sqrt,
     ylab = "Score", xlab = "squared social support",
     main = "Happines residuals (Score and squared social support)",
     pch = 20, col = "orchid", cex = 1)

SS_sqrt_lm <- lm(formula = happy_192_sqrt$Score ~ SS_sqrt)

abline(reg = SS_sqrt_lm)

SS_sqrt_lm

text(x = 0.2, y = 7, labels = "y = 0.1306, x = 4.8536") 

arrows(x0 = SS_sqrt,
       x1 = SS_sqrt,
       y0 = predict(SS_sqrt_lm),
       y1 = predict(SS_sqrt_lm) + residuals(SS_sqrt_lm),
       length = 0)

par(mfrow = c(1,2))

# Frequency Histogram and q-q plot for residuals

hist(residuals(SS_sqrt_lm), main = "")
qqPlot(residuals(SS_sqrt_lm))

par(mfrow = c(1,1))

# Density Histogram, with theoretical Gaussian for residuals

hist(residuals(SS_sqrt_lm), 
     xlim = c(-2, 2), ylim = c(0,.9),
     main = "",
     prob = T)

lines(density(residuals(SS_sqrt_lm)),   
      col = "seagreen3", lty = 1, lwd = 3) 

x <- seq(-1,+1,by=0.02) 

curve(dnorm(x, mean = mean(residuals(SS_sqrt_lm)),
            sd = sd(residuals(SS_sqrt_lm))),
      add = T,
      col = "purple2", lty = 3, lwd = 3)

abline(v = 0,
       freq = F,
       col = "darkblue", lty = 2, lwd = 3)

legend(x = .6, y = .8,
       legend = c("Residuals", "Gaussian", "Mean"),
       lty = c(1,3,2),
       col = c("seagreen3", "purple2","darkblue"), lwd = c(3,3,3))

# Statistical Test of Gaussian Residuals
# NH: There's no difference between the distrubution of the residuals and Gaussian
# H: Thsi is a difference between the distribution of the residuals and Gaussian

shapiro.test(residuals(SS_sqrt_lm))

# Having conducted the  Shapiro wilk test on the residuals for Square root of Social support, 
# the p-value given is  0.2955, indicating no difference between the distribution
# of the residuals and a Gaussian distribution

# Homoscedasticty
# Still cone shaped

plot(y = residuals(SS_sqrt_lm), x = fitted(SS_sqrt_lm),
     pch = 16, cex = .8) 

# The residual standard error

summary(SS_sqrt_lm)$sigma 

(SS_sqrt_uci <- summary(SS_sqrt_lm)$sigma*1.96) 
(SS_sqrt_lci <- -summary(SS_sqrt_lm)$sigma*1.96) 


abline(h = c(0, SS_sqrt_uci, SS_sqrt_lci),
       lwd = c(2,2,2),
       lty = c(2,3,3),
       col = c("darkblue", "orchid", "orchid"))


# Comparison with untransformed Social support variable
# A bit cone shaped but not as cone shaped as the transofrmed variables

plot(y = residuals(SS_lm), x = fitted(SS_lm),
     pch = 16, cex = .8) 

# The residual standard error

summary(SS_lm)$sigma 

(SS_uci <- summary(SS_lm)$sigma*1.96) 
(SS_lci <- -summary(SS_lm)$sigma*1.96) 


abline(h = c(0, SS_uci, SS_lci),
       lwd = c(2,2,2),
       lty = c(2,3,3),
       col = c("darkblue", "orchid", "orchid"))

# Comparison with natural log transformed Social support variable

plot(y = residuals(SS_lm_log), x = fitted(SS_lm_log),
     pch = 16, cex = .8) 

# The residual standard error

summary(SS_lm_log)$sigma 

(SS_log_uci <- summary(SS_lm_log)$sigma*1.96) 
(SS_log_lci <- -summary(SS_lm_log)$sigma*1.96) 


abline(h = c(0, SS_log_uci, SS_log_lci),
       lwd = c(2,2,2),
       lty = c(2,3,3),
       col = c("darkblue", "orchid", "orchid"))


# 08 Square of non-conforming variables within linear regression model ####

# Will look to combining these into the multiple linear regression
# for the purpose of creating a more expressive model, 
# engaging with a more quadratic approach to express the curves found in the 
# data which doesn't conform to the assumptions traditionally required with the multiple linear regression


# Social support
# Linear fit with squared variables

SS_lm_X2 <- lm(happy_192$Score ~ happy_192$Social.support + I(happy_192$Social.support^2))
summary(SS_lm_X2)

# The squared Social support has a low p-value attached indicating more expression,
# in terms of letting us know what Social support means for the Score of happiness. 

# ANOVA comparison of the linear regression models of the squared and un-squared 
# to ascertain expressiveness 
# Null hypothesis: The original Linear Regression model for Social support and that for the model including the squared 
# Hypothesis: The model containing both the squared and un-squared is more expressive, than un-squared alone .

anova(SS_lm, SS_lm_X2)

# The f statistic is 23, and the p-value is very small, there is confidence in 
# the model containing the squared variables as being more expressive that the unsquared variables alone. 


# Residual plot for regression model including squared variable

par(mfrow = c(1, 2))
plot(SS_lm_X2)

# Residuals look even, inclined to accept
# q-q plot also looks fine, a little skew to the top but will accept

par(mfrow = c(1, 1))

# Freedom to make life choices
# Linear fit with squared variables. 

FLC_lm_X2 <- lm(happy_192$Score ~ happy_192$Freedom.to.make.life.choices + I(happy_192$Freedom.to.make.life.choices^2))
summary(FLC_lm_X2)

# While there is some improvement, there is no significant improvement 
# with the squared model, the p-value is 0.19

anova(FLC_lm, FLC_lm_X2)

# This lack of improvment is also reflected in the ANOVA to compare the models

# Have removed Perception of corruption as there was very little significance in
# both the correlations and previous attempts to model in a linear regression 


# 09 Exploring polynomial fits for Freedom to make life choices ####

# up to 5th order
FLC_lm_poly <- lm(happy_192$Score ~ poly(happy_192$Freedom.to.make.life.choices, 5))
summary(FLC_lm_poly)

# up to 3rd order

FLC_lm_poly_3 <- lm(happy_192$Score ~ poly(happy_192$Freedom.to.make.life.choices, 3))
summary(FLC_lm_poly_3)

# There is some significance in the 3rd order, with the p-value at 0.05, 


# 10 Multiple Linear Regression ####

# All the variables, including un - transformed variables which don't conform to 
# required assumptions 
# residuals not Gaussian: Freedom to make life choices and Perception of corruption
# Lack of Homoscedasticity: Perception of corruption, and Social support

# Having found a correlation between Score and some of the factors, 
# now looking to explore the causal relationships between score and some of the 
# independent variables through the perspective of a multiple linear regression
# looking to ascertain the strength of the relationships, and associations, 

# Multiple linear regression with all of the variables 
# irrespective of the conditions being met, 



# Multiple Linear regression excluding Social support, and 
# freedom to make life choices due to lack of homoscedasticity, 
# and perception of corruption due to not being Gaussian

# Null Hypothesis: there is no relationship between the dependant variable Score,
# and the independant variables GDP per capita, Healthy life expectancy and Generosity

# Hypothesis, there is a relationship between the dependant variable Score,
# and the independant variables GDP per capita, Healthy life expectancy and Generosity

happy_19_ex <- happy_192[ , c(1,2,4,6)]

head(happy_19_ex)

H_ex_lm_fit <- lm(Score ~ ., data = happy_19_ex)

summary(H_ex_lm_fit)

G_lm

# Notes on Multiple Linear regression 
# There is more significance derived from Generosity through the perspective of a multiple linear regression
# Interesting to note that in the correlation Generosity also had no significant relationship with GDP per capita and healthly life expectancy
# The coefficient for Generosity in a simple linear regression model was 0.87 compared to 1.45 in a multiple linear regression
# GDP per capita appears to be highly significant
# Healthy life expectancy appears to be highly significant


# 11 Addition of polynomial variables to multiple-regression ####

# Including a quadratic approach that allows for the curves in the data found in 
# social support, freedom to make Life Choices
# Social support has been squared, and Freedom to make life choices has been 
# cubed, and adding these as new variables
# Still however adopting a linear approach to multiple regression


# New Data frame encapsulating the new squared and cubed variables

# Duplicate dataframe (Excluding perception of corruption)

happy_192_sq_cu <- happy_192

head(happy_192)

# Using square function to transform social support

SS_X2 <- happy_192$Social.support^2
summary(SS_X2)
head(SS_X2)

plot(SS_X2)

# Using polynomial function to transform freedom to make life choices

FLC_poly_3 <- poly(happy_192$Freedom.to.make.life.choices, 3)

FLC_poly_3


# Separating 3rd order to have a better look

FLC_ord_3 <- FLC_poly_3[,3]

FLC_ord_3

plot(FLC_ord_3)

# Adding them to new data frame happy_192_sq_cu 

help(cbind)

# Combining the transformed variables in a new data frame

SS_FLC_X2_3 <- data.frame(Social.support.sq = SS_X2, Freedom.life.choices.3 = FLC_poly_3)


# Taking a look

head(SS_FLC_X2_3)

# Adding the squared Social support, with a function to add the polynomial of freedom to make life choices to the duplicate dataframe containing the untransformed variables

happy_192_sq_cu <- head(cbind(happy_192, Social.support.sq = SS_X2, Freedom.life.choices = (poly(happy_192$Freedom.to.make.life.choices, 3))), 156)

# Removing the Freedom to make life choices variable causingthe multicollinearity flag

happy_192_sq_cu_exFLC <- subset(happy_192_sq_cu, select = -c(Freedom.to.make.life.choices))

# Taking a look

head(happy_192_sq_cu)

summary(happy_192_sq_cu)

head(happy_192_sq_cu_exFLC)

#  Multiple linear regression including the quadratic and polynomial 

# Null Hypothesis: there is no relationship between the dependant variable Score,
# and the independent variables GDP per capita, Social support, 
# Healthy life expectancy, Freedom to make life choices, Generosity, 
# Social support squared, and freedom to make life choices cubed.  

# Hypothesis, there is a relationship between the dependant variable Score,
# and the independent variables GDP per capita, Social support, 
# Healthy life expectancy, Freedom to make life choices, Generosity, 
# Social support squared, and freedom to make life choices to the order of 3.  


happy_192_lm_sq_cu <- lm(Score ~ ., data = happy_192_sq_cu)

summary(happy_192_lm_sq_cu)


# Variance inflation factors

vif(happy_192_lm_sq_cu)

# Error Error in vif.default(happy_192_lm_sq_cu) : 
# there are aliased coefficients in the model
# Will need to remove a variable that displays multicolliniarity
# Have done so 


# Cor function to explore
cor(happy_192_sq_cu)

# Duplicate Freedom to make life choices. 


# the un-transformed model for freedom to make life choices yields more 
# significance than the cubed model.
# However not convinced by the heteroscedasticity in residuals displayed in EDA
# This being due to the heteroscedasticity 'pushing' for a greater significance 
# than is actually presence since a least squares regression assumes
# a constant variance.

# Will conduct a Multiple regression excluding Freedome to make life choices, 
# though a Generalised Additive Model may improve the observation that could be 
# gleaned from this model.

# Have included Perception of corruption through violated Gaussian,
# As it did display homoscedasticity across residuals and was slightly 
# significant in correlation testing

# Excluding duplicate freedom to make life choices

happy_192_lm_sq_cu_exFLC <- lm(Score ~ ., data = happy_192_sq_cu_exFLC)

summary(happy_192_lm_sq_cu_exFLC)

coef(summary(happy_192_lm_sq_cu_exFLC))


# There is some significance seen across four of the models in the 
# above Multiple linear regression. 

# the Multiple R squared is 0.80, so about 80% of the variation in the score 
# can be explained by this model, so the 6 factors, presumably largely the significant factors. 

# Looking at the F statistic 71 (greater than 1) in combination with an very small p-value 2.2e-16 for the overall model, 
# there is indication of a highly significant model where the slope for Score 
# and the other 6 factors are approaching zero 

# The below convey significance at an alpha of 0.001
# The p-value for the model for GDP per capita is 0.0037 indicating significance
# The p-value for the model for Healthy life expectancy is 0.0015 indicating significance
# The p-value for the model for squared social support is 0.0047 indicating significance

# The p-value for the model for perception of corruption is 0.012, conveying 
# significance at an alpha of 0.05

# Looking at the intercept, assuming all the factors were 0, the score would be 
# (estimated) as 2.9

# Looking individually at the different factors we can see that those with 
# significance include, GDP per capita, Healthy life expectancy, 
# Perceptions of Corruption, and the squared Social support. 

# It's interesting to see how the Squared social support 
# (transformed due to the heteroscedasticity seen in the un-transformed variable)
# appears significant at only an alpha of 0.001 when accounting for the curve 
# in its slope. 

# We can see that for GDP per capita, a change of 0.67 (0.67%) would affect the Score. 
# For Healthy Life expectancy we can see that a change of 1.1 (1%) would affect the Score
# For perception of corruption a change of 1.36 (1.36%) would affect the score
# And Social support, a change of 1.08 (1%) would affect the score of Happiness. 

# From the above there is confidence in accepting the Hypothesis there is a 
# relationship between the dependant variable (y) Score, and the independent 
# variables (x) GDP per capita, Social support when squared,
# and Healthy life expectancy. There is a slightly significant relationship between
# the dependant variable y and perceptions of corruption. 

# plot to look at residuals of multiple regression

plot(happy_192_lm_sq_cu_exFLC)

# The residuals seems reasonably even
# The outliers are consistently 152, 148 and 133 across the residuals v fitted, 
# q-q plots and scale-location 

# Look at Variance inflation factors (VIFs)

vif(happy_192_lm_sq_cu_exFLC)

# Social support has a VIF of 28 and Social support squared has a VIF of 30,
# And this seems very high indicating a high amount of multicollinearity. 


# Plot of multiple linear regression

help(avPlots)

avPlots(happy_192_lm_sq_cu_exFLC, col = wes_palette("Darjeeling1", 1, type = c("continuous")))
        
# From the Added variable plots, the blue line corroborates with the significance found in the Multiple linear regression  


# 12 Model Selection ####

# The Multiple Linear regression with the exclusions
# Is p greater than n? 

# Removing 'duplicate' Freedom to make life choices (redundant when poly function was used)

happy_192_sq_cu_exFLC <- subset(happy_192_sq_cu, select = -c(Freedom.to.make.life.choices))

# Taking a look

head(happy_192_sq_cu_exFLC)

# Best subset selection

happy192_sq_cu_exFLC_subset <- regsubsets(Score ~., 
                           data = happy_192_sq_cu_exFLC)


summary(happy192_sq_cu_exFLC_subset)

# Adding some models using nvmax function

happy192_sq_cu_exFLC_full <- regsubsets(Score ??? ., data = happy_192_sq_cu_exFLC ,
                           nvmax = 11)

happy192_sq_cu_exFLC_reg_summary <- summary(happy192_sq_cu_exFLC_full)

happy192_sq_cu_exFLC_reg_summary

# Social support squared features in all the models, 

# Taking a look at what we can see

names(happy192_sq_cu_exFLC_reg_summary)

# Having a look at R squared statistic

happy192_sq_cu_exFLC_reg_summary$rsq

# The R squared increases fairly constantly

# Evaluating the models with plots

# Plotting RSS

par(mfrow = c(2, 2))
plot(happy192_sq_cu_exFLC_reg_summary$rss , xlab = " Number of Variables ",
        ylab = " RSS ", type = "l")

# plotting adjusted R squared 

plot(happy192_sq_cu_exFLC_reg_summary$adjr2 , xlab = " Number of Variables ",
        ylab = " Adjusted RSq ", type = "l")


# Finding the maximum point

which.max(happy192_sq_cu_exFLC_reg_summary$adjr2)

# Adding a red dot to show the maximum point

points(7, happy192_sq_cu_exFLC_reg_summary$adjr2[7], col = " red ", cex = 2,
          pch = 20)

# plotting Cp

plot(happy192_sq_cu_exFLC_reg_summary$cp, xlab = " Number of Variables ",
      ylab = "Cp", type = "l")

# Finding the smallest static using which.min

which.min(happy192_sq_cu_exFLC_reg_summary$cp)

# Adding a red dot to show minimum point

points(6, happy192_sq_cu_exFLC_reg_summary$cp[6], col = " red ", cex = 2,
        pch = 20)

# plotting BIC

plot(happy192_sq_cu_exFLC_reg_summary$bic , xlab = " Number of Variables ",
      ylab = " BIC ", type = "l")

# Finding the smallest statistic

which.min(happy192_sq_cu_exFLC_reg_summary$bic)

points(4, happy192_sq_cu_exFLC_reg_summary$bic[4], col = " red ", cex = 2,
        pch = 20)

# Looking for the best model with reg.subsets

help("plot.regsubsets")

plot(happy192_sq_cu_exFLC_full,scale = "r2")
plot(happy192_sq_cu_exFLC_full,scale = "adjr2")
plot(happy192_sq_cu_exFLC_full,scale = "Cp")
plot(happy192_sq_cu_exFLC_full,scale = "bic")


# Looking at the coefficients

coef(happy192_sq_cu_exFLC_full, 8)
# model 8 possibly not the best performing

coef(happy192_sq_cu_exFLC_full, 4)

# This looks good

coef(happy192_sq_cu_exFLC_full, 5)
# Also looks ok, in terms of BIC and Cp, and covering more variables in R squared and adjust R squared 

coef(happy192_sq_cu_exFLC_full, 6)
# Interesting, Possibly performing similarly well to model 5, which leads me to question 5 and 6

# Generosity has very little effect from the perspective of Cp adjusted R squared and BIC
# Freedom to make life choices to the order of 2 has very little effect from the perspective of adjusted R squared, Cp, and BIC
# Freedom to make life choices to the order of 1 also has little effect from the perspective of Cp

# Model 5, has a BIC of 210, which is the highest, but does cross over with the 
# adjusted R squared in terms of including the variables GDP per capita, Healthy
# life expectancy and Freedom to make life choices 1
# The Cp is 8.7, which while isn't the lowest is a lot lower than 18, 45 and 98
# and includes the variables GDP per capita, Healthy life expectancy, Social support squared and freedome to make life choices

# Model 6, in comparison, displays a lower BIC at 200 and includes generosity,
# whereas the Cp is higher at 10 and includes all the variables
# The Adjusted R2 remains 78 losing social support, generosity, Perceptions of corruption and Freedom of life choices 2 and 3

# Possibly Model 5 is the best here. 
# Model 6 has a lower BIC, 

# Forward and backwards stepwise selection

# Forwards
# The best one variable model starts with Social support squared
# The best two variable model has Social support squared 
# The three variable model has the above with Freedom to make life choices additionally
# The four variable model then has GDP per capita additionally (interestingly one of the more significant in the Multiple linear regression)
# The five variable model then included social support which was not originally significant. 

# It's interesting that Freedom to make life choices only shows up in the 3 variable model, 
# It shows up as fairly significant in the Multiple linear regression, 
# though I'm inclined to attribute this to the heteroscedasticity the residuals exhibted inflating this 
# May need to approach this variable with a non-linear regression 

happy_192_sq_cu_exFLC_regfit_fwd <- regsubsets (Score ??? ., data = happy_192_sq_cu_exFLC ,
                          nvmax = 11, method = "forward")

summary (happy_192_sq_cu_exFLC_regfit_fwd)

# A look at the coefficients of model 4

coef(happy_192_sq_cu_exFLC_regfit_fwd, 4)

# A look at the coefficients of model 5

coef(happy_192_sq_cu_exFLC_regfit_fwd, 5)

# Backwards
# the most effective one variable model starts with Social support squared
# The most effective two variable model additionally has GDP.capita (Similar to my expectations)
# Then the 3 variable model has freedom to make life choices additionally
# Then the 4 variable model has healthy life expectancy additionally
# With the 5 variable model with Social support which wasn't within 0.05 significance in the Multiple Linear regression 

# Model 4 looks promising

happy_192_sq_cu_exFLC_regfit_bwd <- regsubsets (Score ~ ., data = happy_192_sq_cu_exFLC ,
                            nvmax = 11, method = "backward")

summary (happy_192_sq_cu_exFLC_regfit_bwd)

# Taking a look at the coefficients 

# Model 4

coef(happy_192_sq_cu_exFLC_regfit_bwd, 4)

# Model 5

coef(happy_192_sq_cu_exFLC_regfit_bwd, 5)

# All 

coef(happy192_sq_cu_exFLC_full, 5)

# All the variables, showed that the coefficients were the same across forwards, and backwards selection
# The only differences being seen in subset selection

coef(happy_192_sq_cu_exFLC_regfit_bwd, 9)
coef(happy_192_sq_cu_exFLC_regfit_bwd, 9)
coef(happy192_sq_cu_exFLC_full, 9)

# Validation set approach

# making a training set

set.seed (1)

train <- sample (c(TRUE , FALSE), nrow (happy_192_sq_cu_exFLC),
                   replace = TRUE)
test <- (!train)

# Subset selection

happy_subset <- regsubsets (Score ??? .,
                           data = happy_192_sq_cu_exFLC[train , ], nvmax = 9)

summary(happy_subset)

# model matrix from test data

happy_test_mat <- model.matrix (Score ??? ., data = happy_192_sq_cu_exFLC[test , ])
 
# Validation  

happy_val_errors = rep(NA,9)
for (i in 1:9) {
  coefi = coef(happy_subset, id = i)
  pred = happy_test_mat[,names(coefi)] %*% coefi
  happy_val_errors[i] = mean(((happy_192$Score[test]) - pred)^2)
}

# Taking a look 

happy_val_errors

# How many variables are in the best performing model? 

which.min(happy_val_errors)

# There are four variables in the best performing model

coef(happy_subset, 4)

# Writing a prediction method for regsubsets() (As it doesn't have one)

predict.regsubsets <- function (object, newdata, id, ...) {
  form <- as.formula (object$call[[2]])
  mat <- model.matrix (form, newdata)
  coefi <- coef (object, id = id)
  xvars <- names (coefi)
  mat[, xvars] %*% coefi
}  

# Best subset selection on the full dataset, and four variable model

happy_regfit_best <- regsubsets(Score ??? ., data = happy_192_sq_cu_exFLC,
                           nvmax = 11)

coef(happy_regfit_best, 4)

# Cross-validation to choose a model in the group of different sized models

# making folds and allocating observations

k <- 10
n <- nrow (happy_192_sq_cu_exFLC)
set.seed(1)
folds <- sample(rep (1:k, length = n))

cv.errors <- matrix (NA, k, 11,
                     dimnames = list (NULL , paste (1:11)))


# Writing a loop to perform the cross validation


for (j in 1:k) {
  happy_best_fit <- regsubsets(Score ~ .,
                         data = happy_192_sq_cu_exFLC[folds != j, ],
                         nvmax = 9)
  for (i in 1:9) {
    pred <- predict(happy_best_fit, happy_192_sq_cu_exFLC[folds == j, ], id = i)
    cv.errors[j, i] <-
      mean((happy_192_sq_cu_exFLC$Score[folds == j] - pred)^2)
  }
}


# Using apply to average over the columns to see a vector
# where ith element is the crossvalidation error for the i-variable model

mean.cv.errors <- apply (cv.errors , 2, mean)

# taking a look

mean.cv.errors

par (mfrow = c(1, 1))

# Plot of mean.cv.error

plot (mean.cv.errors , type = "b")

# A four variable model seems to be the one

happy_reg_best <- regsubsets (Score ??? ., data = happy_192_sq_cu_exFLC ,
                        nvmax = 11)

coef(happy_reg_best, 4)




# 13 Polynomial regression of freedom to make life choices ####

# Polynomial regression of score against Freedom to make life choices 
# Would like a better fit for the polynomial of Freedom to make life choices
# As not sure about the inclusion of the variable to the order of 1 and 2
# In the multiple linear regression

attach(happy_192)

poly_FLC <- lm(Score ~ poly(Freedom.to.make.life.choices, 4), data = happy_192)

coef(summary(poly_FLC)) 

# Curious

# plot the residuals

# Direct approach to fit

poly_FLC_2 <- lm(Score ~ poly(Freedom.to.make.life.choices, 4, raw = T), data = happy_192)

coef(summary(poly_FLC_2))

# Grid of values for prediction with standard error

FLC_lims <- range(Freedom.to.make.life.choices)

FLC.grid <- seq(from = FLC_lims[1], to = FLC_lims[2])

FLC_preds <- predict(poly_FLC, newdata = list(Freedom.to.make.life.choices = FLC.grid),
                     se = TRUE)

se.bands <- cbind(FLC_preds$poly_FLC + 2 * FLC_preds$se.fit,
                  FLC_preds$poly_FLC - 2 * FLC_preds$se.fit)

# Plotting the above and adding degree 3 polynomial

par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1),
    oma = c(0, 0, 4, 0))

plot(Freedom.to.make.life.choices, Score, xlim = FLC_lims, cex = .75, col = "dark orchid")

title("Degree-4 Polynomial", outer = T)

lines(FLC.grid, FLC_preds$poly_FLC, lwd = 2, col = "dark green")

matlines(FLC.grid,lwd = 1, col = "dark green", lty = 3)


# Seeing what degree of polynomial fits best

# Fitting some models, linear to a 5th order polynomial to see what the
# simplest model is, that'll explain the relationship between Score 
# and Freedom to make life choices.

FLC_fit_1 <- lm(Score ~ Freedom.to.make.life.choices, data = happy_192)
FLC_fit_2 <- lm(Score ~ poly(Freedom.to.make.life.choices, 2), data = happy_192)
FLC_fit_3 <- lm(Score ~ poly(Freedom.to.make.life.choices, 3), data = happy_192)
FLC_fit_4 <- lm(Score ~ poly(Freedom.to.make.life.choices, 4), data = happy_192)
FLC_fit_5 <- lm(Score ~ poly(Freedom.to.make.life.choices, 5), data = happy_192)

# ANOVA to see how the linear model performs in terms of expression against the 
# polynomial models

anova(FLC_fit_1, FLC_fit_2, FLC_fit_3, FLC_fit_4, FLC_fit_5)

# Looks like the cubic might be the most expressive with a p-value of 0.27, 
# But possibly the squared, as this has a p-value of 0.18

coef(summary(FLC_fit_3))

# Cross validation to try a different perspective and gain more confidence

set.seed (1)
train <- sample (156, 124)

# Linear regression function with subset to fit the training set 

FLC_fit_CV <- lm(Score ??? Freedom.to.make.life.choices , data = happy_192 , subset = train)

# Estimating the response for all the observations using predict function and 
# Calculating mean squared error with mean

mean ((Score - predict (FLC_fit_CV , happy_192))[-train ]^2)

# The mean squared error is 0.80

# Using poly function to estimate quadratic and cubic regression test error

FLC_fit2_CV <- lm(Score ??? poly (Freedom.to.make.life.choices , 2), data = happy_192 ,
                subset = train)
mean ((Score - predict (FLC_fit2_CV , happy_192))[-train]^2)

# The mean generated, and so test error for quadratic is 0.80

FLC_fit3_CV <- lm(Score ??? poly (Freedom.to.make.life.choices , 3), data = happy_192 ,
                subset = train)
mean ((Score - predict (FLC_fit3_CV , ))[-train]^2)

# The mean generated, and so test error the cubed is 2.03

# Trying cross validation again with a different training set to check for 
# consistency and gain confidence in the test error 

set.seed (5)
train <- sample (156, 124)

FLC_fit1_CV2 <- lm(Score ??? Freedom.to.make.life.choices , subset = train)
mean ((Score - predict(FLC_fit1_CV2 , happy_192))[-train ]^2)

# The mean generated, and so test error the linear model is 0.7

FLC_fit2_CV2 <- lm(Score ??? poly (Freedom.to.make.life.choices , 2), data = happy_192 ,
                subset = train)
mean ((Score - predict (FLC_fit2_CV2 , happy_192))[-train]^2)

# The mean generated, and so test error the quadratic model is 0.68

FLC_fit3_CV2 <- lm(Score ??? poly (Freedom.to.make.life.choices , 3), data = happy_192 ,
                subset = train)
mean ((Score - predict (FLC_fit3_CV2 , happy_192))[-train]^2)

# The mean generated, and so test error the cubed model is 0.67

# The cross validation indicates that the squared model may be the most effective

# Predicting if a Score could be 5

FLC_fit_glm <- glm(I(Score > 5) ~ poly(Freedom.to.make.life.choices, 4), data = happy_192,
           family = binomial)

# Predictions

FLC_preds <- predict(FLC_fit_glm, newdata = list(Freedom.to.make.life.choices = FLC.grid), se = T)


# Calculating confidence intervals

FLC_pfit <- exp(FLC_preds$fit) / (1 + exp(FLC_preds$fit))

# non-numeric argument to mathematical function error fixing

is.numeric(happy_192)
is.numeric(FLC_fit_glm)
is.numeric(FLC_preds)

FLC_preds <- as.numeric

FLC_fit_glm <- as.numeric

happy_192 <- as.numeric

head(happy_192)

is.na(happy_192)

is.na(happy_19)

is.numeric(happy_192)

is.data.frame(happy_192)

sapply(happy_192, mode)

# Didn't work, got error object of type 'builtin' is not subsettable

class(happy_192)

print(happy_192)


happy_192[] <- lapply(happy_192, function(x) as.numeric(as.character(x)))
happy_192
sapply(happy_192, class)

happy_192[] <- lapply(happy_192, as.numeric)


# Attempt to calculate confidence interval continued

se.bands.logit <- cbind(FLC_preds$FLC_fit_glm + 2 * FLC_preds$se.fit,
                        FLC_preds$FLC_fit_glm - 2 * FLC_preds$se.fit)

se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))

preds <- predict(FLC_fit_glm, newdata = list(Freedom.to.make.life.choices = FLC.grid),
                 type = "response", se = T)

# Plot I would have made had I been able to create the probability fit for Freedom to make life choices

plot(Freedom.to.make.life.choices, I(Score > 5), xlim = FLC_lims, type = "n",
     ylim = c(0, .2))

points(jitter(Freedom.to.make.life.choices), I((Score > 5/5) ), cex = .5, pch = "|", col = "orchid")

lines(FLC.grid, FLC_pfit, lwd = 2, col = "blue")

matlines(FLC.grid, se.bands, lwd = 1, col = "blue", lty = 3)


# Fitting a step function

table(cut(Freedom.to.make.life.choices, 4))

FLC_fit_sf <- lm(Score ~ cut(Freedom.to.make.life.choices, 4), data = happy_192)

coef(summary(FLC_fit_sf))

# The cut function has picked the cutpoints for Freedom to make life choices scores
# as 0.158, 0.316, and 0.473, for which the intercepts are 0.91, 1.30 and 2.1 respectively. 
# meaning that a Freedom to make life choices 0.158 score could indicate an overall additional value to the score of score of 0.91, 
# Freedom to make life choices score of 0.316 could indicate an overall additional value to the score of 1.30,
# Freedom to make life choices score of 0.473 could indicate an overall additional value to the score of 2.1,

# 14 Regression Splines #####

FLC_fit_rs <- lm(Score ~ bs(Freedom.to.make.life.choices, knots = c(25, 40, 60)), data = happy_192)

FLC_pred_rs <- predict(FLC_fit_rs, newdata = list(Freedom.to.make.life.choices = FLC.grid), se = T)

plot(Freedom.to.make.life.choices, Score, col = "orchid")
lines(FLC.grid, FLC_pred_rs$FLC_fit_rs, lwd = 2)
lines(FLC.grid, FLC_pred_rs$FLC_fit_rs + 2 * FLC_pred_rs$se, lty = "dashed")
lines(FLC.grid, FLC_pred_rs$FLC_fit_rs - 2 * FLC_pred_rs$se, lty = "dashed")

# Error in xy.coords(x, y) : 'x' and 'y' lengths differ

# What do these look like...

FLC_fit_rs

# Why does FLC_fit_rs show NAs? 

# Return to this later... 

FLC_pred_rs
