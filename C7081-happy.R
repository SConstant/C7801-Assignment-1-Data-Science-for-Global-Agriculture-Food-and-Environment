## HEADER ####
## Who: Sophie Constant
## https://dsgarage.netlify.app/
## What: 2.1 Question, explore, analyze
## Last edited: <DATE TODAY in yyyy-mm-dd format)


## CONTENTS ####
# 00 Setup
# 01 dataframes
# 02 Question
# 03 EDA
# 04 Correlation plots 
# 05 Assumption testing for Multiple linear regression
# 06 Log transformed variables and assumption testing for Multiple linear regression 
# 07 multiple Linear Regression 
# 08 Possible ANOVA because post hoc test/effect attribution
# 09 Variable Selection (choice depending on p value from multiple linear regression) because why not? 
# 10 Evaluation of Variable selection (possibly root mean square? Partial least squares)
# 11 Some kind of tree


# 00 Setup ####

setwd("C:/Users/Sophi/OneDrive/Documents/Data Science/Module 1/Assignment 1.2")

install.packages("gclus")
install.packages("PerformanceAnalytics")
install.packages("psych")

library(openxlsx)
library(dplyr)
library(car)
library(gclus)
library(PerformanceAnalytics)
library(psych)


happy_19 <- read.csv("2019.csv")

summary(happy_19)

happy_19 <- as.data.frame(lapply(happy_19, as.numeric))

happy_19


# 01 Dataframes and arguments ####

# All the data for 20119

happy_19 <- read.csv("2019.csv")

summary(happy_19)

# Scores, and independant variables from 2019

happy_192 <- happy_19[3:9]

head(happy_192)

# All the linear regression arguments

GDP_lm <- lm(formula = happy_192$Score ~ happy_192$GDP.per.capita)

SS_lm <- lm(formula = happy_192$Score ~ happy_192$Social.support)

HLE_lm <- lm(formula = happy_192$Score ~ happy_192$Healthy.life.expectancy)

FLC_lm <- lm(formula = happy_192$Score ~ happy_192$Freedom.to.make.life.choices)

G_lm <- lm(formula = happy_192$Score ~ happy_192$Generosity)

PC_lm <- lm(formula = happy_192$Score ~ happy_192$Perceptions.of.corruption)

# See log transformed variables for that which has been log transformed

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
# Bins on th eright are generally a bit bigger

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


abline(h = c(0, s_uci, s_lci),
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

(HLE_uci <- summary(FLC_lm)$sigma*1.96) 
(HLE_lci <- -summary(FLC_lm)$sigma*1.96) 


abline(h = c(0, HLE_uci, HLE_lci),
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
happy_192_new[,c(3,5,7)] <- log(happy_192_new[,c(3,5,7)])

head(happy_192)

# Fixing -Inf values in natural log transformed columns 

happy_192_new$Perceptions.of.corruption
happy_192_new$Freedom.to.make.life.choices
happy_192_new$Social.support

happy_192_new$Perceptions.of.corruption

is.na(happy_192_new) <- sapply(happy_192_new, is.infinite)

# Exploration of natural log transformed residuals 

# Perceptions of corruption 

plot(y = happy_192_new$Score, x = happy_192_new$Perceptions.of.corruption,
     ylab = "Score", xlab = "Perceptions of Corruption log",
     main = "Happiness residuals (Score and Perceptions of corruption log)",
     pch = 20, col = "orchid", cex = 1)

PC_lm_log <- lm(formula = happy_192_new$Score ~ happy_192_new$Perceptions.of.corruption)

PC_lm_log

abline(reg = PC_lm_log)

text(x = -4.5, y = 7, labels = "y = 6.1578, x = 0.2961") 

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
# Plot looks a bit bunched up to the right/skewed to the left
# q-q plot looks a bit wavy with outliers at 152 and 153
# q-q plot also has a little skew to the top

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
# Plot is quite skewed, more points below the line and quite bunched to the right
# Outliers on qq plot are 102 151, bunched up in the middle and a little skew to the top
# Frequency histogram, not quite bell shaped
# Density histogram looks ok, slight deviation of residuals in th epositive bins

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


# Homoscedasticity testing on natural log transformed variables

# log transformed Social Support
# Still skewed to the right
# In a side by side comparison looks worse than the SS_lm when not log transformed
# Human/Neurodiverse error - previous homoscedasticity plot included upper and lower confidence interval from wrong argument

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


# Perception of corruption natural log transformed


# Plot of residuals for natural log transformed Perception of corruption
# Improvement, with points looking more evenly distributed,
# A few scattered outliers but will accept

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



# 07 Multiple Linear Regression ####

# All the variables, including un - transformed variables which don't conform to 
# required assumptions 
# residuals not Gaussian: Freedom to make life choices and Perception of corruption
# Homoscedasticity: Perception of corruption, and Social support

H_lm_fit <- lm(Score ~ ., data = happy_192)
summary(H_lm_fit)




happy_192$

