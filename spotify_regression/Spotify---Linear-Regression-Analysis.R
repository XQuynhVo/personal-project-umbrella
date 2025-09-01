### Introduction
# Linear Regression Analysis: Spotify
# Question: What drives Spotify song popularity?

# Source: Prof. Kelly Ramsay - York University

### Preliminary steps:

#import data
df <- read.csv("spotify.csv")

#first look
head(df)

#look at the structure of df
str(df)

#count missing values in each column
colSums(is.na(df))

#summary statistics
summary(df)

#distribution of each feature
for (i in 1:length(colnames(df))) {
  plotName = cat("Distribution of ", colnames(df)[i])
  hist(data.matrix(df[colnames(df)[i]]),
       main = plotName,
       xlab = colnames(df)[i])
}

## comment: the data is clean. Except tempo which looks close to normal, the others look skewed(duration_ms - extremely, danceability - slightly, etc.)

### First fit the model

model1 = lm(popularity ~ ., data=df)

# result 1
summary(model1)

### comments: 
# 1. p-value for the F-test very small (< 2.2e-16) shows that at least one of these predictors explain popularity.
# 2. p-value for each t-test shows that the intercept, danceability, energy, acousticness, instrumentalness and tempo statistically
# have a significant influence on a song's popularity.
# 3. energy, acousticness and instrumentalness have negative impact whereas the intercept, danceability
# and tempo have positive impact on popularity.(*) 
# 4. Even after accounted for a large number of predictors, the
# adjusted R-squared = 0.0113 (~ 1.13%) shows the model explains little about the relationship of interest.
# A song popularity may be not be captured by these characteristics alone.

#(*) An example for the interpretation for the estimated coefficient for `danceability` is as follow: 
# Keeping all other predictors constant, a unit increase in `danceability` causes an average of 2.2 unit in song's popularity

# confident intervals for all covariates
 confint(model1, level=0.95)

### comments: 
# the CIs for each covariates show us that with 95% confidence, they can capture the real coefficients for these covariates
 
#------------------------------
# 3. Assumption check
#------------------------------

### Checking normality

# Check normality of the error term using the QQ-plot for the residuals
if (!require("car")) install.packages("car")

# Plot QQ-plot
qqPlot(model1$residuals, distribution="norm", pch=1, col="red", xlab="Theoretical normal quantiles", ylab="Sample quantiles" )

### the distribution of the errors is not so great, light-tailed. Check by plotting a histogram
hist(model1$residuals,breaks = 100, xlab="Residuals", main="Histogram of residuals", freq=FALSE, xlim=c(-60,60))
curve(dnorm(x, mean=mean(model1$residuals), sd=sd(model1$residuals)), 
      col="red", lwd=2, add=TRUE)

### Checking other assumptions for the errors e.g independence, mean = 0, constant variance

plot(model1$fitted.values, model1$residuals, xlab = "Fitted values", ylab = "Residuals")

### comment: 
# 1. As the fitted popularity score increases, the variance of the residuals increases.
# 2. The mean of the errors seem to be positive, thus the model seems to underestimate the response.

#------------------------------
# 4. Model improvement
#------------------------------

# Recall that the histogram for `popularity` shows a great number of observations with popularity score equal 0
# This may cause a problem for our model, thus we will remove them for now and look closer at those with popularity score = 0 later

df2 = subset(df, popularity > 0)

# Fit a second model with this modified df

model2 = lm(popularity ~., data = df2)

# Result 2

summary(model2)

# Assumption check for model 2

# Plot QQ-plot
qqPlot(model2$residuals, distribution="norm", pch=1, col="red", xlab="Theoretical normal quantiles", ylab="Sample quantiles" )

# many points below the line is now onto the line, but the distribution is still light-tailed.

# Plot of the residuals against the fitted value
plot(model2$fitted.values, model2$residuals, xlab = "Fitted values", ylab = "Residuals")

### Comments:
# 1. the model tends to underestimate popularity score that are less than $20$.
# 2. for values that are greater than $20$, we see that the variance of residuals increases as popularity score increases.
# 3. the assumption of the errors being identically distributed is still not satisfied, but the normality assumption is better respected.
# 4. the model better explains the response, with significantly increased R_adjusted = 0.45

# Addressing multicollinearity:

### Let's fit a second model without `acousticness`
model3 = lm(popularity ~ .-acousticness, data = df2)

# result 3
summary(model3)

# change in coefficients
model2$coefficients - model1$coefficients[-5] #omit `acousticness` in model 1
# coefficient shift: energy (-5.6 to -1.96 -> large increase), danceability (2.2 to 2.7)

# comments:
# 1. Adjusted R-squared drops, this makes sense as we remove `acousticness` and it is a significant predictor
# 2. `duration_ms` and `liveness` were insignificant before, but now weakly significant. This may show
# a relationship between `liveness` and `acousticness` since acoustic tracks are often recorded live.
# 3. Removing `acousticness`, which has a negative effect on popularity, coefficient of `energy` increases.
# This shows `acousticness` and `energy` are correlated (acoustic songs tend to be less energetic).
# 4. Removing `acousticness`, `danceability` grows. This could mean acousticness was “masking” part of danceability’s effect,
# since acoustic songs are often less danceable.

# correlation matrix
cor(subset(df, select = -popularity))

# comments: check by correlation matrix, `energy` is shown to negatively correlate with `acousticness`.
