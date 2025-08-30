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
 
### Let's fit a second model without `acousticness`
model2 = lm(popularity ~ .-acousticness, data = df)

# result 2
summary(model2)

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