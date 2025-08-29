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

## comment: the data is clean

### First fit into the model

model1 = lm(popularity ~ ., data=df)

# result
summary(model1)

### comment: 
# 1. p-value for the F-test very small (< 2.2e-16) shows that at least one of these predictors explain popularity.
# 2. p-value for each t-test shows that the intercept, danceability, energy, acousticness, instrumentalness and tempo statistically
# have an influence on a song's popularity.
# 3. energy, acousticness and instrumentalness have negative impact whereas the intercept, danceability
# and tempo have positive impact on popularity.
# 4. adjusted R-squared = 0.0113 (~ 1.13%) shows the model explains little about the relationship of interest.

