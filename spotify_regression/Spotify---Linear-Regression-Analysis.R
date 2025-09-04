#------------------------------
# Introduction
#------------------------------

# Linear Regression Analysis: Spotify
# Source: Prof. Kelly Ramsay - York University

# Question: What drives Spotify song popularity?

#------------------------------
# 1. Preliminary steps:
#------------------------------

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

### Comment: the data is clean. Except tempo which looks close to normal, the others look skewed(duration_ms - extremely, danceability - slightly, etc.)
# popularity has an extreme number of observation at 0.

#------------------------------
# 2. Model fitting
#------------------------------

model1 = lm(popularity ~ ., data=df)

# result 1
summary(model1)

# anova 1
null_model1 = lm(popularity ~ 1, data = df)
anova(null_model1, model1)

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

#------------------------------
# 3. Assumption check
#------------------------------

### Checking normality of the error term

# Check normality of the error term using the QQ-plot for the residuals
if (!require("car")) install.packages("car")

# Student residual
student_res1 = rstudent(model1)

# Plot QQ-plot
qqPlot(student_res1, distribution="norm", pch=1, col="red", xlab="Theoretical normal quantiles", ylab="Sample quantiles" )

### comment: the distribution of the errors is not so great, light-tailed. Check by plotting a histogram
hist(student_res1, breaks = 100, xlab="Residuals", main="Histogram of residuals", freq=FALSE)
curve(dnorm(x, mean=mean(student_res1), sd=sd(student_res1)), 
      col="red", lwd=2, add=TRUE)

# Checking other assumptions for the errors e.g independence, mean = 0, constant variance (Homoscedasticity)
plot(model1$fitted.values, res1, xlab = "Fitted values", ylab = "Residuals")
abline(h=0)

# Check linearity
covars = colnames(df)[-1]

for (i in 1:length(covars)) {
  k = covars[i]
  temp_x = data.matrix(df[k])
  plot(temp_x, res1, xlab = covars[i], ylab = "Studentized residuals")
}

### comment: 
# 1. As the fitted popularity score increases, the variance of the residuals increases.
# 2. The mean of the errors seem to be positive, thus the model seems to underestimate the response.
# 3. The errors are likely to not distributed identically, and linearity is also not guaranteed.

#------------------------------
# 4. Model improvement
#------------------------------

# Recall that the histogram for `popularity` shows a great number of observations with popularity score equal 0
# This may cause a problem for our model, thus we will remove those points for now and look closer at those with popularity score = 0 later

df2 = subset(df, popularity > 0)
# Check for outliers/influencial/leverage points

# 1. Hat matrix
# X=model.matrix(model2)
# hat=X%*%solve(t(X)%*%X)%*%t(X)
# diag(hat)
# p=ncol(X)
# n=nrow(X)
# out_1=which(diag(hat)>2*p/n) # those larger than this rule of thumb are outliers
# plot(sort(diag(hat)[out_1]))
# abline(h=2*p/n)

# 2. Depth
depths=ddalpha::depth.projection(df2,df2)
plot(sort(depths, decreasing = F))

# look closer at the left end
plot(sort(depths, decreasing = F)[1:1000])

# comment: we notice a small crack at around 0.022, we look closer
plot(sort(depths, decreasing = F)[1:100])

# the point with lowest depth
df2[which.min(depths),]
# --> turn out to be the one with extreme duration (5237295 ~ 1.45 hours)

# cutoff point = 10%, we found those points classified as outliers
df2[which(depths < 0.1),]

# We will remove the outliers for now. Robust regression may be used to tackle this problem better
df2 = df2[-which(depths < 0.1),]

# Fit a second model with this modified df

model2 = lm(popularity ~., data = df2)

# Result 2

summary(model2)

# Assumption check for model 2

res2 = rstudent(model2)

# Plot QQ-plot
qqPlot(res2, distribution="norm", pch=1, col="red", xlab="Theoretical normal quantiles", ylab="Sample quantiles" )

# many points below the line is now onto the line, but the distribution is still light-tailed.

# Plot of the residuals against the fitted value
plot(model2$fitted.values, res2, xlab = "Fitted values", ylab = "Residuals")
abline(h=0)

# Check linearity
for (i in 1:length(covars)) {
  k = covars[i]
  temp_x = data.matrix(df2[k])
  plot(temp_x, res2, xlab = covars[i], ylab = "Studentized residuals")
}

### Comments:
# 1. the model tends to underestimate popularity score that are less than $25$.
# 2. for values that are greater than $20$, we see that the variance of residuals increases as popularity score increases.
# 3. the assumption of the errors being identically distributed is still not satisfied, but the normality assumption is better respected.
# 4. the assumption for linearity is also better supported (improvement in the plots of each covars against residuals).
# 4. the model better explains the response, with significantly increased R_adjusted = 0.43.

# Addressing multicollinearity:

# correlation matrix
X = model.matrix(model2)
corrplot::corrplot(cor(X[,-1]), main = "Correlation Matrix")
# comments: `energy` is clearly shown to negatively correlate with `acousticness`.

# using VIF 
car::vif(model2) #since our data is all numeric, we use the default term

# comment: the correlation matrix together with VIFs show that `acousticness` and `energy` are correlated, though the correlation is mild

### Let's fit a second model without `acousticness`
model3 = lm(popularity ~ .-acousticness, data = df2)

# result 3
summary(model3)

# change in coefficients
model3$coefficients - model2$coefficients[-5] #omit `acousticness` in model 1
# coefficient shift: energy (-5.6 to -1.96 -> large increase), danceability (2.2 to 2.7)

# Assumption check for model 3:

res3 = rstudent(model3)

# Plot QQ-plot
qqPlot(res3, distribution="norm", pch=1, col="red", xlab="Theoretical normal quantiles", ylab="Sample quantiles" )

# Plot of the residuals against the fitted value
plot(model3$fitted.values, res3, xlab = "Fitted values", ylab = "Residuals")
abline(h=0)

# still, homoscedasticity remains but the errors now center around 0

# Check linearity
covars3 = c("duration_ms", "danceability", "energy", "instrumentalness", "liveness","tempo")
for (i in 1:length(covars)) {
  k = covars3[i]
  temp_x = data.matrix(df2[k])
  plot(temp_x, res2, xlab = covars3[i], ylab = "Studentized residuals")
}

# check VIFs
car::vif(model3)

### comments:
# 1. Adjusted R-squared drops, this makes sense as we remove `acousticness` and it is a significant predictor
# 2. `duration_ms` and `liveness` were insignificant before, but now significant. This may show
# a relationship between `liveness` and `acousticness` since acoustic tracks are often recorded live.
# 3. Removing `acousticness`, `danceability` grows. This could mean acousticness was “masking” part of danceability’s effect,
# since acoustic songs are often less danceable.
# 4. VIFs decrease --> resolve multicollinearity
