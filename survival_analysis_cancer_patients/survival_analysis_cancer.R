rm(list = ls())
setwd("R/RScripts/courses/4130K/Project")

library(readxl)
library(survival)
library(ggplot2)
library(dplyr)
library(corrplot)
library(survminer)

# Load the dataset
df = read_excel("Datasets/seer1.xlsx")
View(df)

# Size
dim(df)

# Datatype
str(df)

# It can create less errors if we change the column names as by convention
#Names start with a 
colnames(df) = make.names(colnames(df))
colnames(df)

# Change "event" variable to numerical value 0,1
df$event = ifelse(df$event == TRUE, 1, 0)

# Check null value
print(colSums(is.na(df)))
#no null values
#df = na.omit(df)


# Numeric variables
# Take numeric values out
numeric_df <- df[, sapply(df, is.numeric)]
# Numeric cols
colnames(numeric_df)

# Summary statistics of the numeric variables
numeric_vars = colnames(numeric_df)
for (i in numeric_vars) {
  cat("Summary statistics of ", i, ": \n", summary(df[,i]), "\n\n")
}

# Explore the relationship between numerical variables with survival time
# Ex: Age vs Survival time
ggplot(df, aes(x = Age, y = time, color = factor(event))) +
  geom_point(alpha = 0.6) +
  labs(title = "Survival Time vs. Age", x = "Age", y = "Survival Time", color = "Event")

# Correlation matrix for numerical variables
cor_matrix = cor(numeric_df)
print(cor_matrix)

# Correlation plot of the correlation matrix
corrplot(cor_matrix, type = "upper", 
         tl.col = "black", tl.srt = 30)

# Categorical variables
cat_df = df[, !sapply(df, is.numeric)]
cat_vars = colnames(cat_df)

for (i in cat_vars) {
  cat("Counts of each category in ")
  print(table(df[,i]))
}
# This step is to make sure categorical vars are treated as factor
df[, cat_vars] = lapply(df[,cat_vars], as.factor)

# Plot survival times vs Grade
ggplot(df, aes(x = `Grade`, y = time)) +
  geom_boxplot() +
  labs(title = "Survival Time by Grade", x = "Grade", y = "Survival Time")

# Censoring Analysis 
# Extracting the number of events and censored observations
num_events <- sum(df$event == TRUE) # Number of individuals with events (status = 1)
num_censored <- sum(df$event == FALSE) # Number of censored observations (status = 0)

# Print censoring summary
print("Censoring Summary: ")
cat("Number of events (deaths):", num_events, "\n")
cat("Number of censored observations:", num_censored, "\n")
cat("Censoring proportion:", round(num_censored / (num_events + num_censored), 3), "\n")

# Distribution of survival time not accounting for censoring
hist(df$time, freq = TRUE,
     main= "Distribution of Overall Survival Times",
     xlab= "Time", ylab = "Frequency",
     xlim=c(0, 107),
     col = "lightblue")
abline(v = mean(df$time), col="darkblue", lwd = 2, lty = "dashed")
cat("Mean overall survival time:",mean(df$time))

# Distribution of survival time based on censored vs non-censored data
censored_data = df[df$event==0,]
hist(censored_data$time, freq = TRUE,
     main= "Distribution of Survival Times for censored data",
     xlab= "Time", ylab = "Frequency",
     xlim=c(0, 107),
     col = "lightblue")
abline(v = mean(censored_data$time), col="darkblue", lwd = 2, lty = "dashed")
cat("Mean censored survival time:",mean(censored_data$time))

non_censored_data = df[df$event==1,]
hist(non_censored_data$time, freq = TRUE,
     main= "Distribution of Survival Times for noncensored data",
     xlab= "Time", ylab = "Frequency",
     xlim=c(0, 107),
     col = "lightblue")
abline(v = mean(non_censored_data$time), col="darkblue", lwd = 2, lty = "dashed")
cat("Mean censored survival time:",mean(non_censored_data$time))

#Survival analysis
# Fit a survival curve
s_obj <- Surv(df$time, df$event)
s_fit <- survfit(s_obj ~ 1, data = df)

# Summarize the survival curve
summary_fit <- summary(s_fit)
summary_fit

# Plot the survival curve
ggsurvplot(
  s_fit, 
  data = df, 
  conf.int = TRUE, 
  xlab = "Time (Months)", 
  ylab = "Survival Probability", 
  title = "Kaplan-Meier Survival Curve"
)

# Estimate the 25th, 50th, and 75th percentiles of the survival time
times <- summary_fit$time
probs <- summary_fit$surv

percentile_25 <- times[which.min(abs(probs - 0.75))] # 75% survival time
percentile_75 <- times[which.min(abs(probs - 0.25))] # 25% survival time
median_survival <- times[which.min(abs(probs - 0.5))]

cat("Estimated median survival time:", median_survival, "days\n")
cat("Estimated 25th percentile survival time:", percentile_25, "days\n")
cat("Estimated 75th percentile survival time:", percentile_75, "days\n")
# We got the same values for the all percentiles because the survival probability is
# all higher than 0.75


# Faceted survival curves

# Create a dictionary containing the survival summary of all the categorical variables
surv_df_dict = list()
for (i in cat_vars) {
  # Create a key
  key = i
  
  # Fit survival model
  formula = as.formula(paste("s_obj ~", i))
  fit = survfit(formula, data = df)
  # print(formula)
  
  # Extract survival data for plotting
  summ = summary(fit, data = df)
  
  # Convert survival objects into dataframes 
  surv_df = data.frame(
    time = summ$time,
    surv = summ$surv,
    strata = summ$strata
  )
  
  value = surv_df 
  
  # Add the key-value pair to the dictionary
  surv_df_dict[[key]] = value
}

# print(surv_df_dict)

# Plot survival curves by Race
p_race = ggplot(surv_df_dict[["Race"]], aes(x = time, y = surv, color = strata)) +
  geom_step() +
  labs(title = "Survival Curves by Race", x = "Months", y = "Survival Probability") +
  theme_minimal()
print(p_race)

# Plot survival curves by Marital Status
p_MS = ggplot(surv_df_dict[["Marital.Status"]], aes(x = time, y = surv, color = strata)) +
  geom_step() +
  labs(title = "Survival Curves by Marital Status", x = "Months", y = "Survival Probability") +
  theme_minimal()
print(p_MS)

# Plot survival curves by T Stage
p_TStage = ggplot(surv_df_dict[["T.Stage"]], aes(x = time, y = surv, color = strata)) +
  geom_step() +
  labs(title = "Survival Curves by T Stage", x = "Months", y = "Survival Probability") +
  theme_minimal()
print(p_TStage)

# Plot survival curves by N Stage
p_NStage = ggplot(surv_df_dict[["N.Stage"]], aes(x = time, y = surv, color = strata)) +
  geom_step() +
  labs(title = "Survival Curves by N Stage", x = "Months", y = "Survival Probability") +
  theme_minimal()
print(p_NStage)

# Plot survival curves by 6th Stage
p_X6Stage = ggplot(surv_df_dict[["X6th.Stage"]], aes(x = time, y = surv, color = strata)) +
  geom_step() +
  labs(title = "Survival Curves by 6th Stage", x = "Months", y = "Survival Probability") +
  theme_minimal()
print(p_X6Stage)

# Plot survival curves by differentiate
p_diff = ggplot(surv_df_dict[["differentiate"]], aes(x = time, y = surv, color = strata)) +
  geom_step() +
  labs(title = "Survival Curves by Differentiation", x = "Months", y = "Survival Probability") +
  theme_minimal()
print(p_diff)

# Plot survival curves by Grade
p_grade = ggplot(surv_df_dict[["Grade"]], aes(x = time, y = surv, color = strata)) +
  geom_step() +
  labs(title = "Survival Curves by Grade", x = "Months", y = "Survival Probability") +
  theme_minimal()
print(p_grade)

# Plot survival curves by A Stage
p_AStage = ggplot(surv_df_dict[["A.Stage"]], aes(x = time, y = surv, color = strata)) +
  geom_step() +
  labs(title = "Survival Curves by A Stage", x = "Months", y = "Survival Probability") +
  theme_minimal()
print(p_AStage)

# Plot survival curves by Estrogen Status
p_ES = ggplot(surv_df_dict[["Estrogen.Status"]], aes(x = time, y = surv, color = strata)) +
  geom_step() +
  labs(title = "Survival Curves by Estrogen Status", x = "Days", y = "Survival Probability") +
  theme_minimal()
print(p_ES)

# Plot survival curves by Progesterone Status
p_PS = ggplot(surv_df_dict[["Progesterone.Status"]], aes(x = time, y = surv, color = strata)) +
  geom_step() +
  labs(title = "Survival Curves by Progesterone Status", x = "Months", y = "Survival Probability") +
  theme_minimal()
print(p_PS)

# By age
# Categorize age
df$Age_group = ifelse(df$Age < median(df$Age), "Younger", "Older") #30-44, 45-60, >60
# Ensure age are treated as categorical
df$Age_group = as.factor(df$Age_group)
# Fit survival curve by age group
fit_age <- survfit(s_obj ~ Age_group, data = df)
# Extract survival summary
summary_age <- summary(fit_age)
# Convert survival objects into data frames correctly
surv_age_df <- data.frame(
  time = summary_age$time,
  surv = summary_age$surv,
  strata = summary_age$strata
)

# Plot survival curves by age group
p_age <- ggplot(surv_age_df, aes(x = time, y = surv, color = strata)) +
  geom_step() +
  labs(title = "Survival Curves by Age Group", x = "Months", y = "Survival Probability") +
  theme_minimal()
# Print the plots
print(p_age)

#Race: Black people have lowest survival probability followed by White people and other races have the highest survival probability
#Marital status: Married people have highest survival probability while separated people have the lowest survival probability (for separated group: more events of censored data were observed at the same -> big drop)
#T-stage: T1 stage has highest survival probability while T4 has the most drastic drop in the survival probability
#N-stage (no crossing): N1 has the highest survival probability followed by N2 and lastly N3. N3 has the steepest drop in survival
#6th-stage: IIA has the highest survival probability while IIIC has the lowest probability. IIIC and IIIB show steepest drop in probability. IIB and IIIA show not much difference
#Differentiate: Poorly differentiated cases show the lowest and steepest drop in survival probability. Moderately and well differentiated cases have closely intertwining pattern up until the 50 days time point
#Grade: Grade 3 shows the lowest and steepest drop in survival probability. Grade 1 and 2 cases have closely intertwining pattern up until the 50 days time point (similar measure of cell differentiation)
#A-stage: distant cases have higher survival probability but the regional cases show drastic drop (multiple events happening at the same time?)
#Estrogen status: positive has lower survival and steep drop in probability
#Progesterone status: positive has lower survival and steeper drop
#Age group: 45-60 shows the highest survival probability overall. The younger age groups have higher survival probability than the older age group at the beginning but their survival quickly declines over time and becomes the lowest at the end.


# Note that the survival curves for different categories in A Stage, ES and PS do not cross
# Log-rank Test can be used to verify if their survival times are indeed different

for (i in c("N.Stage", "A.Stage", "Estrogen.Status", "Progesterone.Status")) {
  cat("Log-rank Test for: ", i, "\n")
  formula = as.formula(paste("s_obj ~", i))
  print(survdiff(formula, data=df))
}

# The difference in survival time depending on the location of the tumors, whether they
# are distant or regional is not significant!

# Cox proportional hazards model
# As we do not know about the underlying distribution of survival times yet

cph_fit = coxph(Surv(time,event) ~.-Age_group, data=df)
# Display the summary of the Cox model.
summary(cph_fit)

# Note NA values for Grade, and 6th.Stage

# Check the distribution of event occurrences across levels of Grade 
event_counts_grade = table(df$Grade, df$event)
barplot(event_counts_grade, 
        beside=TRUE, 
        legend=rownames(event_counts_grade), 
        main = "Distribution of event occurrences across levels of Grade",
        xlab="Grade")
# There are very few observations for both censored and non_censored data for grade 4

# Similarly, we want to check the distribution of event occurrences across levels of Grade 
event_counts_6th_stage = table(df$X6th.Stage, df$event)
barplot(event_counts_6th_stage, 
        beside=TRUE, 
        legend=rownames(event_counts_6th_stage), 
        main = "Distribution of event occurrences across levels of 6th Stage",
        xlab="6th Stage")
# There are very few observations for both censored and non_censored data for grade 4

# Run the Cox model considering Grade only
cph_fit_grade = coxph(Surv(time,event) ~Grade, data=df)
# Display the summary of the Cox model.
summary(cph_fit_grade)

# Run the Cox model considering 6th Stage only
cph_fit_6 = coxph(Surv(time,event) ~X6th.Stage, data=df)
# Display the summary of the Cox model.
summary(cph_fit_6)

# Check multicollinearity of 6th stage with T-stage and N-stage
table(df$X6th.Stage, df$T.Stage)
table(df$X6th.Stage, df$N.Stage)
#If a category in one variable always corresponds to a specific category (or small subset) in another variable, 
#then one variable can be perfectly predicted from the other(s) - a strong sign of collinearity.

# Check multicollinearity of Differentiate and Grade
table(df$differentiate,df$Grade)

# Remove collinear variables
cox_removed <- coxph(Surv(time, event) ~ . -T.Stage -N.Stage -Grade -Age_group, data = df)
summary(cox_removed)

# Check assumption
ph <- cox.zph(cox_removed)
print(ph)

# Plot Schoenfeld residuals
plot(ph)

#Estrogen status violates the proportional hazards assumption
#The GLOBAL p-value = 0.069 is greater than 0.05, meaning the overall model 
#does not significantly violate the PH assumption

# Remove Estrogen status variable, use it as a reference variable
cox1 <- coxph(Surv(time, event) ~ . -T.Stage -N.Stage -Grade -Age_group- Estrogen.Status + strata(Estrogen.Status), data = df)
summary(cox1)

# Check Cox proportion model assumption again
ph1<-cox.zph(cox1)
print(ph1)

plot(ph1)
