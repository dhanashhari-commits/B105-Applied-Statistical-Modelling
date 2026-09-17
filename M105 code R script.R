# 1. IMPORT DATASET

library(readr)
bank <- read_csv("C:/Users/User/Downloads/bank+marketing/bank.csv")
View(bank)

head(bank)
tail(bank)
dim(bank)
names(bank)
str(bank)
summary(bank)


# 2.CHECK DATA QUALITY
# Check missing values
sum(is.na(bank))
colSums(is.na(bank))

# Check duplicate observations
sum(duplicated(bank))

# Display duplicates if any
bank[duplicated(bank), ]

# Check blank values in character variables
sapply(bank, function(x) sum(x == "", na.rm = TRUE))


# 3. CHECK CATEGORICAL VALUES
table(bank$job)
table(bank$marital)
table(bank$education)
table(bank$default)
table(bank$housing)
table(bank$loan)
table(bank$contact)
table(bank$month)
table(bank$poutcome)
table(bank$y)


# 4. Count "unknown" categories
sapply(bank, function(x) {
  if(is.character(x)) sum(x == "unknown") else NA
})

#  5. CONVERT CATEGORICAL VARIABLES TO FACTORS
factor_variables <- c("job", "marital", "education", "default",
                      "housing", "loan", "contact", "month",
                      "poutcome", "y")
bank[factor_variables] <- lapply(bank[factor_variables], factor)
str(bank)



# 6. DESCRIPTIVE STATISTICS
# Overall summary
summary(bank)

# Numerical variables
numeric_variables <- c("age", "balance", "day", "duration",
                       "campaign", "pdays", "previous")
summary(bank[numeric_variables])

# Means
sapply(bank[numeric_variables], mean, na.rm = TRUE)

# Medians
sapply(bank[numeric_variables], median, na.rm = TRUE)

# Standard deviations
sapply(bank[numeric_variables], sd, na.rm = TRUE)

# Minimum values
sapply(bank[numeric_variables], min, na.rm = TRUE)

# Maximum values
sapply(bank[numeric_variables], max, na.rm = TRUE)



# 7. TARGET VARIABLE ANALYSIS
subscription_frequency <- table(bank$y)
subscription_frequency

subscription_percentage <- prop.table(subscription_frequency) * 100
round(subscription_percentage, 2)

barplot(subscription_frequency,
        main = "Term Deposit Subscription",
        xlab = "Subscription Status",
        ylab = "Number of Customers")


# 8. EXPLORATORY DATA ANALYSIS - AGE
summary(bank$age)

hist(bank$age,
     breaks = 30,
     main = "Distribution of Customer Age",
     xlab = "Age")

boxplot(bank$age,
        main = "Boxplot of Customer Age",
        ylab = "Age")

boxplot(age ~ y,
        data = bank,
        main = "Age by Subscription Status",
        xlab = "Subscription",
        ylab = "Age")

aggregate(age ~ y, data = bank, FUN = mean)
aggregate(age ~ y, data = bank, FUN = median)


# 9. EXPLORATORY DATA ANALYSIS - BALANCE
summary(bank$balance)

hist(bank$balance,
     breaks = 30,
     main = "Distribution of Customer Balance",
     xlab = "Account Balance")

boxplot(bank$balance,
        main = "Boxplot of Customer Balance",
        ylab = "Balance")

boxplot(balance ~ y,
        data = bank,
        main = "Balance by Subscription Status",
        xlab = "Subscription",
        ylab = "Balance")

aggregate(balance ~ y, data = bank, FUN = mean)
aggregate(balance ~ y, data = bank, FUN = median)



# 10. EXPLORATORY DATA ANALYSIS - CALL DURATION
summary(bank$duration)

hist(bank$duration,
     breaks = 30,
     main = "Distribution of Call Duration",
     xlab = "Duration in Seconds")

boxplot(bank$duration,
        main = "Boxplot of Call Duration",
        ylab = "Duration in Seconds")

boxplot(duration ~ y,
        data = bank,
        main = "Call Duration by Subscription Status",
        xlab = "Subscription",
        ylab = "Duration in Seconds")

aggregate(duration ~ y, data = bank, FUN = mean)
aggregate(duration ~ y, data = bank, FUN = median)




# 11. EXPLORATORY DATA ANALYSIS - CAMPAIGN CONTACTS
summary(bank$campaign)

hist(bank$campaign,
     breaks = 30,
     main = "Distribution of Campaign Contacts",
     xlab = "Number of Contacts")

boxplot(campaign ~ y,
        data = bank,
        main = "Campaign Contacts by Subscription",
        xlab = "Subscription",
        ylab = "Number of Contacts")

aggregate(campaign ~ y, data = bank, FUN = mean)
aggregate(campaign ~ y, data = bank, FUN = median)



# 12. CATEGORICAL EXPLORATORY ANALYSIS - HOUSING LOAN
housing_table <- table(bank$housing, bank$y)
housing_table

round(prop.table(housing_table, margin = 1) * 100, 2)

barplot(housing_table,
        beside = TRUE,
        main = "Housing Loan and Term Deposit Subscription",
        xlab = "Housing Loan",
        ylab = "Number of Customers",
        legend.text = TRUE)



# 13. CATEGORICAL EXPLORATORY ANALYSIS - PERSONAL LOAN
loan_table <- table(bank$loan, bank$y)
loan_table

round(prop.table(loan_table, margin = 1) * 100, 2)

barplot(loan_table,
        beside = TRUE,
        main = "Personal Loan and Term Deposit Subscription",
        xlab = "Personal Loan",
        ylab = "Number of Customers",
        legend.text = TRUE)



# 14. CATEGORICAL EXPLORATORY ANALYSIS - PREVIOUS CAMPAIGN
poutcome_table <- table(bank$poutcome, bank$y)
poutcome_table

round(prop.table(poutcome_table, margin = 1) * 100, 2)

barplot(poutcome_table,
        beside = TRUE,
        main = "Previous Campaign Outcome and Subscription",
        xlab = "Previous Campaign Outcome",
        ylab = "Number of Customers",
        legend.text = TRUE)


# ============================================================
# 15. HYPOTHESIS TEST 1: HOUSING LOAN VS SUBSCRIPTION
# H0: Housing loan status and subscription are independent.
# H1: Housing loan status and subscription are associated.
# Significance level: alpha = 0.05
# ============================================================

housing_table <- table(bank$housing, bank$y)
housing_table

# Run Pearson Chi-square test
housing_test <- chisq.test(housing_table)
housing_test

# Check expected frequencies
housing_test$expected

# Check assumption: all expected frequencies should normally be >= 5
all(housing_test$expected >= 5)

# Examine residuals
housing_test$residuals

# Subscription percentages within housing groups
round(prop.table(housing_table, margin = 1) * 100, 2)

# Effect size: Phi coefficient
housing_phi <- sqrt(as.numeric(housing_test$statistic) /
                      sum(housing_table))
housing_phi

# Decision
housing_p <- housing_test$p.value
if(housing_p < 0.05) {
  print("Reject H0: Housing loan status is significantly associated with subscription.")
} else {
  print("Fail to reject H0: There is insufficient evidence of an association.")
}


# ============================================================
# 16. HYPOTHESIS TEST 2: PERSONAL LOAN VS SUBSCRIPTION
# H0: Personal loan status and subscription are independent.
# H1: Personal loan status and subscription are associated.
# ============================================================

loan_table <- table(bank$loan, bank$y)
loan_table

loan_test <- chisq.test(loan_table)
loan_test

# Check expected frequencies
loan_test$expected
all(loan_test$expected >= 5)

# Examine residuals
loan_test$residuals

# Percentages
round(prop.table(loan_table, margin = 1) * 100, 2)

# Phi effect size
loan_phi <- sqrt(as.numeric(loan_test$statistic) /
                   sum(loan_table))
loan_phi

# Decision
loan_p <- loan_test$p.value
if(loan_p < 0.05) {
  print("Reject H0: Personal loan status is significantly associated with subscription.")
} else {
  print("Fail to reject H0: There is insufficient evidence of an association.")
}


# ============================================================
# 17. HYPOTHESIS TEST 3: PREVIOUS CAMPAIGN VS SUBSCRIPTION
# H0: Previous campaign outcome and subscription are independent.
# H1: Previous campaign outcome and subscription are associated.
# ============================================================

poutcome_table <- table(bank$poutcome, bank$y)
poutcome_table

poutcome_test <- chisq.test(poutcome_table)
poutcome_test

# Check expected frequencies
poutcome_test$expected
all(poutcome_test$expected >= 5)

# Examine residuals
poutcome_test$residuals

# Percentages
round(prop.table(poutcome_table, margin = 1) * 100, 2)

# Cramer's V because table is larger than 2x2
chi_squared <- as.numeric(poutcome_test$statistic)
n <- sum(poutcome_table)
r <- nrow(poutcome_table)
k <- ncol(poutcome_table)
cramers_v <- sqrt(chi_squared / (n * min(r - 1, k - 1)))
cramers_v

# Decision
poutcome_p <- poutcome_test$p.value
if(poutcome_p < 0.05) {
  print("Reject H0: Previous campaign outcome is significantly associated with subscription.")
} else {
  print("Fail to reject H0: There is insufficient evidence of an association.")
}




# ============================================================
# 18. HYPOTHESIS TEST 4: CALL DURATION VS SUBSCRIPTION
# H0: Distribution of call duration is the same between groups.
# H1: Distribution of call duration differs between groups.
# ============================================================

# Descriptive comparison
aggregate(duration ~ y, data = bank, FUN = mean)
aggregate(duration ~ y, data = bank, FUN = median)
aggregate(duration ~ y, data = bank, FUN = sd)

# Histograms by subscription group
par(mfrow = c(1, 2))

hist(bank$duration[bank$y == "no"],
     breaks = 30,
     main = "Duration: Non-Subscribers",
     xlab = "Duration")

hist(bank$duration[bank$y == "yes"],
     breaks = 30,
     main = "Duration: Subscribers",
     xlab = "Duration")

par(mfrow = c(1, 1))

# Q-Q plots to examine normality
par(mfrow = c(1, 2))

qqnorm(bank$duration[bank$y == "no"],
       main = "Q-Q Plot: Non-Subscribers")
qqline(bank$duration[bank$y == "no"])

qqnorm(bank$duration[bank$y == "yes"],
       main = "Q-Q Plot: Subscribers")
qqline(bank$duration[bank$y == "yes"])

par(mfrow = c(1, 1))

# Non-parametric Wilcoxon rank-sum test
duration_test <- wilcox.test(duration ~ y,
                             data = bank,
                             exact = FALSE,
                             conf.int = TRUE)
duration_test

# Decision
duration_p <- duration_test$p.value
if(duration_p < 0.05) {
  print("Reject H0: Call duration differs significantly between subscription groups.")
} else {
  print("Fail to reject H0: There is insufficient evidence of a difference.")
}




# ============================================================
# 19. CORRELATION ANALYSIS FOR NUMERICAL VARIABLES
# ============================================================

correlation_variables <- bank[, c("age", "balance", "duration",
                                  "campaign", "pdays", "previous")]

correlation_matrix <- cor(correlation_variables,
                          use = "complete.obs",
                          method = "spearman")
round(correlation_matrix, 3)

# ============================================================
# 20. BINARY LOGISTIC REGRESSION
# Outcome: Whether customer subscribed to term deposit
# ============================================================

# Convert target to binary
bank$subscribed <- ifelse(bank$y == "yes", 1, 0)

table(bank$subscribed)

# Fit logistic regression
# Duration is excluded here if the business objective is to identify
# customers before/during initial targeting because duration is not
# known before the marketing call has taken place.
model <- glm(subscribed ~ age + balance + housing + loan +
               campaign + previous + education + marital +
               poutcome,
             data = bank,
             family = binomial(link = "logit"))

summary(model)

# ============================================================
# 21. LOGISTIC REGRESSION ODDS RATIOS
# ============================================================

# Regression coefficients
coef(model)

# Odds ratios
odds_ratios <- exp(coef(model))
odds_ratios

# 95% confidence intervals for coefficients
confidence_intervals <- confint(model)
confidence_intervals

# 95% confidence intervals expressed as odds ratios
odds_ratio_ci <- exp(confidence_intervals)
odds_ratio_ci

# Combine results
model_results <- data.frame(
  Coefficient = coef(model),
  Odds_Ratio = exp(coef(model)),
  CI_Lower = exp(confint(model)[, 1]),
  CI_Upper = exp(confint(model)[, 2])
)

round(model_results, 3)

# ============================================================
# 22. MODEL PREDICTED PROBABILITIES
# ============================================================

bank$predicted_probability <- predict(model, type = "response")
summary(bank$predicted_probability)

hist(bank$predicted_probability,
     breaks = 30,
     main = "Predicted Subscription Probabilities",
     xlab = "Predicted Probability")

# ============================================================
# 23. MODEL CLASSIFICATION
# ============================================================

# Initial classification threshold of 0.50
bank$predicted_class <- ifelse(bank$predicted_probability >= 0.50,
                               1, 0)

# Confusion matrix
confusion_matrix <- table(
  Actual = bank$subscribed,
  Predicted = bank$predicted_class
)

confusion_matrix

# ============================================================
# 24. MODEL PERFORMANCE
# ============================================================

TN <- confusion_matrix["0", "0"]
FP <- confusion_matrix["0", "1"]
FN <- confusion_matrix["1", "0"]
TP <- confusion_matrix["1", "1"]

# Accuracy
accuracy <- (TP + TN) / sum(confusion_matrix)

# Sensitivity
sensitivity <- TP / (TP + FN)

# Specificity
specificity <- TN / (TN + FP)

# Precision
precision <- TP / (TP + FP)

accuracy
sensitivity
specificity
precision

performance <- data.frame(
  Measure = c("Accuracy", "Sensitivity",
              "Specificity", "Precision"),
  Value = c(accuracy, sensitivity,
            specificity, precision)
)

performance$Value <- round(performance$Value, 3)
performance

# ============================================================
# 25. CHECK OUTCOME IMBALANCE
# ============================================================

table(bank$y)
round(prop.table(table(bank$y)) * 100, 2)

# Compare model accuracy with majority-class baseline
majority_accuracy <- max(prop.table(table(bank$y)))
majority_accuracy

# ============================================================
# 26. FINAL STATISTICAL RESULTS SUMMARY
# ============================================================

cat("Housing Loan Chi-square p-value:",
    housing_test$p.value, "\n")

cat("Housing Loan Phi:",
    housing_phi, "\n")

cat("Personal Loan Chi-square p-value:",
    loan_test$p.value, "\n")

cat("Personal Loan Phi:",
    loan_phi, "\n")

cat("Previous Campaign Chi-square p-value:",
    poutcome_test$p.value, "\n")

cat("Previous Campaign Cramer's V:",
    cramers_v, "\n")

cat("Call Duration Wilcoxon p-value:",
    duration_test$p.value, "\n")

cat("Logistic Regression AIC:",
    AIC(model), "\n")

cat("Classification Accuracy:",
    accuracy, "\n")

cat("Sensitivity:",
    sensitivity, "\n")

cat("Specificity:",
    specificity, "\n")

cat("Precision:",
    precision, "\n")

# ============================================================
# 27. SAVE CLEANED DATA AND RESULTS
# ============================================================

write.csv(bank,
          "bank_analysis_data.csv",
          row.names = FALSE)

write.csv(model_results,
          "logistic_regression_results.csv",
          row.names = TRUE)

write.csv(performance,
          "model_performance.csv",
          row.names = FALSE)

# ============================================================
# 28. SESSION INFORMATION FOR REPRODUCIBILITY
# ============================================================

sessionInfo()

