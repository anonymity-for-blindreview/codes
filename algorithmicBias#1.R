set.seed(1234)

# 1. generating racial group G
df <- data.frame(race = c(rep(1, 8500), rep(0, 1500))) # 1 for privileged, 0 for underprivileged
n <- nrow(df) # total sample size


# 2. generating other variables based on Figure 1c
df$X <- rnorm(n, 0, 1) # all background factors, besides race

# set.seed(1234)
prob_Y <- plogis(-8*df$race + 3*df$X + rnorm(n, 0, .5))
df$Y <- rbinom(n, 1, prob = prob_Y) # actual college student success
# which is created to be better for underprivileged group

df$Y_h <- ifelse(-0.5*df$race + 3*df$X > .8, 1, 0) # algorithm's prediction for underprivileged group


# 3. confusion matrix
table <- table(df$Y, df$Y_h, df$race)
print(table)

# fairness metrics
tpr0 <- table[2,2,1] / sum(table[2,1,1], table[2,2,1]) #True positive rate (equal opportunity) for race=0
tpr1 <- table[2,2,2] / sum(table[2,1,2], table[2,2,2]) #True positive rate (equal opportunity) for race=1
print(tpr1*100 - tpr0*100) # the difference between true positive rates in percentage

fpr0 <- table[1,2,1] / sum(table[1,1,1], table[1,2,1]) #False positive rate (predictive equality) for race=0
fpr1 <- table[1,2,2] / sum(table[1,1,2], table[1,2,2]) #False positive rate (predictive equality) for race=1
print(fpr1*100 - fpr0*100) # the difference between false positive rates in percentage


# 4. logistic regression
summary(glm(Y_h ~ race, family = binomial(link = 'logit'), data = df))     # Unadjusted for Y
summary(glm(Y_h ~ race + Y, family = binomial(link = 'logit'), data = df)) # Adjusted for Y



