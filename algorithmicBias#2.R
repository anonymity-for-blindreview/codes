
#### A. Generating variables for simulation

# 1. generate race
n <- 1000
df <- data.frame(race= c(rep(1, 750), rep(0, 250)))

# 2. generate X
set.seed(2554)
df$X <- rnorm(n, 0, 1)

# 3. generate Y and Y_h that will yield equal opportunity value > .22 & predictive equality value > .3
evaluate_fairness <- function(a, b, c, d){
  
  # 3. generate prob_Y and Y
  set.seed(1234)
  prob_Y <- plogis(-a * df$race + b * df$X + rnorm(n, 0, 0.5))
  df$Y <- rbinom(n, 1, prob = prob_Y) 
  
  # 4. generate Y_h
  set.seed(4228)
  df$Y_h <- ifelse(-c * df$race + d * df$X >= .8, 1, 0) 
  
  # 5. Confusion matrix
  table <- table(df$Y, df$Y_h, df$race)
  
  # 6. Compute true positive rates (TPRs) and false positive rates (FPRs)
  ## Compute TPR and FPR with safe checks for zero denominators for each racial group
  tpr0 <- if (sum(table[2, , 1]) > 0) table[2, 2, 1] / sum(table[2, 1, 1], table[2, 2, 1]) else NA # TPR for race = 0
  tpr1 <- if (sum(table[2, , 2]) > 0) table[2, 2, 2] / sum(table[2, 1, 2], table[2, 2, 2]) else NA # TPR for race = 1
  
  fpr0 <- if (sum(table[1, , 1]) > 0) table[1, 2, 1] / sum(table[1, 1, 1], table[1, 2, 1]) else NA # FPR for race = 0
  fpr1 <- if (sum(table[1, , 2]) > 0) table[1, 2, 2] / sum(table[1, 1, 2], table[1, 2, 2]) else NA # FPR for race = 1
  
  # 7 Compute differences
  tpr_diff <- if (!is.na(tpr1) && !is.na(tpr0)) tpr1 * 100 - tpr0 * 100 else NA
  fpr_diff <- if (!is.na(fpr1) && !is.na(fpr0)) fpr1 * 100 - fpr0 * 100 else NA
  
  return(c(tpr_diff, fpr_diff))
}

# 8. systematically test sets of values for a, b, c, and d
results <- list()
a_values <- seq(4, 30, 0.5)
b_values <- seq(2, 12, 0.5)
c_values <- seq(0.1, 3, 0.2)
d_values <- seq(1, 5, 0.2)

## this may take awhile to run
## only saving results that meet our conditions for equal opportunity and predictive equality
for (a in a_values) {
  for (b in b_values) {
    for (c in c_values) {
      for (d in d_values) {
        fairness <- evaluate_fairness(a, b, c, d)
        if (!is.na(fairness[1]) && !is.na(fairness[2]) && fairness[1] > 20 && fairness[2] > 20) {
          results <- append(results, list(list(a = a, b = b, c = c, d = d, 
                                               tpr_diff = fairness[1], 
                                               fpr_diff = fairness[2])))
        }
      }
    }
  }
}

# 9. display results
if (length(results) > 0) {
  print("Sets of parameters that satisfy the conditions:")
  combinations <- do.call(rbind, lapply(results, as.data.frame))
  print(combinations)
} else {
  print("No sets of parameters satisfy the conditions.")
}


#### B. Testing for collider bias

# 10. create an empty list 
coeffs <- list()

## this may take awhile to run
for(i in 1:nrow(combinations)){
  
  # 11. regenerate Y
  set.seed(1234)
  prob_Y <- plogis(-1 * combinations$a[i] * df$race + combinations$b[i] * df$X + rnorm(n, 0, 0.5))
  df$Y <- rbinom(n, 1, prob = prob_Y) # actual success
  
  # 12. regenerate Y_h
  set.seed(4228)
  df$Y_h <- ifelse(-1 * combinations$c[i] * df$race + combinations$d[i] * df$X >= .8, 1, 0) # prediction
  
  # 13. run logistic regressions
  glm_unadjusted <- summary(glm(Y_h ~ race, family = binomial(link = 'logit'), data = df)) # Unadjusted for Y
  glm_adjusted <- summary(glm(Y_h ~ race + Y, family = binomial(link = 'logit'), data = df)) # Adjusted for Y
  
  # 14. saving coefficients from each logistic regression
  coef_u <- as.data.frame(t(glm_unadjusted$coefficients['race', ]))
  coef_a <- as.data.frame(t(glm_adjusted$coefficients['race', ]))
  
  coeffs[[i]] <- list(
    coef_u = coef_u,
    coef_a = coef_a
  )
}

# 15. create a data frame of logistic regression results
coeffs_df <- do.call(rbind, lapply(coeffs, as.data.frame))
print(nrow(coeffs_df)) # a total of 27,730 cases

sub_df <- coeffs_df[coeffs_df["coef_u.Pr...z.."] < .05 & coeffs_df["coef_a.Pr...z.."] < .05, ]
nrow(sub_df) # there are a total of 13,486 cases in which the unadjusted and adjusted coefficients of race are statistically significant at 5% level of significance



### run the following code only if you want to export the results to excel
# library(writexl)
# write_xlsx(sub_df, 'output.xlsx')







