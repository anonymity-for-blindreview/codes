###################################################################
#### Case 1. Collider bias leading to algorithmic bias
###################################################################

##################### Define Functions
## 1. DGP
sim1 <- function(a = a, b = a, n = 10000){
  # race
  race <- rbinom(n, 1, prob = .85)
  
  # Other features
  X <- rnorm(n, 0, 1)
  
  # Y
  prob.Y <- plogis(a*race + b*X + rnorm(n, 0, 1))
  Y <- rbinom(n, 1, prob = prob.Y)
  
  # Y-hat
  # Initialize base model
  tr <- glm(Y ~ X, family = binomial(link="logit"))
  # Compute predicted probabilities (like model.predict_proba in Python)
  prob.Y.h <- predict(tr, type = "response")
  # Compute predicted class labels (like model.predict in Python)
  Y.h <- as.numeric(prob.Y.h >= 0.5)
  
  m1 <- lm(scale(Y.h) ~ scale(race))
  m2 <- lm(scale(Y.h) ~ scale(race) + scale(Y))
  
  rslt <- list(
    race = race,
    Y = Y,
    Y.h = Y.h,
    r.unadj = summary(m1)$coef["scale(race)","Estimate"],
    p.unadj = summary(m1)$coef["scale(race)", "Pr(>|t|)"],
    r.adj = summary(m2)$coef["scale(race)","Estimate"],
    p.adj = summary(m2)$coef["scale(race)", "Pr(>|t|)"]
  )
  
  return(rslt)
}

## 2. Fairness Metrics
fairness <- function(Y=Y, Y_h=Y.h, race=race){
  table <- table(Y, Y_h, race)
  tpr0 <- table[2,2,1] / sum(table[2,1,1], table[2,2,1]) #True positive rate (equal opportunity) for Underprivileged racial group
  tpr1 <- table[2,2,2] / sum(table[2,1,2], table[2,2,2]) #True positive rate (equal opportunity) for Privileged racial group
  eo <- (tpr1 - tpr0)
  
  fpr0 <- table[1,2,1] / sum(table[1,1,1], table[1,2,1]) #False positive rate (predictive equality) for Underprivileged racial group
  fpr1 <- table[1,2,2] / sum(table[1,1,2], table[1,2,2]) #False positive rate (predictive equality) for Privileged racial group
  pe <- (fpr1 - fpr0)
  
  fm <- list(
    tpr0 = tpr0,
    tpr1 = tpr1,
    fpr0 = fpr0,
    fpr1 = fpr1,
    eo = eo,
    pe = pe
  )
  
  return(fm)
}

## 3. Whole
whole.f <- function(a = a, b = b){
  s <- sim1(a = a, b = b)
  fair <- fairness(s$Y, s$Y.h, s$race)
  r.unadj <- s$r.unadj
  p.unadj <- s$p.unadj
  r.adj <- s$r.adj
  p.adj <- s$p.adj
  
  return(list(
    tpr0 = fair$tpr0,
    tpr1 = fair$tpr1,
    fpr0 = fair$fpr0,
    fpr1 = fair$fpr1,
    eos = fair$eo,
    pes = fair$pe,
    r.unadj = r.unadj,
    r.adj = r.adj,
    p.unadj = p.unadj,
    p.adj = p.adj
  ))
}

##################### Results
R = 10000
out <- replicate(R, whole.f(a = 4, b = 4), simplify = TRUE)

## values unadjusting for Y
r.unadj.mean <- mean(as.numeric(out["r.unadj", , drop=TRUE]))
MCSD.unadj <- sd(as.numeric(out["r.unadj", , drop=TRUE]))
MCSE.unadj <- MCSD.unadj / sqrt(R)
rej.rate.unadj <- mean(as.numeric(out["p.unadj",,drop=TRUE]) < 0.05)

## values adjusting for Y
r.adj.mean <- mean(as.numeric(out["r.adj", , drop=TRUE]))
MCSD.adj <- sd(as.numeric(out["r.adj", , drop=TRUE]))
MCSE.adj <- MCSD.adj / sqrt(R)
rej.rate.adj <- mean(as.numeric(out["p.adj",,drop=TRUE]) < 0.05)

## fairness metrics
tpr0.mean <- mean(as.numeric(out["tpr0", , drop=TRUE]))
tpr1.mean <- mean(as.numeric(out["tpr1", , drop=TRUE]))
fpr0.mean <- mean(as.numeric(out["fpr0", , drop=TRUE]))
fpr1.mean <- mean(as.numeric(out["fpr1", , drop=TRUE]))
eo.mean <- mean(as.numeric(out["eos", , drop=TRUE]))
pe.mean <- mean(as.numeric(out["pes", , drop=TRUE]))

lr_df <- data.frame(
  r.unadj.mean = r.unadj.mean,
  MCSE.unadj = MCSE.unadj,
  r.adj.mean = r.adj.mean,
  MCSE.adj = MCSE.adj,
  rej.rate.unadj = rej.rate.unadj,
  rej.rate.adj = rej.rate.adj
)

fm_df <- data.frame(
  tpr0.mean = tpr0.mean,
  tpr1.mean = tpr1.mean,
  fpr0.mean = fpr0.mean,
  fpr1.mean = fpr1.mean,
  eo.mean = eo.mean,
  pe.mean = pe.mean
)

print(lr_df)
print(fm_df)


###################################################################
#### Case 2. Collider bias leading to statistical insignificance
###################################################################

##################### Define Functions
## 1. DGP
sim2 <- function(a = a, b = b, n = 10000){
  # race
  race <- rbinom(n, 1, prob = .75)  # 1 for privileged, 0 for underprivileged
  
  # Other features or covariates
  X <- rnorm(n, 0, 1)
  
  # Y
  prob.Y <- plogis(a*race + b*X + rnorm(n, 0, 1))
  Y <- rbinom(n, 1, prob = prob.Y)
  
  # Y-hat
  # Initialize base model
  tr <- glm(Y ~ race + X, family = binomial(link="logit"))
  # Compute predicted probabilities (like model.predict_proba)
  prob.Y.h <- predict(tr, type = "response")
  # Compute predicted class labels (like model.predict)
  Y.h <- as.numeric(prob.Y.h >= 0.5)
  
  m1 <- lm(scale(Y.h) ~ scale(race))
  m2 <- lm(scale(Y.h) ~ scale(race) + scale(Y))
  
  rslt <- list(
    race = race,
    Y = Y,
    Y.h = Y.h,
    r.unadj = summary(m1)$coef["scale(race)","Estimate"],
    r.adj = summary(m2)$coef["scale(race)","Estimate"],
    p.unadj = summary(m1)$coef["scale(race)","Pr(>|t|)"],
    p.adj = summary(m2)$coef["scale(race)", "Pr(>|t|)"]
  )
  
  return(rslt)
}

## 2. Fairness Metrics
fairness <- function(Y=Y, Y_h=Y.h, race=race){
  table <- table(Y, Y_h, race)
  tpr0 <- table[2,2,1] / sum(table[2,1,1], table[2,2,1]) #True positive rate (equal opportunity) for Underprivileged racial group
  tpr1 <- table[2,2,2] / sum(table[2,1,2], table[2,2,2]) #True positive rate (equal opportunity) for Privileged racial group
  eo <- (tpr1 - tpr0)
  
  fpr0 <- table[1,2,1] / sum(table[1,1,1], table[1,2,1]) #False positive rate (predictive equality) for Underprivileged racial group
  fpr1 <- table[1,2,2] / sum(table[1,1,2], table[1,2,2]) #False positive rate (predictive equality) for Privileged racial group
  pe <- (fpr1 - fpr0)
  
  fm <- list(
    tpr0 = tpr0,
    tpr1 = tpr1,
    fpr0 = fpr0,
    fpr1 = fpr1,
    eo = eo,
    pe = pe
  )
  
  return(fm)
}

## 3. Whole
whole.f <- function(a = a, b = b){
  s <- sim2(a = a, b = b)
  fair <- fairness(s$Y, s$Y.h, s$race)
  r.unadj <- s$r.unadj
  r.adj <- s$r.adj
  p.unadj <- s$p.unadj
  p.adj <- s$p.adj
  
  return(list(
    tpr0 = fair$tpr0,
    tpr1 = fair$tpr1,
    fpr0 = fair$fpr0,
    fpr1 = fair$fpr1,
    eos = fair$eo,
    pes = fair$pe,
    r.unadj = r.unadj,
    r.adj = r.adj,
    p.unadj = p.unadj,
    p.adj = p.adj
  ))
}

##################### Results
R = 10000
out <- replicate(R, whole.f(a = 0.5, b = 5.5), simplify = TRUE)

## values unadjusting for Y
r.unadj.mean <- mean(as.numeric(out["r.unadj", , drop=TRUE]))
MCSD.unadj <- sd(as.numeric(out["r.unadj", , drop=TRUE]))
MCSE.unadj <- MCSD.unadj / sqrt(R)
rej.rate.unadj <- mean(as.numeric(out["p.unadj",,drop=TRUE]) < 0.05)

## values adjusting for Y
r.adj.mean <- mean(as.numeric(out["r.adj", , drop=TRUE]))
MCSD.adj <- sd(as.numeric(out["r.adj", , drop=TRUE]))
MCSE.adj <- MCSD.adj / sqrt(R)
rej.rate.adj <- mean(as.numeric(out["p.adj",,drop=TRUE]) < 0.05)

## fairness metrics
tpr0.mean <- mean(as.numeric(out["tpr0", , drop=TRUE]))
tpr1.mean <- mean(as.numeric(out["tpr1", , drop=TRUE]))
fpr0.mean <- mean(as.numeric(out["fpr0", , drop=TRUE]))
fpr1.mean <- mean(as.numeric(out["fpr1", , drop=TRUE]))
eo.mean <- mean(as.numeric(out["eos", , drop=TRUE]))
pe.mean <- mean(as.numeric(out["pes", , drop=TRUE]))

lr_df <- data.frame(
  r.unadj.mean = r.unadj.mean,
  MCSE.unadj = MCSE.unadj,
  r.adj.mean = r.adj.mean,
  MCSE.adj = MCSE.adj,
  rej.rate.unadj = rej.rate.unadj,
  rej.rate.adj = rej.rate.adj
)

fm_df <- data.frame(
  tpr0.mean = tpr0.mean,
  tpr1.mean = tpr1.mean,
  fpr0.mean = fpr0.mean,
  fpr1.mean = fpr1.mean,
  eo.mean = eo.mean,
  pe.mean = pe.mean
)

print(lr_df)
print(fm_df)

