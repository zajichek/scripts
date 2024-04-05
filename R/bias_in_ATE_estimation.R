# Created: 2024-04-05
# Author: Alex Zajichek
# Description: Exploring potential induced bias in ATE estimation due to confounding and censoring
# Q: Does the ATE in the counterfactual distribution accurately reflect the HR in generating distribution?

# Load packages 
library(tidyverse)
library(survival)

# Set the seed
set.seed(91938)

# Set sample size 
n <- 100000

# Define true effect
lambda <- -log(0.56) / 10 # S(10) = .56
log_lambda_control <- log(lambda) 
log_lambda_treatment <- log_lambda_control + log(0.70) # HR = 0.70

# Generate the reponse under each treatment
time_treat <- rexp(n, rate = exp(log_lambda_treatment))
time_control <- rexp(n, rate = exp(log_lambda_control))

### 1. No censoring; fully-observed counterfactual distributions

# Put into a data frame
dat1 <-
    tibble(
        Event = 1,
        Treatment = time_treat,
        Control = time_control
    ) |>
    pivot_longer(
        cols = -Event,
        names_to = "Group",
        values_to = "Time"
    ) 

# a. Do we recover the HR when fitting an exp model?
mod1a <- survreg(Surv(Time) ~ Group, data = dat1, dist = "exponential")
1 / exp(mod1a$coefficients[[2]]) # 0.6965777 BASICALLY

# b. Do we recover the HR fitting a coxph model?
mod1b <- coxph(Surv(Time) ~ Group, data = dat1)
exp(mod1b$coefficients) # 0.6961598 BASICALLY

### 2. Now, introduce censoring to the process

# Random censor time
time_cens <- runif(n, min = 0, max = 10)

# Put into a newdata frame
dat2 <-
    tibble(
        Treatment = time_treat,
        Control = time_control,
        Censor = time_cens
    ) |>
    pivot_longer(
        cols = c(Treatment, Control),
        names_to = "Group",
        values_to = "Time"
    ) |>
    
    # Define event
    mutate(
        Event = Time <= Censor, # 20.25%
        Time = pmin(Time, Censor)
    )

# a. Do we recover the HR when fitting an exp model?
mod2a <- survreg(Surv(Time, Event) ~ Group, data = dat2, dist = "exponential")
1 / exp(mod2a$coefficients[[2]]) # 0.6992379 Basically?

# b. Do we recover the HR fitting a coxph model?
mod2b <- coxph(Surv(Time, Event) ~ Group, data = dat2)
exp(mod2b$coefficients) # 0.6994092 MAYBE?

# Seems to introduce bias

### 3. Introduce earlier censoring to the process

# Random censor time
time_cens <- runif(n, min = 0, max = 5)

# Put into a newdata frame
dat3 <-
    tibble(
        Treatment = time_treat,
        Control = time_control,
        Censor = time_cens
    ) |>
    pivot_longer(
        cols = c(Treatment, Control),
        names_to = "Group",
        values_to = "Time"
    ) |>
    
    # Define event
    mutate(
        Event = Time <= Censor, # 20.25%
        Time = pmin(Time, Censor)
    )

# a. Do we recover the HR when fitting an exp model?
mod3a <- survreg(Surv(Time, Event) ~ Group, data = dat3, dist = "exponential")
1 / exp(mod3a$coefficients[[2]]) # 0.698088 MAYBE?

# b. Do we recover the HR fitting a coxph model?
mod3b <- coxph(Surv(Time, Event) ~ Group, data = dat3)
exp(mod3b$coefficients) # 0.6981966 MAYBE?

### 4. Introduce later censoring to the process

# Random censor time
time_cens <- runif(n, min = 0, max = 20)

# Put into a newdata frame
dat4 <-
    tibble(
        Treatment = time_treat,
        Control = time_control,
        Censor = time_cens
    ) |>
    pivot_longer(
        cols = c(Treatment, Control),
        names_to = "Group",
        values_to = "Time"
    ) |>
    
    # Define event
    mutate(
        Event = Time <= Censor, # 20.25%
        Time = pmin(Time, Censor)
    )

# a. Do we recover the HR when fitting an exp model?
mod4a <- survreg(Surv(Time, Event) ~ Group, data = dat4, dist = "exponential")
1 / exp(mod4a$coefficients[[2]]) #  0.6943199 MAYBE?

# b. Do we recover the HR fitting a coxph model?
mod4b <- coxph(Surv(Time, Event) ~ Group, data = dat4)
exp(mod4b$coefficients) # 0.6943204 MAYBE?

# How does this change with?
# -> Sample size (This seemed to affect it; but the Exp/Cox are so close, that it seems like the amount of bias depends on sample size?)
# -> True hazard ratio
# -> Censoring mechanism
# -> Realized samples
# -> Confounding
