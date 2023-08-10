# Created: 2023-08-09
# Author: Alex Zajichek
# Description: Replicating simulation done in SAS for overlap weighting with survival model
# Source: http://www2.stat.duke.edu/~fl35/OW/OW_survival_Demo.sas

# Load packages
library(tidyverse)
library(survival)

# Set the sample size 
n <- 1000

# Set the seed
set.seed(123)

## 1. Make a dataset defining the population
population <-
    tibble(
        
        # Define covariates
        age = rnorm(n, mean = 50, sd = 10),
        zage = (age - 50) / 10,
        sex = rbinom(n, size = 1, prob = 0.6),
        race = rbinom(n, size = 1, prob = 0.3),
        income = rnorm(n, mean = 20, sd = 3),
        zincome = (income - 20) / 3,
        
        # Define the TRUE PS linear predictor
        linear_term = -0.4 + log(1.5)*zage*(age<75) + log(0.8)*sex + log(1.5)*race + log(.50)*zincome,
        
        # Define the TRUE PS score
        pA = exp(linear_term) / (1 + exp(linear_term)),
        
        # Generate the REALIZED treatment value
        A = rbinom(n, size = 1, prob = pA),
        
        # Generate the TRUE overlap weight (for the treatment the patient happened to receive)
        overlap = A * (1-pA) + (1-A)*pA,
        
        # Baseline hazard
        lambdat = 1*365/0.09,
        
        # Define TRUE Weibull linear predictor
        linear_term_t_0 = exp(-(log(3) * zage + log(0.8) * sex + log(1.5) * race + log(.70) * zincome)), # If NOT treated
        linear_term_t_1 = exp(-(log(3) * zage + log(0.8) * sex + log(1.5) * race + log(.70) * zincome + log(0.70))), # If treated
        
        # Generate REALIZED survival times under each treatment scenario (same parameterizations in SAS)
        t_0 = rweibull(n, shape = 1, scale = lambdat * linear_term_t_0), 
        t_1 = rweibull(n, shape = 1, scale = lambdat * linear_term_t_1),
        
        # The ACTUAL event time outcome depends on the treatment ACTUALLY observed for the patient
        `T` = t_0 * (1 - A) + t_1 * A,
        
        # Generate a REALIZED censoring time (completely random)
        C = 500 + 500 * runif(n),
        
        # Calculate the OBSERVED time in the data set
        time = pmin(`T`, C), # They either had the event, or were censored first
        
        # Calculate the event status (TRUE if had event, otherwise censored)
        Y = as.numeric(`T` < C),
        
        # Calculate the normalized overlap weight value
        zoverlap = overlap * A / sum(overlap * A) + overlap * (1 - A) / sum(overlap * (1 - A))
        
    )

# Summarize the data
population %>% map(summary)
population %>% ggplot() + geom_point(aes(x = zage, y = pA, color = factor(A)))

## 2. Build the propensity score model

# Run the model (not sure if the spline methods are EXACTLY the same)
mod <- 
    glm(
        formula = A ~ rms::rcs(zage, 4, norm  = 0) + rms::rcs(zincome, 4) + factor(sex) + factor(race),
        data = population,
        family = "binomial"
    )

# Add the ESTIMATED propensity scores and overlap weights
population <-
    population %>%
    
    # Add columns
    mutate(
        p1 = predict(mod, type = "response"), # P(A = 1|X) estimate
        ow_weight = A * (1 - p1) + (1 - A) * p1,
        zow_weight = ow_weight * A / sum(ow_weight * A) + ow_weight * (1 - A) / sum(ow_weight * (1 - A))
    ) 

# Summarise the observed propensity score distribution
with(population, tapply(ow_weight, A, summary)) # Raw
with(population, tapply(zow_weight, A, summary)) # Normalized

# Make a graph of the unweighted PS distribution
population %>%
    
    # Make a plot
    ggplot() +
    geom_density(
        aes(
            x = p1,
            color = factor(A)
        )
    )

# Make a graph of the weighted PS distribution
population %>%
    
    # Make a plot
    ggplot() +
    geom_density(
        aes(
            x = p1,
            color = factor(A),
            weight = ow_weight
        )
    )

### 3. Analysis of outcome

## First, obtain the TRUE causal effect (since we simulated)

# Make a dataset where we observed ALL potential outcomes (i.e., all treatment outcomes for ALL patients, and ALL control outcomes for all patients; so n = 2*n in original)
true_dat <-
    population %>%
    
    # Observed outcome when everyone was treated
    transmute(
        A_p = 1,
        `T` = t_1, # The REALIZED event time when they were treated
        time = pmin(`T`, C),
        Y_p = as.numeric(`T` < C),
        zoverlap
    ) %>%
    
    # Bind to get the observed outcome when everyone was NOT treated
    bind_rows(
        population %>%
            transmute(
                A_p = 0,
                `T` = t_0, # The REALIZED event time when they were NOT treated
                time = pmin(`T`, C),
                Y_p = as.numeric(`T` < C),
                zoverlap
            )
    )

# Run a Cox model to get the HR
true_mod <-
    coxph(
        formula = Surv(time, Y_p == 1) ~ factor(A_p), 
        data = true_dat, # Stacked dataset with ALL potential outcomes
        weights = zoverlap # True overlap weights
    ) 
true_mod$coefficients %>% exp # HR = 0.7303015

## Now, run the estimation on the OBSERVED data that we'd see in real life (where we only observe one potential outcome from each patient, for the treatment they actually received)
est_mod <-
    coxph(
        formula = Surv(time, Y == 1) ~ A, 
        data = population %>% mutate(A = factor(A)), # Just the one outcome from the one treatment we observed
        weights = zow_weight, # Estimated overlap weights
        robust = TRUE
    )
est_mod$coefficients %>% exp # 0.8473617 
est_mod %>% confint() %>% exp # 0.6406714 1.120733 <-- contains true value

# Get the survival probability at time 365 (cumulative hazard approach)
cum_haz <- 
    basehaz(est_mod, centered = FALSE) %>% # Same as TRUE here; evaluated at covariates = 0
    
    # Find the cumulative hazard at time 365
    filter(time <= 365) %>% tail(1)

# Compute the survival probability at that time point for each patient
surv_365 <- exp(-cum_haz$hazard * exp(est_mod$linear.predictors)) %>% unique()
1 - surv_365

# From a survfit call
surv_curves <-
    survfit(
        est_mod,
        newdata = tibble(A = factor(c(0, 1)))
    )

# Find the index closest to 1 year
ind_365 <- which.min(abs(surv_curves$time - 365))

# Extract those elements
1 - surv_curves$surv[ind_365,] # 0.1454575 0.1247064
1 - surv_curves$lower[ind_365,] # 0.5278987 0.4706504 
1 - surv_curves$upper[ind_365,] # 0 0
