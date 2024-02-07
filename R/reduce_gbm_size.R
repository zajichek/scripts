# Created: 2024-02-07
# Author: Alex Zajichek
# Description: Example of reducing size of {gbm} object for more efficient usage

# Get a data set
dat <- cheese::heart_disease

# Make an unecessarily-large sample
set.seed(112233)
dat <- dat[sample(1:nrow(dat), 1000000, replace = TRUE), ]

# Run a model (without retaining data)
mod <-
    gbm::gbm(
        formula = as.numeric(HeartDisease == "Yes") ~ . - BloodSugar,
        data = dat,
        distribution = "bernoulli",
        keep.data = FALSE
    )

# Remove unnecessary component, and restore object class
reduced_mod <- mod[setdiff(names(mod), "fit")]
class(reduced_mod) <- "gbm"

# What are the object sizes?
object.size(mod) # 8.1 MB
object.size(reduced_mod) # 0.10 MB

# Save to disk for later use
save(mod, file = "mod.RData") # 1.8 MB
save(reduced_mod, file = "reduced_mod.RData") # 9 KB

## Do they produce the same predictions?

# Load the models from disk
load(file = "mod.RData")
load(file = "reduced_mod.RData")

# Get predictions
n.trees <- mod$n.trees
pred_mod <- predict(mod, newdata = dat, n.trees = n.trees, type = "response")
pred_reduced_mod <- predict(reduced_mod, newdata = dat, n.trees = n.trees, type = "response")
all.equal(pred_mod, pred_reduced_mod) # TRUE

# Caveat: Trying to do this with multiple models in lists, etc. creates environment overhead when trying to save to an .RData file, so you lose the size benefit
# Fix: Define models on their own