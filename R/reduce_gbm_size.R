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
object.size(mod) # 8101360 bytes
object.size(reduced_mod) # 101240 bytes

# Do they produce the same predictions?
n.trees <- mod$n.trees
pred_mod <- predict(mod, newdata = dat, n.trees = n.trees, type = "response")
pred_reduced_mod <- predict(reduced_mod, newdata = dat, n.trees = n.trees, type = "response")
all.equal(pred_mod, pred_reduced_mod) # TRUE