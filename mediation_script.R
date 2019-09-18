### Script for computing the moderation effects in Saulin et al. ##
###################################################################

# load lavaan package
library(lavaan)
# read in data
moderation_data <- read.csv("moderation_data.csv", header = TRUE, sep = ";")

# set up the model
moderation_model <- ' 
decis_rel_diff  ~ c*betas_bil_putam 
decis_rel_diff  ~ b*a_rel_diff       + c1*diff_multi_reciprocity_ratings + c2*empathy_scale
a_rel_diff      ~ a*betas_bil_putam  + a1*diff_multi_reciprocity_ratings + a2*empathy_scale

## indirect and total effects
indirect := a*b       #+ a*d
total    := a*b + c   #+ d
direct   := c
'

# fit the model
fit_model   <- sem(moderation_model, data = moderation_data, estimator = "MLR")
# get model statistics
summary(fit_model)
