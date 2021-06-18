#######################################################################
####################### analysis of hddm parameters ###################
#######################################################################

# packages
library(dplyr)
library(plyr)
library(car)
library(psych)
library(lme4)
library(tidyr)

### please note the multi-motive condition is here termed "emprecip" for convenience


# read in data exported from hddm
model <- read.csv("hddm_model_stats.csv", header = TRUE)

# get the relevant columns and lines and make convenient additional columns (check whether labelling was correct!)
model_subj           <- model[c(6:137, 143:274, 318:449),c(1:3)]
model_subj$condition <- rep(c("control", "empathy", "emprecip","reciprocity"), each = 33, times=3)
model_subj$param     <- rep(c("a", "v", "z"), each =132)
model_subj$ID        <- rep(c(1:33), times = 3)

####################################################################################
# Is there a difference between the multi-motive and the single motive conditions? #
####################################################################################

#######################
# for parameter v

# make dataframe only containing v-parameter
model_v <- model_subj[model_subj$param=="v",]

# compute relative differences between the multi.motive condition and each single motive condition
v_rec_index <- (model_v$mean[model_v$condition=="emprecip"]-model_v$mean[model_v$condition=="reciprocity"])/model_v$mean[model_v$condition=="reciprocity"]
v_emp_index <- (model_v$mean[model_v$condition=="emprecip"]-model_v$mean[model_v$condition=="empathy"])/model_v$mean[model_v$condition=="empathy"]
# compute the relative differences between empathy condition and reciprocity condition for testing of "empathy dominance hypothesis"
v_emp_dom_index <- (model_v$mean[model_v$condition=="empathy"]-model_v$mean[model_v$condition=="reciprocity"])/model_v$mean[model_v$condition=="reciprocity"]

# test whether indices are greater than zero
t.test(v_rec_index)
sd(v_rec_index)/sqrt(33)
t.test(v_emp_index)
sd(v_emp_index)/sqrt(33)

#####################
# for parameter z

# make dataframe only containing z-parameter
model_z <- model_subj[model_subj$param=="z",]

# compute relative differences between the multi.motive condition and each single motive condition
z_rec_index <- (model_z$mean[model_z$condition=="emprecip"]-model_z$mean[model_z$condition=="reciprocity"])/model_z$mean[model_z$condition=="reciprocity"]
z_emp_index <- (model_z$mean[model_z$condition=="emprecip"]-model_z$mean[model_z$condition=="empathy"])/model_z$mean[model_z$condition=="empathy"]
# compute the relative differences between empathy condition and reciprocity condition for testing of "empathy dominance hypothesis"
z_emp_dom_index <- (model_z$mean[model_z$condition=="empathy"]-model_z$mean[model_z$condition=="reciprocity"])/model_z$mean[model_z$condition=="reciprocity"]

# test whether indices are greater than zero
t.test(z_rec_index)
sd(z_rec_index)/sqrt(33)
t.test(z_emp_index)
sd(z_emp_index)/sqrt(33)

#####################
# for parameter a

# make dataframe only containing a-parameter
model_a <- model_subj[model_subj$param=="a",]

# compute relative differences between the multi.motive condition and each single motive condition
a_rec_index <- (model_a$mean[model_a$condition=="emprecip"]-model_a$mean[model_a$condition=="reciprocity"])/model_a$mean[model_a$condition=="reciprocity"]
a_emp_index <- (model_a$mean[model_a$condition=="emprecip"]-model_a$mean[model_a$condition=="empathy"])/model_a$mean[model_a$condition=="empathy"]
# compute the relative differences between empathy condition and reciprocity condition for testing of "empathy dominance hypothesis"
a_emp_dom_index <- (model_a$mean[model_a$condition=="empathy"]-model_a$mean[model_a$condition=="reciprocity"])/model_a$mean[model_a$condition=="reciprocity"]

# test whether indices are greater than zero
t.test(a_rec_index)
sd(a_rec_index)/sqrt(33)
t.test(a_emp_index)
sd(a_emp_index)/sqrt(33)

################################################################################################################
############### empathy dominance vs real interaction ##########################################################
################################################################################################################

# make a dataframe that includes the relative difference between the multi-motive and the reciprocity condition (rec_index) 
# and the relative difference between the empathy and the reciprocity condition (emp_dom_index)
# for the a-paramater as well as the z-parameter
hypotheses_df           <- as.data.frame(c(scale(a_rec_index), scale(z_rec_index)))
hypotheses_df$emp_dom   <- c(scale(a_emp_dom_index), scale(z_emp_dom_index))
hypotheses_df$param     <- rep(c("a", "z"), each = 33)
names(hypotheses_df)[1] <- "rec_index"

# test whether the relative differences between the empathy and the reciprocity condition can explain 
# the relative differences between the multi-motive and the reciprocity condition
lm_hypotheses <- lm(rec_index ~ emp_dom*param, hypotheses_df)
summary(lm_hypotheses)


# conduct the analogous analysis using the neural betas extracted from bilateral putamen (based on the AAL nomenclature)
# first read in betas
# multi-motive vs. reciprocity contrast
aal_putam_rec <- read.csv("mm_minus_rec_aal_biputam.csv", sep = ";", header = FALSE)
# empathy vs. reciprocity contrast
aal_putam_empMINUSrec <- read.csv("emp_minus_rec_aal_biputam.csv", sep = ";",header = FALSE)

# build the dataframe
empdom_neuro_df <- c(z_rec_index, z_empdom_index) %>%
  add_column(., putam_betas = c(aal_putam_rec$V1, aal_putam_rec$V1))%>%
  add_column(., index = rep(c("enhancement", "dominance"), each = 33))

# run model
lm_empdom_neuro_test <- lm(scale(rec_index) ~index*scale(putam_betas), empdom_neuro_df)
summary(lm_empdom_neuro_test)
