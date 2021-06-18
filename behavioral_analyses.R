## script for the behavioral analyses as reported in
# "The neural computation of goal-directed behavior in complex motivational states"
# Saulin, Horn, Lotze, Kaiser, & Hein

### load packages
library(dplyr)
library(car)
library(lme4)
library(tidyr)
library(quickpsy)


######################################################################################################
#################### read in data

induction                     <- read.csv("induction.csv", header = TRUE, sep = ";")
induction_prosocial_decisions <- read.csv("induction_prosocial_decisions.csv")

traits_prosocial_decisions    <- read.csv("prosocial_decisions_sepcific_traits.csv", header = TRUE)

prosocial_decisions           <- read.csv("prosocial_decisions.csv")
prosocial_decisions_absolute  <- read.csv("prosocial_decisions_absolute.csv")
prosocial_rts_per_condition   <- read.csv("prosocial_rts_per_condition.csv")

data_all_trials               <- read.csv("data_all_trials.csv",header = TRUE, sep = ";")

######################################################################################################
################ motive ratings ######################################################################

# ratings in the baseline vs.motive conditions
lmer_induction <- lmer(rating ~ cond_type + (1 | subjectID), induction)
Anova(lmer_induction)

# difference in ratings between motive conditions?
lmer_rating_condition        <- lmer(rating ~ condition + (1|subjectID),induction_prosocial_decisions )
Anova(lmer_rating_condition)

# effect of rating on prosocial decisions in motive conditions?
lmer_prosoc_rating_condition      <- lmer(prosoc_decis ~ rating*condition + (1|subjectID),induction_prosocial_decisions )
Anova(lmer_prosoc_rating_condition)

# focus on single motive conditions
# difference in rating values?
ind_pros_single         <- induction_prosocial_decisions[induction_prosocial_decisions$condition=="empathy" |induction_prosocial_decisions$condition=="reciprocity",]
lmer_rating_single      <- lmer(rating ~ condition + (1|subjectID),ind_pros_single)
Anova(lmer_rating_single)

# effect of rating on prosocial decisions in the single motive conditions?
lmer_rating_prosoc_single      <- lmer(prosoc_decis ~ rating*condition + (1|subjectID),ind_pros_single)
Anova(lmer_rating_prosoc_single)

# specificity of induction for the motive conditions regarding empathy and reciprocity?
lmer_trait_motive_conds <- lmer(scale(prosocial_decisions) ~ trait_measure_value*trait_measure_type*condition + (1|ID) , reb_big_reg_4conds[reb_big_reg_4conds$condition!="baseline",])
Anova(lmer_trait_motive_conds)

######################################################################################################
################ frequency of prosocial decisions ####################################################

# difference in the frequency of prosocial decisions between baseline and motive conditions?
lmer_prosoc_decis_baselinevsmotive <- lmer(decis ~ cond_type + (1|subjectID), prosocial_decisions)
Anova(lmer_prosoc_decis_baselinevsmotive)

# focus on comparison between motive conditions

# reciprocity vs empathy
lmer_recipVSemp <- lmer(decis ~ condition + (1|subjectID), prosocial_decisions[prosocial_decisions$condition!="control" & prosocial_decisions$condition!="multi-motive",])
Anova(lmer_recipVSemp)

# multi-motive vs empathy
lmer_multiVSemp <- lmer(decis ~ condition + (1|subjectID), prosocial_decisions[prosocial_decisions$condition!="control" & prosocial_decisions$condition!="reciprocity",])
Anova(lmer_multiVSemp)

# multi-motive vs reciprocity
lmer_multiVSrecip <- lmer(decis ~ condition + (1|subjectID), prosocial_decisions[prosocial_decisions$condition!="control" & prosocial_decisions$condition!="empathy",])
Anova(lmer_multiVSrecip)


# compute the percent change in prosocial prosocial choices in the multi-motive condition relative to each single motive condition
prosoc_multi <- prosocial_decisions$decis[prosocial_decisions$condition=="multi-motive"]
prosoc_recip <- prosocial_decisions$decis[prosocial_decisions$condition=="reciprocity"]
prosoc_emp   <- prosocial_decisions$decis[prosocial_decisions$condition=="empathy"]

reciprocity_index <- (prosoc_multi-prosoc_recip)/prosoc_recip*100
t.test(reciprocity_index)

empathy_index <- (prosoc_multi-prosoc_emp)/prosoc_emp*100
t.test(empathy_index)

######################################################################################################
################ reaction times ######################################################################

# difference in reaction times in the baseline condition vs the motive conditions?
lmer_RT_cond_type <- lmer(rt ~ cond_type + (1|ID), prosocial_rts_per_condition)
Anova(lmer_RT_cond_type)

# difference between motive conditions?
lmer_RT_conditions_only_motives <- lmer(rt ~ condition + (1|ID), prosocial_rts_per_condition[prosocial_rts_per_condition$condition!="control",])
Anova(lmer_RT_conditions_only_motives)

######################################################################################################
################ distribution of prosocial decisions #################################################

# compare distributions of the absolute number of prosocial decisions in the different conditions

ks.test(prosocial_decisions_absolute$control, prosocial_decisions_absolute$empathy)
ks.test(prosocial_decisions_absolute$reciprocity, prosocial_decisions_absolute$empathy)
ks.test(prosocial_decisions_absolute$reciprocity, prosocial_decisions_absolute$emprecip)
ks.test(prosocial_decisions_absolute$empathy, prosocial_decisions_absolute$emprecip)
ks.test(prosocial_decisions_absolute$reciprocity, prosocial_decisions_absolute$control)
ks.test(prosocial_decisions_absolute$control, prosocial_decisions_absolute$emprecip)


####################################################################################################
################ distribution of rts ###############################################################

rt_per_condition_and_id <- aggregate(rt ~ ID + condition, data_all_trials, mean)

# compare distributions of the reaction times in the different conditions
ks.test(rt_per_condition_and_id$rt[rt_per_condition_and_id$condition=="control"], rt_per_condition_and_id$rt[rt_per_condition_and_id$condition=="empathy"])
ks.test(rt_per_condition_and_id$rt[rt_per_condition_and_id$condition=="reciprocity"], rt_per_condition_and_id$rt[rt_per_condition_and_id$condition=="empathy"])
ks.test(rt_per_condition_and_id$rt[rt_per_condition_and_id$condition=="reciprocity"], rt_per_condition_and_id$rt[rt_per_condition_and_id$condition=="multi-motive"])
ks.test(rt_per_condition_and_id$rt[rt_per_condition_and_id$condition=="empathy"], rt_per_condition_and_id$rt[rt_per_condition_and_id$condition=="multi-motive"])
ks.test(rt_per_condition_and_id$rt[rt_per_condition_and_id$condition=="reciprocity"], rt_per_condition_and_id$rt[rt_per_condition_and_id$condition=="control"])
ks.test(rt_per_condition_and_id$rt[rt_per_condition_and_id$condition=="multi-motive"], rt_per_condition_and_id$rt[rt_per_condition_and_id$condition=="control"])

####################################################################################################
################ point information and prosocial behavior ##########################################

### do the possible loss for the self or the possible gain for the partner differentially influence 
### the frequency of prosocial decisions in the different conditions?
glmer_self_loss_other_gain <- glmer(resp_code ~ self_poss_loss*condition + other_poss_gain*condition + (1|ID), family = "binomial", data_all_trials)
Anova(glmer_self_loss_other_gain)

### does the difference in point equality differentially influence the frequency of prosocial decisions in the differen conditions?
glmer_diff_equality <- glmer(resp_code ~ diff_diff*condition + (1|ID), family = "binomial", data_all_trials)
Anova(glmer_diff_equality)

####################################################################################################
############# psychometric functions ###############################################################

# psychometric functions for the other's possible gain

# compute one function per condition
fit_other_gain      <- quickpsy(data_all_trials, other_poss_gain, resp_code, grouping = .(condition))

# compute one function per subject and condition
fit_other_gain_subj <- quickpsy(data_all_trials, other_poss_gain, resp_code, grouping = .(condition, ID))

lmer_thre_other_gain <- lmer(thre ~ condition + (1|ID), fit_other_gain_subj$thresholds)
Anova(lmer_thre_other_gain)

