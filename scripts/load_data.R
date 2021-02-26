
#library(rstan)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
#library(brms)
#stopifnot(packageVersion("brms") >= '0.10.0')

### Load data
data <- readRDS("../workspace/responses_rcs.rds")

#################### #################### ####################
#################### ### Prepare data ### ####################
#################### #################### ####################

stopifnot(all(data$time == data$stim_time))
data$condition %<>% factor(levels = c("none", "low", "high", "amb") )
contrasts(data$condition)

data %<>% mutate( clTrialId = scale( log(trial_id) ),
                  cTrialId = scale( trial_id ),
                )

data %<>% mutate(
  is_ungrammatical = as.integer(condition == "none" ),
  cAttachmentAmb  = as.integer(condition == "amb" ),
  cAttachmentHigh = as.integer(condition == "high" ),
  cAttachmentLow  = as.integer(condition == "low" ),
  cAttachmentAcceptable = as.integer(condition %in% c("amb", "high", "low") )
)

data$indAmb  <- data$condition %>% dplyr::recode("none" = 0, "high" = 0, "low" = 0, "amb" = 1)
data$indHigh <- data$condition %>% dplyr::recode("none" = 0, "high" = 1, "low" = 0, "amb" = 0)
data$indLow  <- data$condition %>% dplyr::recode("none" = 0, "high" = 0, "low" = 1, "amb" = 0)


# data$cLowVsAmb  <- data$condition %>% dplyr::recode("none" = 0, "high" = 0, "low" = 1, "amb" = 0)
# data$cHighVsAmb <- data$condition %>% dplyr::recode("none" = 0, "high" = 1, "low" = 0, "amb" = 0)

data$cLowVsAvg  <- data$condition %>% dplyr::recode("none" = 0, "high" = 0, "low" = 1, "amb" = -1)
data$cHighVsAvg <- data$condition %>% dplyr::recode("none" = 0, "high" = 1, "low" = 0, "amb" = -1)
data$cGenderMasc <- data$gender %>% dplyr::recode("fem" = -0.5, "masc" = .5)


# # Make sure to exclude data points too close to each other - otherwise a high correlation coefficient will mess up the likelihood,
# # since the approximation doesn't deal well with it.
# data %<>% group_by(subject, trial_id) %>% 
#   mutate(last_time = lag(time, default = NA),
#          delta_time = time - last_time
#   )
# 
# # drop all responses with delta_time < 0.2
# data %<>% subset(delta_time > 0.2)
# # drop all responses with delta_time > 2
# data %<>% subset(delta_time < 2)


