
#library(rstan)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
#library(brms)
#stopifnot(packageVersion("brms") >= '0.10.0')

### Load data
data <- readRDS("../workspace/data/responses_rcs.rds")

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

# data$indAmb  <- data$condition %>% dplyr::recode("none" = 0, "high" = 0, "low" = 0, "amb" = 1)
# data$indHigh <- data$condition %>% dplyr::recode("none" = 0, "high" = 1, "low" = 0, "amb" = 0)
# data$indLow  <- data$condition %>% dplyr::recode("none" = 0, "high" = 0, "low" = 1, "amb" = 0)

data$cLowVsAmb  <- data$condition %>% dplyr::recode("none" = 0, "high" = 0, "low" = 1, "amb" = 0)
data$cHighVsAmb <- data$condition %>% dplyr::recode("none" = 0, "high" = 1, "low" = 0, "amb" = 0)

data$cLowVsAvg  <- data$condition %>% dplyr::recode("none" = 0, "high" = 0, "low" = 1, "amb" = -1)
data$cHighVsAvg <- data$condition %>% dplyr::recode("none" = 0, "high" = 1, "low" = 0, "amb" = -1)

data$ind_amb  <- data$condition %>% dplyr::recode("none" = 0, "high" = 0, "low" = 0, "amb" = 1)
data$ind_high <- data$condition %>% dplyr::recode("none" = 0, "high" = 1, "low" = 0, "amb" = 0)
data$ind_low  <- data$condition %>% dplyr::recode("none" = 0, "high" = 0, "low" = 1, "amb" = 0)

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



# data %>% group_by(subject, condition, trial_id) %>%
#     dplyr::summarise(responseGrammatical = last(responseGrammatical)) %>%
#     dplyr::summarise(resp_yes = mean(responseGrammatical)) %>%
#     tidyr::pivot_wider(names_from = condition, values_from = resp_yes) %>%
#     arrange(low)

# NOTE:
# There are two participants who didn't rate a significant portion of the grammatical conditions acceptable
# (i.e., proporiton of 'yes' responses is close the one in the ungrammatical condition)
# Excluding them because especially dynamics estimates based on data this radically different may be quite off.
#
#    amb   high  low   none
# 6  0.98  0.11  0.97  0.09
# 7  1.00  0.09  0.98  0.02
#
excluded_participants_rc = c(6, 7)
data %<>% subset( !(subject %in% excluded_participants_rc) )

# exclude responses not belonging to a particular interval (usually, it's one of two responses falling into an interval)
data %<>% subset(maxlik_interval) %>% dplyr::select(-maxlik_interval)

# exclude interval 14, because: 
# (i) it is based on too few data points (~2K as opposed to ~5K in the previous intervals), and 
# (ii) it looks 'off' - it suggests lower d-prime at this last interval only, presumably, because incorrect responses are overrepresented, in this interval only
# to-do: consider removing these data points from the analysis too, so long as within-trial correlation isn't accounted for
data %<>% subset(interval != 14)


contr <- data %>% dplyr::select(condition, is_ungrammatical, cLowVsAvg, cHighVsAvg, cLowVsAmb, cHighVsAmb, ind_amb, ind_high, ind_low) %>% unique()
contr # full.condition, cGenderMasc, 

# compute percentage of yes responses per time window by participant
data_rc_bysubj_n_yes <- data %>% 
      group_by(subject, condition, interval, is_ungrammatical) %>% # gender, 
      summarize_n_responses()

# data_rc_bysubj_dprime <- data %>% 
#       plyr::ddply("gender", function(df) {
#           df %>%
#             group_by(subject, condition, interval) %>%
#             summarize_n_responses() %>%
#             summarize_dprime(data_nyes = ., by_subject = T)
#       })

data_rc_bysubj_n_yes %<>% dplyr::rename(time = avgtime) %>% dplyr::left_join(contr) %>% as.data.frame()
# data_rc_bysubj_dprime %<>% dplyr::rename(time = avgtime) %>% dplyr::left_join(contr) %>% as.data.frame()
