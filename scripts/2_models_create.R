
library(rstan)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(brms)
stopifnot(packageVersion("brms") >= '0.10.0')

library(doMC)
doMC::registerDoMC(parallel::detectCores())

source("./models/functions.R")
source("./models/contrasts_priors.R")
source("./models_postprocessing.R")

source("./load_data.R")

subjects = sort(unique(data$subject))

# save.image(file = "../workspace/cloud_compute_upload.rda")
# 
# data$cNP1Masc <-
#    data$full.condition %>% dplyr::recode(
#       "masc-none"=-.5, "masc-amb"=.5,"fem-none"=.5, "fem-amb"=-.5,
#       "fem-high"=-.5, "fem-low"=.5, "masc-high"=.5, "masc-low"=-.5 
#    )
# 
# data$cNP2Masc <-
#    data$full.condition %>% dplyr::recode(
#       "masc-none"=-.5, "masc-amb"=.5,"fem-none"=.5, "fem-amb"=-.5,
#       "fem-high"=.5, "fem-low"=-.5, "masc-high"=-.5, "masc-low"=.5 
#    )



use_data_bernoulli = T

for (subj in subjects)
{
   print(subj)

   if (use_data_bernoulli) {
     cur_data_df <- data %>% subset_data_nocorr(subject == subj)
     sat_bf_cur <- sat_bf
   } else {
     cur_data_df <- data_rc_bysubj_n_yes %>% subset_data_binomial_nocorr(subject == subj)
     sat_bf_cur <- sat_bf_binomial
   }
   fname <- sprintf("subj_%d", subj)

   formula1_single_subj <- sat_bf_cur( dprime = ~ 1 + cGenderMasc * (cLowVsAvg + cHighVsAvg),
                                       crit = ~ 1  + cGenderMasc,
                                       crit_leftasym =~ 1
                                     )

   # formula2_single_subj <- sat_bf_cur( dprime = ~ 1 + cGenderMasc * (cLowVsAvg + cHighVsAvg),
   #                                     crit = ~ 1  + cGenderMasc + cNP1Masc + cNP2Masc,
   #                                     crit_leftasym =~ 1 + cNP1Masc + cNP2Masc
   # )
   
   fit <- brm(formula2_single_subj,
             prior = c(priors$dprime, priors$crit),
             stanvars = stan_satf_functions, data = cur_data_df$data,
             iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234, init_r = .1
             ,
             file = file.path("../workspace/model_fits/models_singlesubj_uncorr_fullbias", fname)
             )
}


if (use_data_bernoulli) {
  cur_data_df <- data %>% subset_data_nocorr(T)
  sat_bf_cur <- sat_bf
} else {
  cur_data_df <- data_rc_bysubj_n_yes %>% subset_data_binomial_nocorr(T)
  sat_bf_cur <- sat_bf_binomial
}

formula1 <-
   sat_bf_cur(
      dprime = ~ 1, crit = ~ 1,
      dprime_asymptote = ~ 1 + cLowVsAvg + cHighVsAvg + ( cLowVsAvg + cHighVsAvg + 1|d1|subject),
      dprime_invrate   = ~ 1 + cLowVsAvg + cHighVsAvg + ( cLowVsAvg + cHighVsAvg + 1|d2|subject),
      dprime_intercept = ~ 1 + cLowVsAvg + cHighVsAvg + ( cLowVsAvg + cHighVsAvg + 1|d3|subject),
      crit_leftasym   = ~ 1 + (1|c1|subject),
      crit_rightasym = ~ 1 +  (1|c2|subject),
      crit_invrate   = ~ 1 +  (1|c3|subject),
      crit_intercept = ~ 1 +  (1|c4|subject)
   )

prior1 <- c(priors$dprime, priors$crit,
           priors$dprime_randef_subj, priors$crit_randef_subj,
           set_prior("lkj(2)", class = "cor")
         )

fit1 <- brm(formula1, prior = prior1,
            stanvars = stan_satf_functions, data = cur_data_df$data, 
            iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 1234, init_r = .1,
            #control = list(metric = "dense_e", adapt_delta = .99),
            file = "../workspace/model_fits/fit_all_uncorr_bysubj_1_4000B",
            sample_file = "../workspace/model_fits/fit_all_uncorr_bysubj_1_4000B"
            )


formula2 <-
   sat_bf_cur(
      dprime = ~ 1, crit = ~ 1,
      dprime_asymptote = ~ 1 + cGenderMasc * (cLowVsAvg + cHighVsAvg) + ( cGenderMasc * (cLowVsAvg + cHighVsAvg) + 1|d1|subject),
      dprime_invrate   = ~ 1 + cGenderMasc * (cLowVsAvg + cHighVsAvg) + ( cGenderMasc * (cLowVsAvg + cHighVsAvg) + 1|d2|subject),
      dprime_intercept = ~ 1 + cGenderMasc * (cLowVsAvg + cHighVsAvg) + ( cGenderMasc * (cLowVsAvg + cHighVsAvg) + 1|d3|subject),
      crit_leftasym   = ~ 1 + (1|c1|subject),
      crit_rightasym = ~ 1 + cGenderMasc + (cGenderMasc + 1|c2|subject),
      crit_invrate   = ~ 1 + cGenderMasc + (cGenderMasc + 1|c3|subject),
      crit_intercept = ~ 1 + cGenderMasc + (cGenderMasc + 1|c4|subject)
   )


fit2 <- brm(formula2, prior = prior1,
                stanvars = stan_satf_functions, data = cur_data_df$data, 
                iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 1234, init_r = .1,
                sample_prior = "yes",
                #control = list(metric = "dense_e", adapt_delta = .99),
                file = "../workspace/model_fits/fit_all_uncorr_bysubj_2_4000-2",
                sample_file = "../workspace/model_fits/fit_all_uncorr_bysubj_2_4000-2"
                )

fit2_prior <- brm(formula2, prior = prior1,
            stanvars = stan_satf_functions, data = cur_data_df$data[1:2,], 
            sample_prior = "only",
            iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 1234, init_r = .1,
            #control = list(metric = "dense_e", adapt_delta = .99),
            file = "../workspace/model_fits/fit_all_uncorr_bysubj_2_prior"
            )
 

# ##############################################
# 
# data %<>% mutate(cMasc_LowVsAvg = ifelse(gender == "masc", cLowVsAvg, 0),
#                  cMasc_HighVsAvg = ifelse(gender == "masc", cHighVsAvg, 0),
#                  cFem_LowVsAvg = ifelse(gender == "fem", cLowVsAvg, 0),
#                  cFem_HighVsAvg = ifelse(gender == "fem", cHighVsAvg, 0),
#                )
# 
#                
# if (use_data_bernoulli) {
#    cur_data_df <- data %>% subset_data_nocorr(T)
#    sat_bf_cur <- sat_bf
# } else {
#    cur_data_df <- data_rc_bysubj_n_yes %>% subset_data_binomial_nocorr(T)
#    sat_bf_cur <- sat_bf_binomial
# }
# 
# 
# 
# formulaB1 <-
#    sat_bf_cur(
#       dprime = ~ 1 + (cGenderMasc + cNP1Masc + cNP2Masc) * (cLowVsAvg + cHighVsAvg), 
#       crit = ~ 1 + cGenderMasc + cNP1Masc + cNP2Masc,
#       # dprime_asymptote = ~ ,
#       # dprime_invrate   = ~ ,
#       # dprime_intercept = ~ ,
#       crit_leftasym   = ~ 1 + cNP1Masc + cNP2Masc
#       # crit_rightasym = ~ ,
#       # crit_invrate   = ~ ,
#       # crit_intercept = ~ 
#    )
# 
# priorB1 <- c(priors$dprime, priors$crit #,
#             #priors$dprime_randef_subj, priors$crit_randef_subj,
#             #set_prior("lkj(2)", class = "cor")
#             )
# 
# fitB1 <- brm(formulaB1, prior = priorB1,
#             stanvars = stan_satf_functions, data = cur_data_df$data, 
#             iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 1234, init_r = .1,
#             algorithm = "meanfield"
#             # ,
#             # #control = list(metric = "dense_e", adapt_delta = .99),
#             # file = "../workspace/model_fits/fit_all_uncorr_bysubj_1_4000B",
#             # sample_file = "../workspace/model_fits/fit_all_uncorr_bysubj_1_4000B"
#             )
# 
# fitB1

