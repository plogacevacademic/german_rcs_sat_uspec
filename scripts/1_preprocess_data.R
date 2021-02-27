
library(plyr)
library(dplyr)
library(magrittr)

fname_interval_optim_results <- "../workspace/data/intervals_optim_results.rda"
fname_out_locality <- "../workspace/data/responses_locality.rds"
fname_out_rcs <- "../workspace/data/responses_rcs.rds"
fname_out_rcs_complete <- "../workspace/data/responses_rcs_complete.rds"

# Notes:
# - Until participant 7: There were typos, and in some items conditions were mislabeled [-> That's why participant numbers start 1008.]
# - Starting at participant 16: Added 5 target structures to the practice sentences (3 high-attachment sentences, 2 low-attachment sentences)

prep_sat_data <- function(d)
{
  # other pre-processing
  d %<>% subset( !experiment %in% c("practice", "PracticeArrows") )
  d %<>% mutate( time = ifelse(time == -1, NA, time),
                 RT = ifelse(RT == -1, NA, RT) )
  
  d %<>% dplyr::select(-phraseNr, -phrase, -button, -answerLeft, -answerRight, -answerLeft, -answerCorrect)
  
  # label trials consecutively under the assumption that sentences may be repeated
  d %<>% mutate( trial_id = cumsum(event == "ITISleepStart") ) %>% 
    group_by(trial_id) %>% 
    dplyr::mutate(trial_index = min(time, na.rm = T)) %>%
    ungroup() %>%
    dplyr::mutate(trial_index = trial_index - min(time, na.rm = T),
                  trial_index = ordered(trial_index, levels = sort(unique(trial_index))),
                  trial_id = as.integer(trial_index) ) %>%
    dplyr::select(-trial_index)
  
  # drop the ITI interval
  d %<>% group_by(subject, trial_id) %>% 
    dplyr::mutate(is_iti = (time >= time[event == "ITISleepStart"]) &
                    (time <= time[event == "ITISleepEnd"])
    ) %>%
    subset(!is_iti) %>%
    dplyr::select(-is_iti)
  
  
  # determine timing relative to the first signal tone
  d <- ddply(d, .(subject, trial_id), function(d) 
  {
    min_time <- min(d$time, na.rm=T)
    d$trial_time <- d$time - min_time
    stopifnot(!is.na(min_time))
    
    signal_start_evt <- subset(d, event=='signalStart')
    min_signal_start_evt_time <- min(signal_start_evt$time)
    d$signal_time <- d$time - min_signal_start_evt_time
    stopifnot(!is.na(min_signal_start_evt_time))
    
    presentation_stop_evt <- subset(d, event=='displayUpdateStart')
    #save(signal_start_evt, presentation_stop_evt, file = "~/x.rda")
    
    max_presentation_stop_evt_time <- max(presentation_stop_evt$time, na.rm=T)
    d$stim_time <- d$time - max_presentation_stop_evt_time
    stopifnot(!is.na(max_presentation_stop_evt_time))
    
    d
  })
  
  d
}


read_participant <- function(part.num, order)
{
  path <- function(fname) paste0(data.dir, part.num, fname)
  if(order=="forward") {
    fnames <- sapply(c('.all_final1.dat','.all_final2.dat','.all_final3.dat'), path)
  } else {
    fnames <- sapply(c('.all_final1.dat','.all_final3_rev.dat','.all_final2_rev.dat'), path)
  }
  df <- fnames %>% ldply(function(fname) { read.table(fname, header=T, as.is=T) }, .id = NULL)
  prep_sat_data(df)
}



data.dir <- "../data/0_data_raw/"

d.forward <- NULL
for(part.num in c(1008, 1009, 1010, 1021, 1022, 1023, 1024, 1025, 1026, 1027)) {
  d.cur <- read_participant(part.num, order="forward")
  d.forward <- rbind(d.forward, d.cur)
}


d.backward <- read.table(paste(data.dir, "1011.all_final132_rev.dat", sep=""), header=T, as.is=T) %>% prep_sat_data() # participant 1011's data is all in one file.
for(part.num in c(1012, 1013, 1014, 1015, 1016, 1017, 1018, 1019, 1020)) {
  d.cur <- read_participant(part.num, order="backward")
  d.backward <- rbind(d.backward, d.cur)
}

d.all <- rbind(d.forward, d.backward)
d.all$time <- d.all$time-min(d.all$time, na.rm=T)

#d.resp <- ddply(subset(d.all, event=="satResponse" & signal_time > 0),
#                .(subject, trial.id), function(d) {
#  selection <- !is.na(d$responseButton) & !is.na(d$interval)
#  d$valid.presses <- length(d$responseButton[])
#})

##### 
# Length of practice and of the experimental session.

###############################################################

data_resp <- subset(d.all, event=="satResponse" & !is.na(signal_time) & signal_time > 0)
data_resp %<>% dplyr::select(-event)
data_resp %<>% dplyr::select(-RT)
data_resp$time <- data_resp$stim_time
data_resp$full.condition <- data_resp$condition


### The code below is an attempt to remap the responses to response bins, which may make
### sense, because participants seem to sometimes get into a sort of rhythm, and don't exactly
### respond after a response cue tone. 
### Since most analyses are run on raw respones, this won't have much of an implication other than making the plots smoother.
### ---


# P(X_1=t_1, X_2=t_2, X_3=t_3, ..., X_n=t_n)
# = P(X_1=t_1) * P(X_2=t_2|X_1=t_1) * P(X_3|X_1=t_1, X_2=t_2) * ... * P(X_n=t_n|X_1=t_1, ..., X_(n-1)=t_(n-1) )
# approximating by = P(X1) * P(X2) * P(X3) * ... * P(Xn)
# ... where P(X_n=t_n) = sum_{k} [ P(I_k:X_n | X_n=t_n) * P(X_n=t_n | I_k:X_n) ]
# ... where P(I_k:X_n | X_n=t_n) = P(X_n=t_n | I_k:X_n) * P(I_k:X_n) / P(X_n=t_n)
#     ... and therefore P(I_k:X_n | X_n=t_n) ~ P(X_n=t_n | I_k:X_n), assuming a flat prior for simplicity
# 
# not quite sure about this
# P(joint response times) * P( number of responses per interval | joint response times)
#
# NOTE:  Need a way for an interval to "disappear" without much of a penalty
# NOTE2: Need a way to prevent too many responses in one bin.


interval_loglik <- function(p, df, n, perform_mapping = F)
{
  response_dist_midpoint <- sort(p[1:n])
  response_dist_sd <- p[(n+1:n)] %>% exp()
  missing_lp <- log(plogis(p[(2*n+1:n)]))
  nonmissing_lp <- log(plogis(-p[(2*n+1:n)]))
  
  response_dist_midpoint %>% diff() %>% round(2)
  exp(missing_lp)
  
  loglik_time_cond_on_interval <-
    sapply(1:length(response_dist_midpoint), function(i) {
      dnorm(df$time, mean = response_dist_midpoint[i], sd = response_dist_sd[i], log = T) 
    })
  
  loglik_interval_cond_on_time <- 
    apply(loglik_time_cond_on_interval, MARGIN = 1, FUN = function(x) {
      wx <- exp(x + nonmissing_lp)
      log( wx/sum(wx) ) 
    }) %>% t()
  
  if (perform_mapping) {
    interval <- apply(loglik_interval_cond_on_time, MARGIN = 1, FUN = function(ll) which.max(ll) )
    attr(interval, "lik") <- apply(loglik_interval_cond_on_time, MARGIN = 1, FUN = function(ll) max(ll) )
    return (interval)
  }
  
  loglik_time_byresp_byinterval <- loglik_interval_cond_on_time + loglik_time_cond_on_interval
  loglik_time <- exp(loglik_time_byresp_byinterval) %>% apply(MARGIN = 1, FUN = function(x) { sum(x) } ) %>% log()
  
  # probability that *at least* of the responses belongs to this interval
  df_loglik_interval_cond_on_time <- loglik_interval_cond_on_time %>% as.data.frame() %>% cbind(trial_id=df$trial_id)
  loglik_resp_present_bytrial_byinterval <-
    df_loglik_interval_cond_on_time %>%
    group_by(trial_id) %>%
    summarize_at(vars(starts_with("V")), function(x) log(1-exp(sum(log(1-exp(x))))) ) %>%
    # summarize_at(vars(starts_with("V")), function(x) {
    #   ps <- exp(x)
    #   sapply(1:length(ps), function(i) { ps[-i] <- (1-ps[-i]); sum(log(ps)) } )
    #   }) %>%
    .[,-1]
  
  x <- exp(loglik_resp_present_bytrial_byinterval)
  x1m <- 1 - x
  loglik_resp_presence_bytrial_byinterval <- t(t(x) * nonmissing_lp) + t(t(x1m) * missing_lp)
  #nonmissing_lp * exp(loglik_resp_present_bytrial_byinterval) + missing_lp * (1-exp(loglik_resp_present_bytrial_byinterval))
  #summary(loglik_bytrial_byinterval)
  
  # priors
  p_missing_lprior <- dbeta(exp(missing_lp), 1, 3, log = T)
  p_dist_sd_lprior <- dexp(response_dist_sd, rate=4.5, log = T)
  
  ret <- sum(loglik_time) + sum(loglik_resp_presence_bytrial_byinterval) + sum(p_missing_lprior) + sum(p_dist_sd_lprior)
  print( ret )
  if (is.na(ret)) {
    print(p)
    print(response_dist_midpoint)
    print(response_dist_sd)
    print(missing_lp)
    print(nonmissing_lp)
    
    stop("Stopping due to NA.")      
  }
  ret
}


if ( !file.exists(fname_interval_optim_results) )
{
  start <- c(seq(-0.4, 4.8, .4), rep(log(0.2), 14), rep(qlogis(.1), 14) )
  
  res_optim <- plyr::dlply(data_resp, "subject", function(df)
  {
    
    idx = 1:14
    res <- optimize(function(offset) { start[idx] = start[idx] + offset
    interval_loglik(start, df = df, n = 14)
    }, lower = -1, upper = 1, maximum = T)
    start[idx] %<>% add( res$maximum )
    
    idx = 14+1:14
    res <- optimize(function(cur_p) { start[idx] = cur_p
    interval_loglik(start, df = df, n = 14)
    }, lower = -5, upper = 0, maximum = T)
    start[idx] <- res$maximum
    
    for (i in 2*14+1:14) {
      res <- optimize(function(cur_p) { start[i] = cur_p
      interval_loglik(start, df = df, n = 14)
      }, lower = -5, upper = 0, maximum = T)
      start[i] <- res$maximum
    }
    
    optim(start, interval_loglik, df = df, n = 14, control = list(fnscale = -1, maxit = 10^4))
  })
  
  save(res_optim, file = fname_interval_optim_results)    
}

(load(file = fname_interval_optim_results))

data_resp_remapped <-
  plyr::ddply(data_resp, "subject", function(df)
  {
    subj <- df$subject[1]
    p <- res_optim[[as.character(subj)]]$par
    
    df$interval <- interval_loglik(p = p, df = df, n = 14, perform_mapping = T)
    df$interval_lik <- attr(df$interval, "lik")
    df %<>% group_by(subject, trial_id, interval) %>%
      mutate(maxlik_interval = (max(interval_lik) == interval_lik) ) %>%
      dplyr::select(-interval_lik)
    df
  })
data_resp <- data_resp_remapped

###

# Data starts at participant 8 [1008] (There were typos for participants 1-7, and in some items conditions were mislabeled.)
# Relabel participants to start from 1.
data_resp$subject <- data_resp$subject - 1007

data_rc <- subset(data_resp, experiment=="RACE.RC")
data_loc <- subset(data_resp, experiment=="LOCALITY")

data_rc <- within(data_rc, {
  gender <- sapply(strsplit(full.condition, "-"), function(x) x[1])
  condition <- sapply(strsplit(full.condition, "-"), function(x) x[2])
  responseCorrect %<>% dplyr::recode("True" = 1, "False" = 0) %>% as.integer
  responseGrammatical  %<>% dplyr::recode("Y" = 1, "N" = 0) %>% as.integer 
  stimulusGrammatical <- condition %>% dplyr::recode("none"=0, "amb"=1, "high"=1,"low"=1) %>% as.integer
})
data_loc <- within(data_loc, {
  cond.num.np1 <- sapply(strsplit(full.condition, "-"), function(x) x[1])
  cond.num.np2 <- sapply(strsplit(full.condition, "-"), function(x) x[2])
  condition <- sapply(strsplit(full.condition, "-"), function(x) x[3])
  responseCorrect %<>% dplyr::recode("True"=1, "False"=0) %>% as.integer
  responseGrammatical %<>% dplyr::recode("Y"=1, "N"=0) %>% as.integer
})

saveRDS(data_rc, file = fname_out_rcs)
saveRDS(data_loc, file = fname_out_locality)
