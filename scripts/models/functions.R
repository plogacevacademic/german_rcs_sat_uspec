library(plyr)
library(magrittr)

read_file <- function(fname) {
  readChar(fname, nchars = file.size(fname))
}

nlf <- function(formula) formula %T>% {attr(., "nl") <- T; attr(., "loop") <- T}


subset_data_corr <- function(data, ...)
{
  cur_data <- list(data = subset(data, ...))
  
  cur_data$data %<>% ungroup() %>% 
    arrange(subject, experiment, item, condition, trial_id) %>%
    group_by(subject, experiment, item, condition, trial_id) %>% 
    mutate(last_time = lag(time, default = -10),
           delta_time = time - last_time,
           last_responseGrammatical = lag(responseGrammatical, default = -1),
           idx_response = 1:n()
          )
  
  cur_data$stan_last_resp <- cur_data$data$last_responseGrammatical %>% as.integer() %>% stanvar(., "last_responseGrammatical", block = "data")
  cur_data$stan_delta_last <- cur_data$data$delta_time %>% as.double() %>% stanvar(., "delta_time", block = "data")
  cur_data
}


subset_data_nocorr <- function(data, ...)
{
  cur_data <- list(data = subset(data, ...))
  cur_data$data$last_responseGrammatical <- -1
  cur_data$stan_last <- cur_data$data$last_responseGrammatical %>% as.integer() %>% stanvar(., "last_responseGrammatical", block = "data")
  cur_data
}

subset_data_binomial_nocorr <- function(data, ...)
{
  cur_data <- list(data = subset(data, ...))
  cur_data
}


stan_satf_functions <- read_file("./math/satf_math_cdf.stan") %>% 
                        paste(., read_file("./math/satf_math.stan")) %>% 
                        stanvar(block = "functions", scode = .)

