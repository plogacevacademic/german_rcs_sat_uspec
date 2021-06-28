options(dplyr.summarise.inform = FALSE)

# computes d', smooths by adding 0.25 to each count (i.e., 0.5 to the total) in order to avoid extremes values,
# as recommended somewhere by Treisman
# NOTE: this smoothing is essentially a less elegant form of a prior
compute_dprime <- function(N_signal, N_yes_signal, N_noise, N_yes_noise) {
  dprime_smooth = 0.5
  criterion <- qnorm( (N_yes_noise+(dprime_smooth/2))/(N_noise+dprime_smooth) )
  qnorm( (N_yes_signal+(dprime_smooth/2))/(N_signal+dprime_smooth) ) - criterion
}

summarize_n_responses <- function(df) {
  df %>% dplyr::summarize(n = sum(!is.na(responseGrammatical)), 
                          n_subjects = length(unique(subject)),
                          n_yes = sum(responseGrammatical),
                          avgtime = mean(time, na.rm = T))
}

mutate_compute_dprime <- function(df) {
  df %>% mutate(amb_dprime = compute_dprime(amb_n, amb_n_yes, none_n, none_n_yes),
                high_dprime = compute_dprime(high_n, high_n_yes, none_n, none_n_yes),
                low_dprime = compute_dprime(low_n, low_n_yes, none_n, none_n_yes),
                amb_avgtime = (amb_avgtime + none_avgtime)/2,
                high_avgtime = (high_avgtime + none_avgtime)/2,
                low_avgtime = (low_avgtime + none_avgtime)/2,
                amb_ndprime = amb_n + none_n,
                high_ndprime = high_n + none_n,
                low_ndprime = low_n + none_n,
                amb_nsubjects = pmin(amb_n_subjects, none_n_subjects),
                high_nsubjects = pmin(high_n_subjects, none_n_subjects),
                low_nsubjects = pmin(low_n_subjects, none_n_subjects)
  )
}



samples_compute_target_differences <- function(samp)
{
    samp %<>% mutate(fem  = (fem_amb+fem_high+fem_low)/3, 
                     masc = (masc_amb+masc_high+masc_low)/3 )
    samp %<>% mutate(amb  = (fem_amb+masc_amb)/2, 
                     high = (fem_high+masc_high)/2, 
                     low  = (fem_low+masc_low)/2)
    samp %<>% mutate(masc_m_fem = (masc-fem))
    samp %<>% mutate(amb_m_high = (amb-high),
                     amb_m_low  = (amb-low),
                     high_m_low = (high-low))

    samp %<>% mutate(masc__amb_m_high = (masc_amb-masc_high),
                     masc__amb_m_low  = (masc_amb-masc_low),
                     masc__high_m_low = (masc_high-masc_low))
    samp %<>% mutate(fem__amb_m_high = (fem_amb-fem_high),
                     fem__amb_m_low  = (fem_amb-fem_low),
                     fem__high_m_low = (fem_high-fem_low) )
    
    samp %<>% mutate(masc_by_amb_m_high = (masc_amb-masc_high) - (fem_amb-fem_high),
                     masc_by_amb_m_low  = (masc_amb-masc_low) - (fem_amb-fem_low),
                     masc_by_high_m_low = (masc_high - masc_low) - (fem_high - fem_low) )

    samp %<>% mutate(intercept = (fem_amb+fem_high+fem_low+masc_amb+masc_high+masc_low)/6)
    samp
}


samples_expand_to_conditions <- function(contrasts, samples, fn)
{
  plyr::ddply(contrasts, c("full_condition"), function(contr) {
      val <- fn(contr)
      data.frame(idx = 1:length(val), val=val)
  }) %>% 
  tidyr::pivot_wider(names_from = "full_condition", values_from = "val")
}

subject_adjustment_template <- function(x, samples, subject) {
    function(y) {
        if (is.null(subject)) {
          return (0)
        } 
        rname <- paste0("r_",x, "[", subject, ",", y, "]")
        #sdname <- paste0("sd_",x, "_", y)
        samples[[ rname ]] #* samples[[ sdname ]] 
    }
}


samples_extract_asymptotes <- function(contrasts, samples, subject=NULL)
{
  subject_adjustment <- subject_adjustment_template("subject__dprimeAsymptote", samples, subject)
  
  samp_asym_unconstr <- 
    samples_expand_to_conditions(contrasts, samples, 
        function(contr) {
          with(contr, with(samples, 
                b_dprimeAsymptote_Intercept + subject_adjustment("Intercept") +
                  cGenderMasc * (b_dprimeAsymptote_cGenderMasc + subject_adjustment("cGenderMasc")) +
                  cLowVsAvg * (b_dprimeAsymptote_cLowVsAvg + subject_adjustment("cLowVsAvg")) + 
                  cHighVsAvg * (b_dprimeAsymptote_cHighVsAvg + subject_adjustment("cHighVsAvg")) +
                  cGenderMasc*cLowVsAvg * (`b_dprimeAsymptote_cGenderMasc:cLowVsAvg` + subject_adjustment("cGenderMasc:cLowVsAvg")) + 
                  cGenderMasc*cHighVsAvg * (`b_dprimeAsymptote_cGenderMasc:cHighVsAvg` + subject_adjustment("cGenderMasc:cHighVsAvg"))
          ))
        }
    )
  
  samp_asym <- samp_asym_unconstr
  samp_asym[,-1] %<>% exp()
  samp_asym_unconstr %<>% samples_compute_target_differences()
  samp_asym %<>% samples_compute_target_differences()
  
  list(constrained = samp_asym, unconstrained = samp_asym_unconstr)
}


samples_extract_invrates <- function(contrasts, samples, subject = NULL)
{
  subject_adjustment <- subject_adjustment_template("subject__dprimeInvrate", samples, subject)
  
  samp_invrate_unconstr <- 
    samples_expand_to_conditions(contrasts, samples, 
      function(contr) {
        with(contr, with(samples, 
           b_dprimeInvrate_Intercept + subject_adjustment("Intercept") + 
             cGenderMasc * (b_dprimeInvrate_cGenderMasc + subject_adjustment("cGenderMasc")) + 
             cLowVsAvg * (b_dprimeInvrate_cLowVsAvg + subject_adjustment("cLowVsAvg")) + 
             cHighVsAvg * (b_dprimeInvrate_cHighVsAvg + subject_adjustment("cHighVsAvg")) +
             cGenderMasc*cLowVsAvg * (`b_dprimeInvrate_cGenderMasc:cLowVsAvg` + subject_adjustment("cGenderMasc:cLowVsAvg")) + 
             cGenderMasc*cHighVsAvg * (`b_dprimeInvrate_cGenderMasc:cHighVsAvg` + subject_adjustment("cGenderMasc:cHighVsAvg"))
        ))
      }
  )
  
  samp_invrate <- samp_invrate_unconstr
  samp_invrate[,-1] %<>% exp()
  samp_invrate_unconstr %<>% samples_compute_target_differences()
  samp_invrate %<>% samples_compute_target_differences()
  
  list(constrained = samp_invrate, unconstrained = samp_invrate_unconstr)
}


samples_extract_intercept <- function(contrasts, samples, subject = NULL)
{
  subject_adjustment <- subject_adjustment_template("subject__dprimeIntercept", samples, subject)
  
  samp_intercept_unconstr <- 
    samples_expand_to_conditions(contrasts, samples, 
      function(contr) {
        with(contr, with(samples, 
             b_dprimeIntercept_Intercept + subject_adjustment("Intercept") + 
               cGenderMasc * (b_dprimeIntercept_cGenderMasc + subject_adjustment("cGenderMasc")) + 
               cLowVsAvg * (b_dprimeIntercept_cLowVsAvg + subject_adjustment("cLowVsAvg")) + 
               cHighVsAvg * (b_dprimeIntercept_cHighVsAvg + subject_adjustment("cHighVsAvg")) +
               cGenderMasc*cLowVsAvg * (`b_dprimeIntercept_cGenderMasc:cLowVsAvg` + subject_adjustment("cGenderMasc:cLowVsAvg")) + 
               cGenderMasc*cHighVsAvg * (`b_dprimeIntercept_cGenderMasc:cHighVsAvg` + subject_adjustment("cGenderMasc:cHighVsAvg"))
        ))
      }
    )
  
  samp_intercept <- samp_intercept_unconstr
  samp_intercept[,-1] %<>% add(-1) %>% exp()
  samp_intercept_unconstr %<>% samples_compute_target_differences()
  samp_intercept %<>% samples_compute_target_differences()
  
  list(constrained = samp_intercept, unconstrained = samp_intercept_unconstr)
}


extract_contrasts <- function(data)
{
  contr <- data %>% 
              dplyr::select(full_condition, condition, cGenderMasc, cLowVsAvg, cHighVsAvg) %>% 
              unique() %>% subset(condition != "none") %>%
              mutate( full_condition = full_condition %>% gsub("-", "_", .) ) #%>%
              #dplyr::select(-full_condition)
  rownames(contr) <- contr$full_condition
  contr
}

samples_summary <- function(fit_brms, contr, contr_names, subject = NULL)
{
    names_fixef <- fixef(fit_brms) %>% rownames %>% paste0("b_", .)
    fit_samples <- brms::posterior_samples(fit_brms)#, par = names_fixef)
    
    samples <- list()
    samples$asym <- samples_extract_asymptotes(contrasts = contr, samples = fit_samples, subject = subject)
    samples$invrate <- samples_extract_invrates(contrasts = contr, samples = fit_samples, subject = subject)
    samples$intercept <- samples_extract_intercept(contrasts = contr, samples = fit_samples, subject = subject)
    
    samples$asym$constrained %<>% dplyr::select(all_of( c("idx", contr_names) ))
    samples$invrate$constrained %<>% dplyr::select(all_of( c("idx", contr_names) ))
    samples$intercept$constrained %<>% dplyr::select(all_of( c("idx", contr_names) ))
    
    # if (!is.null(contr_names)) {
    #   contr_names <- names(contr_names)
    # }
    
    samp_params <- bind_rows(samples$asym$constrained %T>% {.$param <- "asymptote"},
                             samples$invrate$constrained %T>% {.$param <- "invrate"},
                             samples$intercept$constrained %T>% {.$param <- "intercept"}
                             ) 
    
    summary_params <- samp_params %>% 
      tidyr::pivot_longer(all_of(names(contr_names)), names_to = "label", values_to = "val") %>%
      group_by(param, label) %>% 
      dplyr::summarize(
                lower90 = bayestestR::hdi(val, ci = .9)$CI_low,
                upper90 = bayestestR::hdi(val, ci = .9)$CI_high,
                lower80 = bayestestR::hdi(val, ci = .8)$CI_low,
                upper80 = bayestestR::hdi(val, ci = .8)$CI_high,
                mid = bayestestR::map_estimate(val)[1], 
                p_below_zero = mean(val < 0) )
    
    
    summary_params$label %<>% factor(levels = c(rev(names(contr_names)), "intercept") )
    summary_params$param %<>% factor(levels = c("intercept", "invrate", "asymptote") )
    summary_params$name <- summary_params$label %>% dplyr::recode(!!!contr_names)
    
    summary_params
}

summarize_dprime <- function(data_nyes, by_subject)
{
  # now bring the table into a wide format to compute d-primes
  data_nyes %<>% tidyr::gather(score, value, n, n_yes, avgtime, n_subjects) %>% 
    tidyr::unite(condition_score, condition, score) %>% 
    tidyr::spread(condition_score, value)
  
  # compute the d', and average time of response by subject, by condition, by time point
  data_dprime <- data_nyes %>% mutate_compute_dprime()
  
  selected_columns <- c("interval",
                       "amb_dprime", "high_dprime", "low_dprime",
                       "amb_avgtime", "high_avgtime", "low_avgtime",
                       "amb_ndprime", "high_ndprime", "low_ndprime",
                       "amb_nsubjects", "high_nsubjects", "low_nsubjects")
  if (by_subject) {
    selected_columns %<>% c("subject", .)
  }
  data_dprime %<>% dplyr::select(all_of(selected_columns))

  data_dprime %<>% tidyr::gather(var, val, amb_dprime:low_nsubjects) %>%
                   tidyr::separate(var, c("condition", "var") ) %>%
                   tidyr::spread(var, val)
  data_dprime$ndprime <- with(data_dprime, ifelse(is.na(ndprime), 0, ndprime))
  
  data_dprime
}


fn_map <- function(condition) {
  function(time) { dprime_fn(time, 0, 
                             asymptote_unconstrained = bayestestR::map_estimate(samp_asym$unconstrained[[condition]]), 
                             invrate_unconstrained = bayestestR::map_estimate(samp_invrate$unconstrained[[condition]]), 
                             intercept_unconstrained = bayestestR::map_estimate(samp_intercept$unconstrained[[condition]]))
  }
}

fn_index <- function(condition, idx) {
  samp_asymptote_unconstrained = samp_asym$unconstrained[idx, condition] 
  samp_invrate_unconstrained = samp_invrate$unconstrained[idx, condition]
  samp_intercept_unconstrained = samp_intercept$unconstrained[idx, condition]
  fn <- function(time) { 
    dprime_fn(time, 0, 
              asymptote_unconstrained = samp_asymptote_unconstrained, 
              invrate_unconstrained = samp_invrate_unconstrained, 
              intercept_unconstrained = samp_intercept_unconstrained)
  }
  fn(0)
  fn
}



