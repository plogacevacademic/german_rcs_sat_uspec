

{
  target_contr_names <- c("relativizer gender\n(masc-fem)"="masc_m_fem",
                          "attachment\n(ambiguous-high)"="amb_m_high",
                          "attachment\n(ambiguous-low)"="amb_m_low", 
                          "gender × [ambiguous-high]"="masc_by_amb_m_high",
                          "gender × [ambiguous-low]"="masc_by_amb_m_low"
                          )
  
  intercepts <- data.frame(idx = 1:nrow(samp_asym$constrained),
                           asymptote = samp_asym$constrained$intercept, 
                           invrate = samp_invrate$constrained$intercept, 
                           intercept = samp_intercept$constrained$intercept)
  intercepts %<>% tidyr::pivot_longer(c("asymptote", "invrate", "intercept"), names_to = "label", values_to = "val")
  intercepts %<>% group_by(label) %>% 
    dplyr::summarize(lower90 = bayestestR::hdi(val, ci = .9)$CI_low,
                     upper90 = bayestestR::hdi(val, ci = .9)$CI_high,
                     lower80 = bayestestR::hdi(val, ci = .8)$CI_low,
                     upper80 = bayestestR::hdi(val, ci = .8)$CI_high,
                     mid = bayestestR::map_estimate(val)[1] )
  intercepts <- intercepts %T>% {.$param_type <- paste0(rep(" ", 29), collapse="")}
  intercepts %<>% as.data.frame %T>% {rownames(.) <- .$label}
  
  slopes <- samples_summary(fit_brms = fit_brms, contr = contr, contr_names = target_contr_names) %T>% 
    {.$param %<>% factor(c("intercept","invrate","asymptote")) }
  slopes %<>% as.data.frame %T>% {rownames(.) <- paste0(.$param, "__", .$name) }
  
  
  plots <- list()
  df <- intercepts %>% subset(label == "intercept") %T>% {.$param_type <- "xxxxxxxxxxxxxxxxxxxxxxxxx"} 
  plots$intercept <- df %>% ggplot(aes(mid, param_type)) + xlab("seconds") + 
    scale_x_continuous(limits = c(.3,.5), breaks=c(.3, .4, .5)) + 
    facet_wrap(~"average δ")

  df <- intercepts %>% subset(label == "invrate")
  plots$invrate <- df %>% ggplot(aes(mid, param_type)) + xlab("seconds") + 
    scale_x_continuous(limits=c(.5, .8), breaks=c(.5, .65, .8)) + 
    facet_wrap(~"average 1/β") + scale_y_discrete(breaks=NULL)
  
  df <- intercepts %>% subset(label == "asymptote")
  plots$asymptote <- intercepts %>% subset(label == "asymptote") %>% ggplot(aes(mid, param_type)) + xlab("d' units") + 
    scale_x_continuous(limits=c(1.9,3), breaks=c(2, 2.5, 3)) + 
    facet_wrap(~"average λ") + scale_y_discrete(breaks=NULL)
  
  plots$delta_intercept2 <- slopes %>% subset(param == "intercept") %>% ggplot(aes(mid, label)) + xlab("seconds") +
    scale_x_continuous(limits = c(-.21, .2), breaks = c(-.2, -.1, 0, .1, .2)) + 
    facet_wrap(~"effect on δ") #"\u394 x-intercept"
  plots$delta_invrate2 <- slopes %>% subset(param == "invrate") %>% ggplot(aes(mid, label)) + xlab("seconds") + 
    scale_x_continuous(limits = c(-.25, .27), breaks = c(-.2, -.1, 0, .1, .2)) + 
    facet_wrap(~"effect on 1/β") + scale_y_discrete(labels = NULL, breaks=NULL)
  plots$delta_asymptote2 <- slopes %>% subset(param == "asymptote") %>% ggplot(aes(mid, label)) + xlab("d' units") + 
    scale_x_continuous(limits = c(-.75, 1.75), breaks = c(-.5, 0, .5, 1, 1.5)) + 
    facet_wrap(~"effect on λ" ) + scale_y_discrete(labels = NULL, breaks=NULL)
  
  for (i in 1:length(plots)) {
    plots[[i]] <- plots[[i]] + geom_point(size=3) + 
      geom_errorbarh(aes(xmin=lower90, xmax=upper90), height=0.1) + 
      geom_errorbarh(aes(xmin=lower80, xmax=upper80), height=0.00, size=2) + 
      theme_bw() + ylab("") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    if (i >= 4) {
      plots[[i]] <- plots[[i]] + geom_vline(xintercept = 0, color = "red", linetype = "dotted") # 
    }
  }
  
  plots$intercept <- plots$intercept + theme(axis.text.y = element_text(color="white"))

  {
    p1 <- ggarrange(plotlist = plots[1:3], nrow = 1, widths = c(1.75, 1, 1))
    p2 <- ggarrange(plotlist = plots[4:6], nrow = 1, widths = c(1.75, 1, 1))
    p <- ggarrange(p1, p2, nrow=2, heights = c(1.25,3))
  }
  
  z = 4
  ggsave(p, file = fname_coefs, width = 2*z, height = 1*z, device = cairo_pdf)
}

