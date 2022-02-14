
###############################
### parameter CIs 
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


###############################
### prior CIs 
{
  target_contr_names <- c("relativizer gender\n(masc-fem)"="masc_m_fem",
                          "attachment\n(ambiguous-high)"="amb_m_high",
                          "attachment\n(ambiguous-low)"="amb_m_low", 
                          "gender × [ambiguous-high]"="masc_by_amb_m_high",
                          "gender × [ambiguous-low]"="masc_by_amb_m_low"
  )
  
  intercepts <- data.frame(idx = 1:nrow(samp_asym_prior$constrained),
                           asymptote = samp_asym_prior$constrained$intercept, 
                           invrate = samp_invrate_prior$constrained$intercept, 
                           intercept = samp_intercept_prior$constrained$intercept)
  intercepts %<>% tidyr::pivot_longer(c("asymptote", "invrate", "intercept"), names_to = "label", values_to = "val")
  intercepts %<>% group_by(label) %>% 
    dplyr::summarize(lower90 = bayestestR::hdi(val, ci = .9)$CI_low,
                     upper90 = bayestestR::hdi(val, ci = .9)$CI_high,
                     lower80 = bayestestR::hdi(val, ci = .8)$CI_low,
                     upper80 = bayestestR::hdi(val, ci = .8)$CI_high,
                     mid = bayestestR::map_estimate(val)[1] )
  intercepts <- intercepts %T>% {.$param_type <- paste0(rep(" ", 29), collapse="")}
  intercepts %<>% as.data.frame %T>% {rownames(.) <- .$label}
  
  slopes <- samples_summary(fit_brms = fit_brms_prior, contr = contr, contr_names = target_contr_names) %T>% 
                           {.$param %<>% factor(c("intercept","invrate","asymptote")) }
  slopes %<>% as.data.frame %T>% {rownames(.) <- paste0(.$param, "__", .$name) }

  plots <- list()
  df <- intercepts %>% subset(label == "intercept") %T>% {.$param_type <- "xxxxxxxxxxxxxxxxxxxxxxxxx"} 
  plots$intercept <- df %>% ggplot(aes(mid, param_type)) + xlab("seconds") + 
    scale_x_continuous(limits = c(0, 1), breaks=seq(0, 1, .25)) + #
    facet_wrap(~"average δ")
  
  df <- intercepts %>% subset(label == "invrate")
  plots$invrate <- df %>% ggplot(aes(mid, param_type)) + xlab("seconds") + 
    scale_x_continuous( limits=c(.5, 2), breaks=seq(0, 2, .5)) + # 
    facet_wrap(~"average 1/β") + scale_y_discrete(breaks=NULL)
  
  df <- intercepts %>% subset(label == "asymptote")
  plots$asymptote <- intercepts %>% subset(label == "asymptote") %>% ggplot(aes(mid, param_type)) + xlab("d' units") + 
    scale_x_continuous(limits=c(0,3), breaks=c(0, 1, 2, 3)) + #
    facet_wrap(~"average λ") + scale_y_discrete(breaks=NULL)
  
  plots$delta_intercept2 <- slopes %>% subset(param == "intercept") %>% ggplot(aes(mid, label)) + xlab("seconds") +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .5)) + 
    facet_wrap(~"effect on δ") #"\u394 x-intercept"
  plots$delta_invrate2 <- slopes %>% subset(param == "invrate") %>% ggplot(aes(mid, label)) + xlab("seconds") + 
    scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 1)) + 
    facet_wrap(~"effect on 1/β") + scale_y_discrete(labels = NULL, breaks=NULL)
  plots$delta_asymptote2 <- slopes %>% subset(param == "asymptote") %>% ggplot(aes(mid, label)) + xlab("d' units") + 
    scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 1)) + 
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
  
  print(p)
  
  z = 4
  ggsave(p, file = fname_coef_priors, width = 2*z, height = 1*z, device = cairo_pdf)
}


# ###############################
# ### parameter CIs, broken down by gender 
# {
#   target_contr_names <- c("relativizer gender\n(masc-fem)"="masc_m_fem",
#                           "fem: (ambiguous-high)"="fem__amb_m_high",
#                           "fem: (ambiguous-low)"="fem__amb_m_low",
#                           "masc: (ambiguous-high)"="masc__amb_m_high",
#                           "masc: (ambiguous-low)"="masc__amb_m_low"
#                           )
#   
#   intercepts <- data.frame(idx = 1:nrow(samp_asym$constrained),
#                            asymptote = samp_asym$constrained$intercept, 
#                            invrate = samp_invrate$constrained$intercept, 
#                            intercept = samp_intercept$constrained$intercept)
#   intercepts %<>% tidyr::pivot_longer(c("asymptote", "invrate", "intercept"), names_to = "label", values_to = "val")
#   intercepts %<>% group_by(label) %>% 
#     dplyr::summarize(lower90 = bayestestR::hdi(val, ci = .9)$CI_low,
#                      upper90 = bayestestR::hdi(val, ci = .9)$CI_high,
#                      lower80 = bayestestR::hdi(val, ci = .8)$CI_low,
#                      upper80 = bayestestR::hdi(val, ci = .8)$CI_high,
#                      mid = bayestestR::map_estimate(val)[1] )
#   intercepts <- intercepts %T>% {.$param_type <- paste0(rep(" ", 29), collapse="")}
#   intercepts %<>% as.data.frame %T>% {rownames(.) <- .$label}
#   
#   slopes <- samples_summary(fit_brms = fit_brms, contr = contr, contr_names = target_contr_names) %T>% 
#     {.$param %<>% factor(c("intercept","invrate","asymptote")) }
#   slopes %<>% as.data.frame %T>% {rownames(.) <- paste0(.$param, "__", .$name) }
#   
#   
#   plots <- list()
#   df <- intercepts %>% subset(label == "intercept") %T>% {.$param_type <- "xxxxxxxxxxxxxxxxxxxxxxxxx"} 
#   plots$intercept <- df %>% ggplot(aes(mid, param_type)) + xlab("seconds") + 
#     scale_x_continuous(limits = c(.3,.5), breaks=c(.3, .4, .5)) + 
#     facet_wrap(~"average δ")
#   
#   df <- intercepts %>% subset(label == "invrate")
#   plots$invrate <- df %>% ggplot(aes(mid, param_type)) + xlab("seconds") + 
#     scale_x_continuous(limits=c(.5, .8), breaks=c(.5, .65, .8)) + 
#     facet_wrap(~"average 1/β") + scale_y_discrete(breaks=NULL)
#   
#   df <- intercepts %>% subset(label == "asymptote")
#   plots$asymptote <- intercepts %>% subset(label == "asymptote") %>% ggplot(aes(mid, param_type)) + xlab("d' units") + 
#     scale_x_continuous(limits=c(1.9,3), breaks=c(2, 2.5, 3)) + 
#     facet_wrap(~"average λ") + scale_y_discrete(breaks=NULL)
#   
#   plots$delta_intercept2 <- slopes %>% subset(param == "intercept") %>% ggplot(aes(mid, label)) + xlab("seconds") +
#     scale_x_continuous(limits = c(-.21, .2), breaks = c(-.2, -.1, 0, .1, .2)) + 
#     facet_wrap(~"effect on δ") #"\u394 x-intercept"
#   plots$delta_invrate2 <- slopes %>% subset(param == "invrate") %>% ggplot(aes(mid, label)) + xlab("seconds") + 
#     scale_x_continuous(limits = c(-.25, .35), breaks = c(-.2, -.1, 0, .1, .2)) + 
#     facet_wrap(~"effect on 1/β") + scale_y_discrete(labels = NULL, breaks=NULL)
#   plots$delta_asymptote2 <- slopes %>% subset(param == "asymptote") %>% ggplot(aes(mid, label)) + xlab("d' units") + 
#     scale_x_continuous(limits = c(-.75, 2.1), breaks = c(-.5, 0, .5, 1, 1.5)) + 
#     facet_wrap(~"effect on λ" ) + scale_y_discrete(labels = NULL, breaks=NULL)
#   
#   for (i in 1:length(plots)) {
#     plots[[i]] <- plots[[i]] + geom_point(size=3) + 
#       geom_errorbarh(aes(xmin=lower90, xmax=upper90), height=0.1) + 
#       geom_errorbarh(aes(xmin=lower80, xmax=upper80), height=0.00, size=2) + 
#       theme_bw() + ylab("") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#     if (i >= 4) {
#       plots[[i]] <- plots[[i]] + geom_vline(xintercept = 0, color = "red", linetype = "dotted") # 
#     }
#   }
#   
#   plots$intercept <- plots$intercept + theme(axis.text.y = element_text(color="white"))
#   
#   {
#     p1 <- ggarrange(plotlist = plots[1:3], nrow = 1, widths = c(1.75, 1, 1))
#     p2 <- ggarrange(plotlist = plots[4:6], nrow = 1, widths = c(1.75, 1, 1))
#     p <- ggarrange(p1, p2, nrow=2, heights = c(1.25,3))
#   }
#   
#   p
#   
#   # z = 4
#   # ggsave(p, file = fname_coefs_by_gender, width = 2*z, height = 1*z, device = cairo_pdf)
# }



###############################
### parameter correlations 
{
  library(GGally)
  
  ggally_densityx <- function (data, mapping, ...) 
  {
    relative_x = 0.75
    relative_y = 0.9
    samples_alpha = 0.2
    n_samples = 1000
    idx_samples = sample(1:nrow(data), n_samples)

    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)
    rangeX <- range(x, na.rm = TRUE)
    rangeY <- range(y, na.rm = TRUE)
    p <- ggplot(data = data) + 
          geom_point(data = data.frame(rangeX = rangeX, rangeY = rangeY), 
                     mapping = aes(x = rangeX, y = rangeY), alpha = 0)
    
    #p <- p + geom_point(data = data.frame(), aes(x[idx_samples], y[idx_samples]), alpha = samples_alpha)
    
    if (!is.null(mapping$fill)) {
      p <- p + stat_density2d(mapping = mapping, geom = "polygon", ...) + 
              scale_fill_gradient(low = "#f3faff", # "lightblue",
                                  high = "darkblue", guide = "none") +
              scale_alpha(range = c(0.00, 0.5), guide = "none")
    }
    else {
      p <- p + geom_density2d(mapping = mapping, ...)
    }
    
    p <- p + scale_x_continuous(breaks = seq(-1, 1, by = .1))
    p + geom_label(data = data.frame(), 
                   aes(x=rangeX[1] + diff(rangeX)*relative_x, y=rangeY[1] + diff(rangeY)*relative_y), 
                   label = sprintf("R = %0.2f", cor(x,y) ))  
  }
  
  mapping <- aes(intercept, invrate, fill = ..level..)
  
  p1 <- data.frame(invrate = samp_invrate$constrained$intercept,
                   intercept = samp_intercept$constrained$intercept) %>% 
        ggally_densityx(mapping) +
        facet_wrap(~"grand mean") + xlab("average δ") + ylab("average 1/β")

  p2 <- data.frame(invrate = samp_invrate$constrained$masc_m_fem,
                   intercept = samp_intercept$constrained$masc_m_fem) %>% 
        ggally_densityx(mapping) +
        facet_wrap(~"relativizer gender: masc-fem") + xlab("Δδ") + ylab("Δ1/β") +
        geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
        geom_vline(xintercept = 0, color = "red", linetype = "dotted")

  p3 <- data.frame(invrate = samp_invrate$constrained$amb_m_high,
                   intercept = samp_intercept$constrained$amb_m_high) %>% 
        ggally_densityx(mapping) +
        facet_wrap(~"attachment: ambiguous-high") + xlab("Δδ") + ylab("Δ1/β") +
        geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
        geom_vline(xintercept = 0, color = "red", linetype = "dotted")

  p4 <- data.frame(invrate = samp_invrate$constrained$amb_m_low,
                   intercept = samp_intercept$constrained$amb_m_low) %>% 
        ggally_densityx(mapping)  +
        facet_wrap(~"attachment: ambiguous-low") + xlab("Δδ") + ylab("Δ1/β") +
        geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
        geom_vline(xintercept = 0, color = "red", linetype = "dotted")

  p5 <- data.frame(invrate = samp_invrate$constrained$masc_by_amb_m_high,
                   intercept = samp_intercept$constrained$masc_by_amb_m_high) %>% 
        ggally_densityx(mapping) +
        facet_wrap(~"interaction: gender × [ambiguous-high]") + xlab("Δδ") + ylab("Δ1/β") +
        geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
        geom_vline(xintercept = 0, color = "red", linetype = "dotted")

  p6 <- data.frame(invrate = samp_invrate$constrained$masc_by_amb_m_low,
                   intercept = samp_intercept$constrained$masc_by_amb_m_low) %>% 
        ggally_densityx(mapping) +
        facet_wrap(~"interaction: gender × [ambiguous-low]") + xlab("Δδ") + ylab("Δ1/β") +
        geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
        geom_vline(xintercept = 0, color = "red", linetype = "dotted")
  
  
  
  library(ggpubr)
  
  p <- ggpubr::ggarrange(p1+ theme_bw(), p2 + theme_bw(), p3 + theme_bw(), p4 + theme_bw(), p5 + theme_bw(), p6 + theme_bw(), ncol =2, nrow = 3)
  
  z = 4
  ggsave(p, file = fname_coefs_corr, width = 2*z, height = 2*z, device = cairo_pdf)
  
}

