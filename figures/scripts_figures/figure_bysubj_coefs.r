
fnames_singlesubj_uncorr <- dir("../workspace/model_fits/models_singlesubj_uncorr", full.names = T)

contr <- extract_contrasts(data_rc)

summary_params_bysubj <-
  plyr::ldply(fnames_singlesubj_uncorr, function(fname) {
    fit_brms <- readRDS(fname)
    df <- samples_summary(fit_brms = fit_brms, contr = contr, contr_names = target_contr_names)
    df$subj <- basename(fname) %>% gsub(".rds", "", .)
    df
  })
summary_params_bysubj$subj %<>% gsub("subj_", "", .)

#subjects <- summary_params_bysubj$subj %>% sort() %>% unique()
#summary_params_bysubj$subj %<>% factor(., levels = rev(sort(as.integer(subjects))))
#summary_params_bysubj$name %<>% factor(., levels = (names(target_contr_names)))

# if (F) {
#   p_single <-
#     summary_params_bysubj %>% 
#     ggplot(aes(mid, subj)) +
#     geom_point() + 
#     geom_errorbarh(aes(xmin=lower95, xmax=upper95), height=0.0) + 
#     geom_errorbarh(aes(xmin=lower80, xmax=upper80), height=0.0, size=1) + 
#     facet_grid(name~param, scale = "free_x", switch = "y") + xlab("") + ylab("") + 
#     geom_vline(xintercept = 0, color = "red", alpha = 1) + # linetype = "dotted", 
#     theme_bw()
#   
#   print(p_single)
# }
# 
# 
# if (F)
# {
#   
#   summary_params_bysubj %>% subset(subj == 5 & param == "asymptote")
#   
#   
#   # XXX
#   a <- summary_params_bysubj %>% subset(name %in% c("amb-low") & param == "intercept" & subj != 5)
#   prod(a$p_below_zero)^(1/20)
#   
#   
#   x <- summary_params_bysubj %>% dplyr::select(subj, param, name, mid, lower95, upper95) %>% subset(name %in% c("amb-high", "amb-low"))
#   z <- x %>% pivot_wider(id_cols = c("subj", "name"), names_from = c("param"), values_from = c("mid", "lower95", "upper95"))
#   
#   ggplot(z, aes(mid_intercept, mid_asymptote)) + geom_point() + geom_errorbar(aes(ymin=lower95_asymptote, ymax=upper95_asymptote), alpha=.3) + geom_errorbarh(aes(xmin=lower95_intercept, xmax=upper95_intercept), alpha=.3) + facet_wrap(~name) + geom_vline(xintercept = 0, color="red") + geom_hline(yintercept = 0, color="red")
#   
# }


###
if (T) {
  
  subjects <- unique(fit_brms$data$subject) %>% as.character() %>% as.integer() %>% sort()
  
  summary_params_bysubj_hierarchical <-
    plyr::ldply(subjects, function(subj) {
      df <- samples_summary(fit_brms = fit_brms, contr = contr, contr_names = target_contr_names, subject = subj)
      df$subj <- subj
      df
    })
  
  summary_params_bysubj$model_type <- "single-subject"
  summary_params_bysubj_hierarchical$model_type <- "hierarchical"
  summary_params_bysubj$subj %<>% as.character() %>% as.integer()
  summary_params_bysubj_all <- bind_rows(summary_params_bysubj, summary_params_bysubj_hierarchical)
  
  subj_labels <- paste(rep(subjects, each=2), c("[S]", "[H]"))
  summary_params_bysubj_all$subj_label <- with(summary_params_bysubj_all, paste(subj, ifelse(model_type == "separate", "[S]", "[H]") ))
  summary_params_bysubj_all$subj_label %<>% factor(rev(subj_labels))
  
  summary_params_bysubj_all$subj %<>% as.factor()
  
  p_h <-
    summary_params_bysubj_all %>% subset(model_type == "hierarchical") %>%
    ggplot(aes(mid, subj), position = position_dodge(width = .1) ) +
    geom_point() + 
    geom_errorbarh(aes(xmin=lower90, xmax=upper90), height=0.5) + 
    geom_errorbarh(aes(xmin=lower80, xmax=upper80), height=0.0, size=1) + 
    facet_grid(name~param, scale = "free_x", switch = "y") + xlab("") + ylab("") + 
    geom_vline(xintercept = 0, color = "grey", alpha = 1) + # linetype = "dotted", 
    theme_bw() + theme(legend.position = "top")

  # p_s <-
  #   summary_params_bysubj_all %>% subset(model_type == "single-subject") %>%
  #   ggplot(aes(mid, subj), position = position_dodge(width = .1) ) +
  #   geom_point() + 
  #   geom_errorbarh(aes(xmin=lower90, xmax=upper90), height=0.5) + 
  #   geom_errorbarh(aes(xmin=lower80, xmax=upper80), height=0.0, size=1) + 
  #   facet_grid(name~param, scale = "free_x", switch = "y") + xlab("") + ylab("") + 
  #   geom_vline(xintercept = 0, color = "grey", alpha = 1) + # linetype = "dotted", 
  #   theme_bw() + theme(legend.position = "top")
  
  z = 10
  ggsave(p_h, file = fname_coefs_bysubj, width = 1*z, height = 1*z, device = cairo_pdf)
  
}


