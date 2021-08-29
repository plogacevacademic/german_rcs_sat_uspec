
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

summary_params_bysubj_all$param_label <- 
  summary_params_bysubj_all$param %>% 
  dplyr::recode("intercept"="effect on δ", "invrate"="effect on 1/β", "asymptote"="effect on λ")

summary_params_bysubj_all$coef_label <- 
  summary_params_bysubj_all$name %>% 
  dplyr::recode("masc_m_fem" = "relativizer gender", 
                "amb_m_high" = "ambiguous-high",
                "amb_m_low"  = "ambiguous-low",
                "masc_by_amb_m_high" = "gender × (ambiguous-high)",
                "masc_by_amb_m_low"  = "gender × (ambiguous-low)"
                )

summary_params_bysubj_all$coef_label %<>% 
  factor(levels = c("relativizer gender",
                    "ambiguous-high",
                    "ambiguous-low",
                    "gender × (ambiguous-high)",
                    "gender × (ambiguous-low)"
                    ))
summary_params_bysubj_all$subj_label <- sprintf("S %02d", (summary_params_bysubj_all$subj %>% as.character %>% as.integer))

p_h <-
  summary_params_bysubj_all %>% subset(model_type == "hierarchical") %>%
  ggplot(aes(mid, subj_label), position = position_dodge(width = .1) ) +
  geom_point() + 
  geom_errorbarh(aes(xmin=lower90, xmax=upper90), height=0.5) + 
  geom_errorbarh(aes(xmin=lower80, xmax=upper80), height=0.0, size=1) + 
  facet_grid(coef_label~param_label, scale = "free_x", switch = "y") + xlab("") + ylab("") + 
  geom_vline(xintercept = 0, color = "grey", alpha = 1) + # linetype = "dotted", 
  theme_bw() + theme(legend.position = "top")

p_h

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


