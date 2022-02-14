
library(tidyverse)
library(magrittr)
library(brms)
library(kdensity)


fit1 <- readRDS(file = "../workspace/model_fits/fit_all_uncorr_bysubj_2_4000.rds")
fit2 <- readRDS(file = "../workspace/model_fits/fit_all_uncorr_bysubj_2_prior.rds")

fixef_names <- fixef(fit1) %>% rownames() %>% paste0("b_", .)

s1 <- posterior_samples(fit1)
s2 <- posterior_samples(fit2)

tau <- function(par_name, s_posterior, s_prior) {
  par_min <- min(c(s_posterior[[par_name]], s_prior[[par_name]]))
  par_max <- max(c(s_posterior[[par_name]], s_prior[[par_name]]))
  d_prior = kdensity(s_prior[[par_name]])
  d_posterior = kdensity(s_posterior[[par_name]])
  integrate(function(x) pmin(d_prior(x), d_posterior(x)), lower = par_min*2, upper = par_max*2)
}

# x <- tau(par_name = fixef_names[[1]], s_posterior = s1, s_prior = s2)
# x[[1]]

taus <- sapply(fixef_names, function(fixef_name) tau(par_name = fixef_name, s_posterior = s1, s_prior = s2)[[1]])
df_taus <- data.frame(name = names(taus) %>% gsub("b_", "", .), tau = taus)

name_components <- df_taus$name %>% stringr::str_split_fixed(pattern="_", n=2) 
df_taus$satf_par <- name_components[,1]
df_taus$contrasts <- name_components[,2]

df_taus %<>% mutate( fn = ifelse(grepl("^dprime", satf_par), "dprime", "crit") )
df_taus$satf_par <- sapply(1:nrow(df_taus), function(i) gsub(df_taus$fn[i], "", df_taus$satf_par[i]))

df_taus_dprime <- df_taus %>% subset(fn == "dprime")
df_taus_crit <- df_taus %>% subset(fn != "dprime")

df_taus_dprime$contrasts %<>% gsub(":", "\n× ", .)
df_taus_dprime$contrasts %<>% factor(levels = (unique(.)))
p1 <- df_taus_dprime %>% ggplot(aes(contrasts, tau)) + geom_bar(stat = "identity") + facet_wrap(~satf_par, ncol = 1)
p1 <- p1 + theme(axis.text.x = element_text(angle = 45, vjust = .55)) # , vjust = 0 # , vjust = 0.0, hjust = 0
p1 <- p1 + theme_bw() + geom_hline(yintercept = 0.35, color = "red", linetype = "dashed")

p1

df_taus_crit$contrasts %<>% factor(levels = (unique(.)))
p2 <- df_taus_crit %>% ggplot(aes(contrasts, tau)) + geom_bar(stat = "identity") + facet_wrap(~satf_par, ncol = 1)
p2 <- p2 + theme(axis.text.x = element_text(angle = 45, vjust = .55)) # , vjust = 0 # , vjust = 0.0, hjust = 0
p2 <- p2 + theme_bw() + geom_hline(yintercept = 0.35, color = "red", linetype = "dashed")

ggpubr::ggarrange(p1, p2)

# for comparison:
# tau is 0.35 when
# - N(0, 10) -> N(0, 2)
# - N(0, 1) -> N(1.85, 1)
fn_prior <- function(x) dnorm(x, mean = 0, sd=1)
fn_posterior <- function(x) dnorm(x, mean = 1.85, sd=1)
integrate(function(x) pmin(fn_prior(x), fn_posterior(x)), lower = -100, upper = 100)

