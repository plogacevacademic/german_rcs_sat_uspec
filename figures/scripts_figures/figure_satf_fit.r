
# plot of average of by-participant d-primes
p <- data_rc_bysubj_dprime %>%
  group_by(interval, condition) %>% 
  dplyr::summarise(avg_time = mean(avgtime, na.rm = T), 
                   avg_dprime = mean(dprime, na.rm = T),
                   ndprime = sum(ndprime),
                   n_subjects = length(unique(subject))) %T>%
      { .$condition %<>% dplyr::recode("amb"="ambiguous") } %>%
  ggplot(aes(avg_time, avg_dprime, color = condition)) + geom_point() + 
  geom_point(shape=1, color="black") + theme_bw() + theme(legend.position="top") #+ facet_wrap(~gender)


set.seed(12345)
n_samples_fn <- 250
idx_fn <- sample(1:nrow(samp_asym$unconstrained), n_samples_fn)

for (idx in idx_fn) {
  p <- p + stat_function(fun = fn_index("amb", idx), n = 1000, color = "red", alpha = .05)
  p <- p + stat_function(fun = fn_index("high", idx), n = 1000, color = "green", alpha = .05)
  p <- p + stat_function(fun = fn_index("low", idx), n = 1000, color = "blue", alpha = .05)
}

p <- p + stat_function(fun = fn_map("amb"), n = 1000, color = "red") +
  stat_function(fun = fn_map("high"), n = 1000, color = "green") +
  stat_function(fun = fn_map("low"), n = 1000, color = "blue")

p <- p + xlab("Time since onset of the RC (in seconds)") + ylab("Sensitivity (d')")

ggsave(filename = fname_avg_dprime_plot, plot=p, width=6, height=4)
