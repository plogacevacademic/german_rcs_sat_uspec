
library(tidyverse)
library(magrittr)
library(ggplot2)

cur_arrow <- arrow(length = unit(0.1, "inches"))

(df <- read.table(header = T, text = "
trial   attachment   latency
1       high      60
1       low       40
2       high      50
2       low       70
3       high      30
3       low       40
4       high      35
4       low       30
"))

df <- 
plyr::ldply(c("amb", "high", "low"), 
function(condition) {
  df$condition <- condition
  df
})
df %<>% mutate( latency = ifelse( (condition == "low" & attachment == "high") | (condition == "high" & attachment == "low"), NA, latency) )
df$condition %<>% dplyr::recode("amb"="ambiguous\nattachment\ncondition", "high"="high\nattachment\ncondition", "low"="low\nattachment\ncondition")

df %<>% group_by(condition, trial) %>% mutate(is_fastest = (latency == min(latency, na.rm = T)) )
df$attachment_success <- ifelse( is.na(df$is_fastest) | df$is_fastest, paste(df$attachment, "successful", sep="_"), paste(df$attachment, "timed_out", sep="_") )

cur_theme2 <- 
  theme(legend.position="top", axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), axis.ticks = element_line(colour = "black"), axis.line.x = element_line(),
        panel.background = element_blank(), panel.grid.minor = element_blank(), 
        legend.key = element_blank(), strip.background = element_rect(fill="white", color="black"),
        text = element_text(size = 16))

breaks = c("high_successful", "high_timed_out", "low_successful", "low_timed_out")
labels = c("high attachment", "high attachment, timed out", "low attachment", "low attachment, timed out")

# high (blue)  #00bfc4
# low (green) #7cae00

color_high <- "#00bfc4"
color_low <- "#7cae00"

p_smcm <-
ggplot(df, aes(trial, latency, color = attachment_success, fill = attachment_success, linetype = attachment_success)) + # , alpha = attachment_success
    geom_bar(stat = "identity", position =  "dodge", alpha = .1) + 
    facet_wrap(~condition, scales = "free", nrow = 3, strip.position = "right") + 
    cur_theme2 + scale_y_continuous(breaks = NULL) + xlab(NULL) + ylab("Attachment Completion Time") + 
    scale_x_continuous(breaks = c(1:4,5), labels = c(sprintf("Trial %d", 1:4), "... further trials ..."), limits = c(0.5,5.25)) +
    scale_fill_manual("", breaks = breaks, values = c(color_high, color_high, color_low, color_low), labels = labels) +
    scale_color_manual("", breaks = breaks, values = c(color_high, color_high, color_low, color_low), labels = labels) +
    #scale_alpha_manual(breaks = c("high_successful", "low_successful", "high_timed_out", "low_timed_out"), values = c(1, 1, .2, .2)) +
    scale_linetype_manual("", breaks = breaks, values = c("solid", "dashed", "solid", "dashed"), labels = labels ) #+
    #guides(fill=guide_legend(title=NULL), color=guide_legend(title=NULL), linetype=guide_legend(title=NULL))


# library(ggpubr)
# 
# ggarrange(p_uspec, p_smcm)

#illustrations_processes <- list(smcm = p_smcm, uspec = p_uspec)

#ggsave(illustrations_processes$uspec, file = "./manualFigures/illustrationUspec.pdf", width=10, height=3.25)
ggsave(p_smcm, file = path_illustration_dur_smcm, width=10, height=5)
