
library(tidyverse)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggpubr)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

gg_color_hue(3)
# "#F8766D" "#00BA38" "#619CFF"


# Note: Using Gamma distributions:
# According to http://en.wikipedia.org/wiki/Gamma_distribution#Summation, the sum of several gamma distributions with the 
# same scale equals a gamma distribution with the same scale, but the shape parameters are added


time <- seq(325,1100,1)

par_scale = 100

legend_coord = c(.4, .4)
ylab_label = expression(Pr(processing~complete)) #  <= t
xlab_label = "time (ms)"



# Underspecification plot 1

conditions <- list(
  high = list(shift = 450, shape = 1.35),
  low = list(shift = 440, shape = 1.3),
  ambiguous = list(shift = 390, shape = 1.2)
)
cdfs_uspec1 <-
plyr::ldply(conditions, function(par) {
  data.frame(time = time, value = pgamma(time-par$shift, shape = par$shape, scale = par_scale) )
})

(prediction_cdf_uspec1 <-
  cdfs_uspec1 %>%
  ggplot(aes(x=time, y=value, color=.id)) + geom_line(size=1) +
    ylab(ylab_label) + xlab(xlab_label) + 
    theme_bw() + theme(legend.position="top", strip.background =element_rect(fill="white")) + 
    scale_x_continuous(breaks=NULL) + xlab("time") + 
    labs(color = NULL)
)


# Underspecification plot 2

conditions <- list(
  high = list(shift = 450, shape = 1.35),
  low = list(shift = 440, shape = 1.3),
  ambiguous = list(shift = 440, shape = 1)
)
cdfs_uspec2 <-
  plyr::ldply(conditions, function(par) {
    data.frame(time = time, value = pgamma(time-par$shift, shape = par$shape, scale = par_scale) )
  })

(prediction_cdf_uspec2 <-
    cdfs_uspec2 %>%
    ggplot(aes(x=time, y=value, color=.id)) + geom_line(size=1) +
    ylab(ylab_label) + xlab(xlab_label) + 
    theme_bw() + theme(legend.position="top", strip.background =element_rect(fill="white")) + 
    scale_x_continuous(breaks=NULL) + xlab("time") + 
    labs(color = NULL)
)

prediction_cdfs_uspec <- ggarrange(prediction_cdf_uspec1, prediction_cdf_uspec2, labels = c("A", "B"))



# SMCM plot
conditions <- list(
  high = list(shift = 460, shape = 1.35),
  low = list(shift = 440, shape = 1.3)
)

cdf_smcm_low <- with(conditions$low, data.frame(time = time, value = pgamma(time-shift, shape = shape, scale = par_scale), condition = "low" ))
cdf_smcm_high <- with(conditions$high, data.frame(time = time, value = pgamma(time-shift, shape = shape, scale = par_scale), condition = "high" ))
cdf_smcm_ambiguous <- data.frame(time = time, value = 1- (1-cdf_smcm_low$value) * (1-cdf_smcm_high$value), condition = "ambiguous" )
cdfs_smcm <- cdf_smcm_low %>% rbind(cdf_smcm_high) %>% rbind(cdf_smcm_ambiguous) 

(prediction_cdf_smcm <-
    cdfs_smcm %>%
    ggplot(aes(x=time, y=value, color=condition)) + geom_line(size=1) +
    ylab(ylab_label) + xlab(xlab_label) + 
    theme_bw() + theme(legend.position="top", strip.background =element_rect(fill="white")) + 
    scale_x_continuous(breaks=NULL) + xlab("time") + 
    labs(color = NULL)
)



# Limited-capacity race plot
conditions <- list(
  high = list(shift = 410, shape = 1.2),
  low = list(shift = 390, shape = 1.15)
)

cdf_lcrace_low2 <- with(conditions$low, data.frame(time = time, value = pgamma(time-shift, shape = shape, scale = par_scale), condition = "low"))
cdf_lcrace_high2 <- with(conditions$high, data.frame(time = time, value = pgamma(time-shift, shape = shape, scale = par_scale), condition = "high"))
cdfs_lcrace <- cdfs_smcm
cdfs_lcrace$condition %<>% dplyr::recode("low"="low 2", "high"="high 2")
cdfs_lcrace %<>% rbind(cdf_lcrace_low2) %>% rbind(cdf_lcrace_high2) 

(prediction_cdf_lcrace <-
    cdfs_lcrace %>%
    ggplot(aes(x=time, y=value, color=condition, linetype = condition
               )) + geom_line(size=1) +
    ylab(ylab_label) + xlab(xlab_label) + 
    theme_bw() + theme(legend.position="top", strip.background =element_rect(fill="white")) + 
    scale_x_continuous(breaks=NULL) + xlab("time") + 
    labs(color = NULL)
)

prediction_cdf_lcrace <-
prediction_cdf_lcrace +
  scale_color_manual(name = NULL,
                     breaks = c("ambiguous", "high", "low", "high 2", "low 2"),
                     values = c("#F8766D", "#00BA38", "#619CFF", "#00BA38", "#619CFF"),
                     labels = c("ambiguous", "high", "low", "latent high", "latent low") ) +
  scale_linetype_manual(name = NULL,
                        breaks = c("ambiguous", "high", "low", "high 2", "low 2"),
                     values = c("solid", "solid", "solid", "dotted", "dotted"),
                     labels = c("ambiguous", "high", "low", "latent high", "latent low") ) 



# path_illustration_prediction_cdf_uspec = "../figures/illustrationPredictionUspec.pdf"
# path_illustration_prediction_cdf_smcm = "../figures/illustrationPredictionSMCM.pdf"
# path_illustration_prediction_cdf_lcrace = "../figures/illustrationPredictionLCRace.pdf"


z = .8
ggsave(prediction_cdfs_uspec, file = path_illustration_prediction_cdf_uspec, height = z*3.5, width = z*7.26, device = cairo_pdf)
ggsave(prediction_cdf_smcm, file = path_illustration_prediction_cdf_smcm, height = z*3.5, width = z*7.26, device = cairo_pdf)
ggsave(prediction_cdf_lcrace, file = path_illustration_prediction_cdf_lcrace, height = z*3.5, width = z*7.26, device = cairo_pdf)


