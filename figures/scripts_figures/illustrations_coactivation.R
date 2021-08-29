
library(tidyverse)
library(magrittr)
library(ggplot2)


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

gg_color_hue(3)

# high (blue)  #00bfc4
# low (green) #7cae00

color_high <- "#00bfc4"
color_low  <- "#7cae00"
color_amb  <- "#F8766D"
plot_x_limits = c(0, 3.5)

{
slope_high <- 1/1.5
slope_low <- 1/2.5
slope_amb <- slope_high + slope_low

inv_slope_sd_high = .15
inv_slope_sd_low  = .1
inv_slope_sd_amb  = sqrt(inv_slope_sd_high^2 + inv_slope_sd_low^2)

df_high <- data.frame(x = c(0, 1/slope_high), y = c(0, 1))
df_low  <- data.frame(x = c(0, 1/slope_low),  y = c(0, 1))
df_amb  <- data.frame(x = c(0, 1/slope_amb),  y = c(0, 1))

p <-
ggplot() + geom_line(data = df_high, aes(x, y), color = color_high) + 
            geom_line(data = df_low, aes(x, y), color = color_low) + 
            geom_line(data = df_amb, aes(x, y), color = color_amb)

p

df_band_amb <- data.frame(group = c(1,1,1), polygon.x = c(0, 1/(slope_amb+inv_slope_sd_amb), 1/(slope_amb-inv_slope_sd_amb) ), polygon.y = c(0,1,1))
df_band_high <- data.frame(group = c(1,1,1), polygon.x = c(0, 1/(slope_high+inv_slope_sd_high), 1/(slope_high-inv_slope_sd_high) ), polygon.y = c(0,1,1))
df_band_low <- data.frame(group = c(1,1,1), polygon.x = c(0, 1/(slope_low+inv_slope_sd_low), 1/(slope_low-inv_slope_sd_low) ), polygon.y = c(0,1,1))

p <-
p + geom_polygon( data = df_band_low, aes(x=polygon.x, y=polygon.y, group=group), alpha = .2, fill = color_low) + 
    geom_polygon( data = df_band_high, aes(x=polygon.x, y=polygon.y, group=group), alpha = .2, fill = color_high) + 
    geom_polygon( data = df_band_amb, aes(x=polygon.x, y=polygon.y, group=group), alpha = .2, fill = color_amb)
p
}

p <- p + theme_bw() + scale_x_continuous(breaks = NULL, limits = plot_x_limits) + scale_y_continuous(breaks = NULL) + 
          ylab("evidence") + xlab("time")
p <- p + geom_line(data=data.frame(x=c(0, Inf), y=c(1,1)), aes(x, y), linetype = "dashed")
p1 <- p + geom_label(aes(x=.25, y=0.9, label = "threshold" #label = "decision\nthreshold"
                         ), colour = "black",  position= position_dodge(width=1))

# ********************************************************************************************************************************************** #

recinormal_bounds <- function(x, mean, sd, bleft, bright) {
  ifelse(x >= bleft & x <= bright, 1-pnorm(1/x_grid, mean = mean, sd = sd), 0)
}

x_grid <- seq(plot_x_limits[1], plot_x_limits[2], .005)
df_ecdf_low  <- data.frame(x = x_grid, y = recinormal_bounds(x_grid, slope_low, 1*inv_slope_sd_low, df_band_low$polygon.x[2], df_band_low$polygon.x[3]) )
df_ecdf_high <- data.frame(x = x_grid, y = recinormal_bounds(x_grid, slope_high, 1*inv_slope_sd_high, df_band_high$polygon.x[2], df_band_high$polygon.x[3]) )
df_ecdf_amb  <- data.frame(x = x_grid, y = recinormal_bounds(x_grid, slope_amb, 1*inv_slope_sd_amb, df_band_amb$polygon.x[2], df_band_amb$polygon.x[3]) )

p2 <- 
  ggplot() + 
  stat_function(fun = function(x) 1-pnorm(1/x, mean = slope_low, sd = 1*inv_slope_sd_low ), color = color_low ) + 
  geom_area(data = df_ecdf_low, aes(x=x, y=y), fill = color_low, alpha = .2) +
  stat_function(fun = function(x) 1-pnorm(1/x, mean = slope_high, sd = 1*inv_slope_sd_high ), color = color_high ) + 
  geom_area(data = df_ecdf_high, aes(x=x, y=y), fill = color_high, alpha = .2) +
  stat_function(fun = function(x) 1-pnorm(1/x, mean = slope_amb, sd = 1*inv_slope_sd_amb ), color = color_amb ) + 
  geom_area(data = df_ecdf_amb, aes(x=x, y=y), fill = color_amb, alpha = .2)

p2 <- p2 + scale_x_continuous(breaks = NULL, limits = plot_x_limits) + scale_y_continuous(breaks = NULL) + 
           ylab("Pr(processing complete)") + xlab("time") + theme_bw()

library(ggpubr)

p <- ggarrange(p2 , p1, nrow = 2)


z = .8
ggsave(p, file = path_illustration_prediction_cdf_coactive, height = z*6, width = z*7.26, device = cairo_pdf)

