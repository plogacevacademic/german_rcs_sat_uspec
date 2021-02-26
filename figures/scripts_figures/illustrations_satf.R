

library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(metR)



geom_double_arrow <- function(p, x1, y1, x2, y2) { 
  p +
    geom_segment(aes(x=x1, xend=x2, y=y1, yend=y2), arrow = arrow(type = "closed", length = unit(.25,"cm")), size=.1, colour = annotation_color) +
    geom_segment(aes(x=x2, xend=x1, y=y2, yend=y1), arrow = arrow(type = "closed", length = unit(.25,"cm")), size=.1, colour = annotation_color)
}

dprime <- function(t, lambda=.5, beta=1.5, delta=1.5) ifelse( t >= delta, lambda * (1-exp(-(t-delta)/beta)), 0)

y1 = 0.075
y2 = .5*(1-exp(-1))
annotation_color = "darkgrey"

x <- seq(0,10, 1)
p <- ggplot(data.frame(x=x, y= dprime(x) ), aes(x, y)) + stat_function(fun = dprime)

p <-
  p %>% geom_double_arrow(x1=0, x2=1.5, y1=y1, y2=y1) + 
  geom_label(aes(x=0.75, y=y1, label = "δ"), colour = annotation_color,  position= position_dodge(width=1))

#p + geom_label(aes(x=2.25, y=y1 ), label = expression(beta^-1), parse = T, colour = "black",  position= position_dodge(width=1))

p <- 
  p %>% geom_double_arrow(x1=1.5, x2=1.5+1.5, y1=y1, y2=y1) +
  geom_label(aes(x=2.25, y=y1), label = expression(beta^-1), parse = T, colour = annotation_color,  position= position_dodge(width=1))

p <-
  p + geom_segment(aes(x=1.5, xend=1.5, y=0, yend=y1), color=annotation_color, linetype = "dotted", size=.1) + 
  geom_segment(aes(x=3, xend=3, y=y2, yend=0), color=annotation_color, linetype = "dotted", size=.1) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 10, 1.5))

p <-
  p + geom_segment(aes(x=0, xend=0, y=0, yend=y1), color=annotation_color, linetype = "dotted", size=.1)

p <-
  p + geom_segment(aes(x=-Inf, xend=10, y=.5, yend=.5), color=annotation_color, linetype = "dotted", size=.1) +
  geom_segment(aes(x=-Inf, xend=3, y=y2, yend=y2 ), color=annotation_color, linetype = "dotted", size=.1) 
# +
#   geom_segment(aes(x=-Inf, xend=1.5, y=0, yend=0 ), color="black", linetype = "dotted", size=.1)

p <-
  p + geom_label(aes(x=1, y=y2, label = "λ·0.63"), colour = annotation_color,  position= position_dodge(width=1)) +
  geom_label(aes(x=1, y=.5, label = "λ"), colour = annotation_color,  position= position_dodge(width=1))

p <- p + scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL)
p <- p + theme(axis.ticks = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p <- p + xlab("time") + ylab("accuracy") # "accuracy (d')"
p <- p + geom_point()

#p

z = .8
ggsave(p, file = "./manualFigures/illustrationSATF.pdf", height = z*3.5, width = z*7.26, device = cairo_pdf)

