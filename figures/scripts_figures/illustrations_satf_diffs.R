

library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(metR)


satf <- function(time, asymptote, invrate, intercept) {
  ifelse( time<= intercept, 0, asymptote*(1-exp(-(time-intercept)/invrate)) )
}

time <- seq(0,4.5,by=.001)
satf_reference <- data.frame(time, condition="a", d=satf(time, asymptote=2.8, invrate=.8, intercept=0.3))
satf_a <- data.frame(time, condition="b", d=satf(time, asymptote=2.3, invrate=0.8, intercept=0.3))
satf_b <- data.frame(time, condition="b", d=satf(time, asymptote=2.8, invrate=1.2, intercept=0.3))
satf_c <- data.frame(time, condition="b", d=satf(time, asymptote=2.8, invrate=0.8, intercept=0.8))

df <- rbind(rbind(satf_reference, satf_a) %>% cbind(label="asymptote difference"),
            rbind(satf_reference, satf_b) %>% cbind(label="rate difference"), 
            rbind(satf_reference, satf_c) %>% cbind(label="intercept difference")
)
df$label %<>% factor(levels = c("asymptote difference", "rate difference", "intercept difference"))

p <- ggplot(df, aes(x=time, y=d, linetype=condition))+geom_line()+facet_grid(.~ label)+ theme_bw() + scale_y_continuous(limits=c(0,3))

p <- p + scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL)
p <- p + theme(axis.ticks = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white", color="black"))
p <- p + xlab("time") + ylab("accuracy")


z = .8
ggsave(p, file = path_illustration_satf, height = z*3.5, width = z*7.26, device = cairo_pdf)

