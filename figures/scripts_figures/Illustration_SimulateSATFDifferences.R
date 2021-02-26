
library(satf)
library(ggplot2)

time <- seq(0,4.5,by=.001)
satf.sim.base <- data.frame(time, condition="a", d=SATF(time, lambda=2.8, beta=.8, delta=0.3))
satf.sim1 <- data.frame(time, condition="b", d=SATF(time, lambda=2.3, beta=.8, delta=0.3))
satf.sim2 <- data.frame(time, condition="b", d=SATF(time, lambda=2.8, beta=1, delta=0.3))
satf.sim3 <- data.frame(time, condition="b", d=SATF(time, lambda=2.8, beta=.8, delta=0.8))

width=10; height=3;
xx1 <- rbind(satf.sim.base,satf.sim1)
xx2 <- rbind(satf.sim.base,satf.sim2)
xx3 <- rbind(satf.sim.base,satf.sim3)
xx <- rbind(cbind(xx1,diff="Difference in\nasymptote"),
            cbind(xx3,diff="Difference in\nintercept"), 
            cbind(xx2,diff="Difference in\nrate")
            )

ggplot(xx, aes(x=time, y=d, linetype=condition))+geom_line()+ xlab('time (sec)')+ylab("d'")+facet_grid(.~ diff)+ theme_bw()
ggsave(file="./figures/ExampleSATDiff.pdf", width=width, height=height)