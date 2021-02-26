#### DISTRIBUTIONS DEFINITIONS

scale.all <- 100;
shiftA <- 390; shapeA <- 1.2;
shiftB <- 450; shapeB <- 1.35;
shiftC1 <- 50; shapeC1 <- 0.1;
shiftC2 <- shiftC1; shapeC2 <- shapeC1;
shiftC3 <- 40; shapeC3 <- shapeC1;
pdf.A <- function(x, times=1) {dgamma(x-shiftA, shape=shapeA, scale=scale.all)*times}
pdf.B <- function(x, times=1) {dgamma(x-shiftB, shape=shapeB, scale=scale.all)*times}
pdf.C1 <- function(x, times=1) {dgamma(x-shiftC1, shape=shapeC1, scale=scale.all)*times}
pdf.C2 <- function(x, times=1) {dgamma(x-shiftC2, shape=shapeC2, scale=scale.all)*times}
pdf.C3 <- function(x, times=1) {dgamma(x-shiftC3, shape=shapeC3, scale=scale.all)*times}
# According to http://en.wikipedia.org/wiki/Gamma_distribution#Summation, the sum of several gamma distributions with the 
# same scale equals a gamma distribution with the same scale, but the shape parameters are added
pdf.AC1 <- function(x, times=1) {dgamma(x-shiftA-shiftC1, shape=shapeA+shapeC1, scale=scale.all)*times}
pdf.AC1C2 <- function(x, times=1) {dgamma(x-shiftA-shiftC1-shiftC2, shape=shapeA+shapeC1+shapeC2, scale=scale.all)*times}
pdf.AC1C2C3 <- function(x, times=1) {dgamma(x-shiftA-shiftC1-shiftC2-shiftC3, shape=shapeA+shapeC1+shapeC2+shapeC3, scale=scale.all)*times}

rand.A <- function(x, times=1) {rgamma(x, shape=shapeA, scale=scale.all)+shiftA}
rand.B <- function(x, times=1) {rgamma(x, shape=shapeB, scale=scale.all)+shiftB}
rand.C1 <- function(x, times=1) {rgamma(x, shape=shapeC1, scale=scale.all)+shiftC1}
rand.C2 <- function(x, times=1) {rgamma(x, shape=shapeC2, scale=scale.all)+shiftC2}
rand.C3 <- function(x, times=1) {rgamma(x, shape=shapeC3, scale=scale.all)+shiftC3}
rand.AC1 <- function(x, times=1) {rgamma(x, shape=shapeA+shapeC1, scale=scale.all)+shiftA+shiftC1}
rand.AC1C2 <- function(x, times=1) {rgamma(x, shape=shapeA+shapeC1+shapeC2, scale=scale.all)+shiftA+shiftC1+shiftC2}
rand.AC1C2C3 <- function(x, times=1) {rgamma(x, shape=shapeA+shapeC1+shapeC2+shapeC3, scale=scale.all)+shiftA+shiftC1+shiftC2+shiftC3}

cdf.A <- function(x, times=1) {pgamma(x-shiftA, shape=shapeA, scale=scale.all)*times}
cdf.B <- function(x, times=1) {pgamma(x-shiftB, shape=shapeB, scale=scale.all)*times}
cdf.C1 <- function(x, times=1) {pgamma(x-shiftC1, shape=shapeC1, scale=scale.all)*times}
cdf.C2 <- function(x, times=1) {pgamma(x-shiftC2, shape=shapeC2, scale=scale.all)*times}
cdf.C3 <- function(x, times=1) {pgamma(x-shiftC3, shape=shapeC3, scale=scale.all)*times}
cdf.AC1 <- function(x, times=1) {pgamma(x-shiftA-shiftC1, shape=shapeA+shapeC1, scale=scale.all)*times}
cdf.AC1C2 <- function(x, times=1) {pgamma(x-shiftA-shiftC1-shiftC2, shape=shapeA+shapeC1+shapeC2, scale=scale.all)*times}
cdf.AC1C2C3 <- function(x, times=1) {pgamma(x-shiftA-shiftC1-shiftC2-shiftC3, shape=shapeA+shapeC1+shapeC2+shapeC3, scale=scale.all)*times}


my.plot <- function(data, cumulative=FALSE) {
  p1 <- ggplot(data=data, aes(x=time/1000))
  linetypes <- c("dashed","dotted","solid")
  p1 <- p1+geom_line(aes(y=low, linetype="low"))
  p1 <- p1+geom_line(aes(y=high, linetype="high"))
  p1 <- p1+geom_line(aes(y=amb, linetype="ambiguous"))
  labels <- c("low", "high", "ambiguous")
  p1 <- p1+scale_linetype_manual("attachment", 
                                 breaks=labels, 
                                 values=linetypes)
  p1 <- p1 + xlab("time (sec)")
  if(cumulative) {
    p1 <- p1 + ylab(expression(Pr(success <= t)))
    # TODO: Pr(success <= t) is actually inaccurate, because the success is not smaller than t. 
    #The time of a particular success is smaller than t.   Look up the proper notation for this.
  }
  p1
}

# plot little circles in the models predictions denoting that 75% of the asymptote have been reached
rate.proportion.to.plot <- 1-exp(-1)

eval.distance <- function(t, fn, asym) (fn(t*1000)-asym)^2 
find.asym.prop <- function(fn, prop=rate.proportion.to.plot) {
  t <- optimize(eval.distance, interval=c(.1,5), fn=fn, asym=prop*fn(10^9))$minimum
  data.frame(t=t, p=prop*fn(10^9))
}
find.asym.props <- function(cdfs, mnum, model) { d <- rbind(find.asym.prop(fn=cdfs$cdf.amb),
                                                            find.asym.prop(fn=cdfs$cdf.low),
                                                            find.asym.prop(fn=cdfs$cdf.high))
                                                 d$condition <- c("amb","low","high")
                                                 d$model <- model
                                                 d$mnum <- mnum
                                                 d
}
# for gamma distribution: mean = shape*scale, variance = shape*scale^2
gamma.mean.sd <- function(shift, shape, scale) c(m=shift+shape*scale, sd=sqrt(shape*scale^2), shift=shift)
m.sd.AC1 <- gamma.mean.sd(shift=shiftA+shiftC1, shape=(shapeA+shapeC1), scale=scale.all)
m.sd.AC1C2 <- gamma.mean.sd(shift=shiftA+shiftC1+shiftC2, shape=(shapeA+shapeC1+shapeC2), scale=scale.all)

# my.plot <- function(data, plot.Cs=T) {
#   p1 <- ggplot(data=data, aes(x=time))
#   linetypes <- c("dotted","longdash")
#   p1 <- p1+geom_line(aes(y=C, linetype=label.C))
#   p1 <- p1+geom_line(aes(y=A, linetype=label.A))
#   p1 <- p1+scale_linetype_manual(breaks=c(label.C, label.A), values=linetypes)
#   p1
# }
# 
