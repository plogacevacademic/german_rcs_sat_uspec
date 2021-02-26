
stopifnot(class(time) == "numeric")

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


# my.plot <- function(data, cumulative=FALSE) {
#   p1 <- ggplot(data=data, aes(x=time/1000))
#   linetypes <- c("dashed","dotted","solid")
#   p1 <- p1+geom_line(aes(y=low, linetype="low"))
#   p1 <- p1+geom_line(aes(y=high, linetype="high"))
#   p1 <- p1+geom_line(aes(y=amb, linetype="ambiguous"))
#   labels <- c("low", "high", "ambiguous")
#   p1 <- p1+scale_linetype_manual("attachment", 
#                                  breaks=labels, 
#                                  values=linetypes)
#   p1 <- p1 + xlab("time (sec)")
#   if(cumulative) {
#     p1 <- p1 + ylab(expression(Pr(success <= t)))
#     # TODO: Pr(success <= t) is actually inaccurate, because the success is not smaller than t. 
#     #The time of a particular success is smaller than t.   Look up the proper notation for this.
#   }
#   p1
# }

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

uspec.predictions <- function(p.success.unamb, p.success.amb, time, model) 
{
  pdf.low <- function(time) pdf.AC1(time, p.success.unamb)
  cdf.low <- function(time) cdf.AC1(time, p.success.unamb)
  m.sd.low <- gamma.mean.sd(shiftA+shiftC1, shapeA+shapeC1, scale.all)
  
  pdf.high <- function(time) pdf.B(time, p.success.unamb)
  cdf.high <- function(time) cdf.B(time, p.success.unamb)
  m.sd.high <- gamma.mean.sd(shiftB, shapeB, scale.all)
  
  pdf.amb <- function(time) pdf.A(time, p.success.amb)
  cdf.amb <- function(time) cdf.A(time, p.success.amb)
  m.sd.amb <- gamma.mean.sd(shiftA, shapeA, scale.all)

  tmpdata.pdf <- data.frame(time=time, low=pdf.low(time), high=pdf.high(time), amb=pdf.amb(time), model=model )
  tmpdata.cdf <- data.frame(time=time, low=cdf.low(time), high=cdf.high(time), amb=cdf.amb(time), model=model )
  
  # save means and standard deviations
  m.sd <- list(m.low=m.sd.low[['m']], sd.low=m.sd.low[['sd']], shift.low=m.sd.low[['shift']], 
               m.high=m.sd.high[['m']], sd.high=m.sd.high[['sd']], shift.high=m.sd.high[['shift']], 
               m.amb=m.sd.amb[['m']], sd.amb=m.sd.amb[['m']], shift.amb=m.sd.amb[['shift']]  )
  
  list(pdf=tmpdata.pdf, cdf=tmpdata.cdf, m.sd=m.sd,
       cdf.amb=cdf.amb, cdf.low=cdf.low, cdf.high=cdf.high)
}

smcm.predictions <- function(p.success, time, model) 
{
  rand.low <- function(n) { res <- rand.AC1(n); res[sample(1:n, size=n*(1-p.success))] <- Inf; res }
  pdf.low <- function(time) pdf.AC1(time, p.success)
  cdf.low <- function(time) cdf.AC1(time, p.success)
  m.sd.low <- gamma.mean.sd(shiftA+shiftC1, shapeA+shapeC1, scale.all)
  
  rand.high <- function(n) { res <- rand.B(n); res[sample(1:n, size=n*(1-p.success))] <- Inf; res }
  pdf.high <- function(time) pdf.B(time, p.success)
  cdf.high <- function(time) cdf.B(time, p.success)
  m.sd.high <- gamma.mean.sd(shiftB, shapeB, scale.all)
  
  sim.n <- 10^6
  RT.race <- pmin(rand.low(sim.n), rand.high(sim.n))
  pdf.amb <- function(time) splinefun(density(RT.race, from=min(time), to=max(time)))(time)
  cdf.amb <- function(time) 1-(1-cdf.low(time))*(1-cdf.high(time))
  
  tmpdata.pdf <- data.frame(time=time, low=pdf.low(time), high=pdf.high(time), amb=pdf.amb(time), model=model )
  tmpdata.cdf <- data.frame(time=time, low=cdf.low(time), high=cdf.high(time), amb=cdf.amb(time), model=model )
  
  # save means and standard deviations
  m.sd <- list(m.low=m.sd.low[['m']], sd.low=m.sd.low[['sd']], shift.low=m.sd.low[['shift']], 
               m.high=m.sd.high[['m']], sd.high=m.sd.high[['sd']], shift.high=m.sd.high[['shift']], 
               m.amb=mean(RT.race[RT.race!=Inf]), sd.amb=sd(RT.race[RT.race!=Inf]), shift.amb=min(m.sd.low[['shift']], m.sd.high[['shift']]) )
  
  list(pdf=tmpdata.pdf, cdf=tmpdata.cdf, m.sd=m.sd,
       cdf.low=cdf.low, cdf.high=cdf.high, cdf.amb=cdf.amb)
}


#############################################
# ****** simulate underspecification ****** #
#############################################

labelUSPEC1 <- "USPEC"
labelUSPEC2 <- "F-USPEC"

underspec.p.success <- .8
underspec.p.successC1 <- .8
uspec1 <- uspec.predictions(p.success.unamb=1, p.success.amb=1, time=time, model=labelUSPEC1) 
uspec2 <- uspec.predictions(p.success.unamb=.8, p.success.amb=1-.2^2, time=time, model=labelUSPEC2) 

uspec1.rate <- find.asym.props(uspec1, "USPEC1", labelUSPEC1)
uspec2.rate <- find.asym.props(uspec2, "USPEC2", labelUSPEC2)
uspec.rate <- rbind(uspec1.rate, uspec2.rate)

uspec1$m.sd <- round( unlist(uspec1$m.sd) )
uspec2$m.sd <- round( unlist(uspec2$m.sd) )
stopifnot(all(uspec1$m.sd == uspec2$m.sd))

conds <- c('amb','low','high')
shifts <- paste('shift',conds,sep=".")
uspec.rate <- round(with(uspec.rate, tapply(t, list(condition,mnum), mean))*1000)
uspec.rate <- uspec.rate[conds, ] - c(uspec1$m.sd[shifts], uspec2$m.sd[shifts])
stopifnot(all(uspec.rate[,'USPEC1'] == uspec.rate[,'USPEC2']))



#######################################
# ********** simulate SMCM ********** #
#######################################

labelSMCM1 <- "SMCM"
labelSMCM2 <- "F-SMCM"

smcm2.p.success <- .8
smcm1 <- smcm.predictions(p.success=1, time=time, model=labelSMCM1) 
smcm2 <- smcm.predictions(p.success=smcm2.p.success, time=time, model=labelSMCM2) 

smcm1.rate <- find.asym.props(smcm1, "SMCM1", labelSMCM1)
smcm2.rate <- find.asym.props(smcm2, "SMCM2", labelSMCM2)
smcm.rate <- rbind(smcm1.rate, smcm2.rate)

smcm1$m.sd <- round( unlist(smcm1$m.sd) )
smcm2$m.sd <- round( unlist(smcm2$m.sd) )

stopifnot(smcm1$m.sd[['m.low']] == smcm2$m.sd[['m.low']])
stopifnot(smcm1$m.sd[['sd.low']] == smcm2$m.sd[['sd.low']])
stopifnot(smcm1$m.sd[['m.high']] == smcm2$m.sd[['m.high']])
stopifnot(smcm1$m.sd[['sd.high']] == smcm2$m.sd[['sd.high']])

smcm2.p.two.failures <- (1-smcm2.p.success)^2
smcm2.p.no.failures <- smcm2.p.success^2
smcm2.p.one.failure <- 1-smcm2.p.two.failures-smcm2.p.no.failures

conds <- c('amb','low','high')
shifts <- paste('shift',conds,sep=".")
smcm.rate <- round(with(smcm.rate, tapply(t, list(condition,mnum), mean))*1000)
smcm.rate <- smcm.rate[conds, ] - c(smcm1$m.sd[shifts], smcm2$m.sd[shifts])
stopifnot(smcm.rate[c('low','high'),'SMCM1'] == smcm.rate[c('low','high'),'SMCM2'])


