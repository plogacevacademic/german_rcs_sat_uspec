library(ggplot2)

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
