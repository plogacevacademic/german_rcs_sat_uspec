library(ggplot2)

urm.predictions <- function(p.success, time, model) 
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

labelURM1 <- "a) URM"
labelURM2 <- "a) F-URM"

urm2.p.success <- .8
urm1 <- urm.predictions(p.success=1, time=time, model=labelURM1) 
urm2 <- urm.predictions(p.success=urm2.p.success, time=time, model=labelURM2) 

urm1.rate <- find.asym.props(urm1, "URM1", labelURM1)
urm2.rate <- find.asym.props(urm2, "URM2", labelURM2)
urm.rate <- rbind(urm1.rate, urm2.rate)

urm1$m.sd <- round( unlist(urm1$m.sd) )
urm2$m.sd <- round( unlist(urm2$m.sd) )

stopifnot(urm1$m.sd[['m.low']] == urm2$m.sd[['m.low']])
stopifnot(urm1$m.sd[['sd.low']] == urm2$m.sd[['sd.low']])
stopifnot(urm1$m.sd[['m.high']] == urm2$m.sd[['m.high']])
stopifnot(urm1$m.sd[['sd.high']] == urm2$m.sd[['sd.high']])

urm2.p.two.failures <- (1-urm2.p.success)^2
urm2.p.no.failures <- urm2.p.success^2
urm2.p.one.failure <- 1-urm2.p.two.failures-urm2.p.no.failures

conds <- c('amb','low','high')
shifts <- paste('shift',conds,sep=".")
urm.rate <- round(with(urm.rate, tapply(t, list(condition,mnum), mean))*1000)
urm.rate <- urm.rate[conds, ] - c(urm1$m.sd[shifts], urm2$m.sd[shifts])
stopifnot(urm.rate[c('low','high'),'URM1'] == urm.rate[c('low','high'),'URM2'])
