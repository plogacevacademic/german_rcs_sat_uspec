library(ggplot2)

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
