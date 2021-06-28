format_CrI <- function(vals, ci_width=90, digits=2) {
  fmt <- paste0("%.0", digits, "f") 
  fmt_expr <- paste0("[",fmt, "; ", fmt, "]")
  sprintf(fmt_expr, vals[paste0("lower",ci_width)], vals[paste0("upper",ci_width)])
}

format_post_prob <- function(vals, p_below = T, digits = 3)
{
  if(p_below) {
    posterior_prob <- (vals[["p_below_zero"]])
    relation_theta_val <- "<"
  } else {
    posterior_prob <- (1-vals[["p_below_zero"]])
    relation_theta_val <- ">"
  }
  
  min_value <- 1/10^digits
  if (posterior_prob < min_value) { 
    relation_postprob_val <- "<" 
    posterior_prob <- min_value
  }
  else if (posterior_prob > 1-min_value) { 
    relation_postprob_val <- ">" 
    posterior_prob <- 1-min_value
  }
  else {
    relation_postprob_val <- "="
  }
  
  fmt_expr <- paste0("P(\\theta ", relation_theta_val, " 0) ", relation_postprob_val, " %.0", digits, "f")
  p_direction <- sprintf(fmt_expr, posterior_prob)
  
  p_direction
}

format_CrI_and_post_prob <- function(vals, ci_width=90, ci_digits=2, pp_below=T, pp_digits=3) {
  CrI <- format_CrI(vals, ci_width=ci_width, digits=ci_digits)
  pp <- format_post_prob(vals, p_below=pp_below, digits = pp_digits)
  sprintf("$CrI = %s$, $%s$", CrI, pp)
}