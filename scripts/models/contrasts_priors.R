
###################################
### Set priors and satf formula ###
###################################
priors <- list()

# Constraints on choosing priors:
# # d': 
# - The intercept *must* be located between 0 and the last response, *much* more likely
#   in the first of half of the time windows.
# - The asmptote must be >1, and very unlikely to be above ~5 (moreover, the practical
#   difference between an asymptote of ~5 and a much larger asymptote is negligible). 
#   So let's shrink aymptotes to the [0; 5] interval.
# - The invrate (1/rate) *must* be strictly positive, and smaller than length of the
#   timewindow we operate on, and most probably in the [500ms; 1500ms] interval. 
#   (Reminder: The inverse of the rate corresponds to the amount of time participants
#   require to reach ~63% of the asymptote, starting from the (x-)intercept.)
# - ...
#
# # bias:
# - If I ignore what I already know about this dataset, we can make very few assumptions
#   about the left and right asymptotes, other the fact that outside the interval of around
#   [-5;5], they don't make much sense.
# - The invrate must be strictly positive. (The left and right asymptote switch places when
#   it's negative.)
# - Assuming that the bias doesn't change before the d' intercept, the midpoint seems
#   likely located between the d' intercept and <d' intercept + invrate>.

# note:
# 1-pnorm(-2, mean=0, sd=1)*2    # ~95% between [-2*sd; +2*sd]
# 1-pnorm(-1.65, mean=0, sd=1)*2 # ~90% between [-1.65*sd; +1.65*sd]
# 1-pnorm(-1.3, mean=0, sd=1)*2  # ~80% between [-1.3*sd; +1.3*sd]


priors$dprime <- c(

      # transformation: exp(x)
      # * intercept to be kept roughly in [1; 4], with ~95% prob
      #   -> (0.5 * c(-2,0,2) ) %>% add(0) %>% exp() --> approximately in [0; 4]
      # * other coefficients should reflect differences of no more than factor 2 (or 0.5) with 95% prob
      #   -> ((.5*c(-2/2,0,2/2) ) %>% add(0) %>% exp()) %>% {.[3]/.[1]}
      set_prior(nlpar = "dprimeAsymptote", prior = "normal(0, .5)", class = "b", coef = "Intercept"),
      set_prior(nlpar = "dprimeAsymptote", prior = "normal(0, .35)", class = "b"),
      #
      # transformation: exp(x)
      # * intercept to be kept roughly in [0.5; 1.5], with ~95% prob
      #   -> (0.35 * c(-2,0,2) ) %>% add(0.0) %>% exp()
      # * other coefficients should reflect differences of no more than factor 2 (or 0.5) with 99% prob
      #   -> ((0.35 * c(-2/2,0,2/2) ) %>% add(0.0) %>% exp()) %>% {.[3]/.[1]}
      set_prior(nlpar = "dprimeInvrate", prior = "normal(0, .35)", class = "b", coef = "Intercept"),
      set_prior(nlpar = "dprimeInvrate", prior = "normal(0, .35)", class = "b"),
      #
      # transformation: exp(x - 1)
      # * intercept to be kept roughly in [0.1; 1.2], with ~95% prob
      #   -> (0.6 * c(-2,0,2) ) %>% add(-1) %>% exp()
      # * other coefficients should reflect differences of no more than factor 2 (or 0.5) with 99% prob
      #   -> ((0.35 * c(-2/2,0,2/2) ) %>% add(-1) %>% exp()) %>% {.[3]/.[1]}
      set_prior(nlpar = "dprimeIntercept", prior = "normal(0, .6)", class = "b", coef = "Intercept"),
      set_prior(nlpar = "dprimeIntercept", prior = "normal(0, .35)", class = "b")
    )


priors$dprime_separate <- c(
  
  # transformation: exp(x)
  # * intercept to be kept roughly in [1; 4], with ~95% prob
  #   -> (0.5 * c(-2,0,2) ) %>% add(0) %>% exp() --> approximately in [0; 4]
  # * other coefficients should reflect differences of no more than factor 2 (or 0.5) with 95% prob
  #   -> ((.5*c(-2/2,0,2/2) ) %>% add(0) %>% exp()) %>% {.[3]/.[1]}
  set_prior(nlpar = "dprimeAsymptote", prior = "normal(0, .5)", class = "b"),
  #
  # transformation: exp(x)
  # * intercept to be kept roughly in [0.5; 1.5], with ~95% prob
  #   -> (0.35 * c(-2,0,2) ) %>% add(0.0) %>% exp()
  # * other coefficients should reflect differences of no more than factor 2 (or 0.5) with 99% prob
  #   -> ((0.35 * c(-2/2,0,2/2) ) %>% add(0.0) %>% exp()) %>% {.[3]/.[1]}
  set_prior(nlpar = "dprimeInvrate", prior = "normal(0, .35)", class = "b"),
  #
  # transformation: exp(x - 1)
  # * intercept to be kept roughly in [0.1; 1.2], with ~95% prob
  #   -> (0.6 * c(-2,0,2) ) %>% add(-1) %>% exp()
  # * other coefficients should reflect differences of no more than factor 2 (or 0.5) with 99% prob
  #   -> ((0.35 * c(-2/2,0,2/2) ) %>% add(-1) %>% exp()) %>% {.[3]/.[1]}
  set_prior(nlpar = "dprimeIntercept", prior = "normal(0, .6)", class = "b")
)

priors$crit = c(
            # transformation: identity
            # * intercept to be kept roughly in [-3; 3], with ~95% prob
            # * other coefficients should reflect differences of no more than 1 unit (or 0.5) with 95% prob
            set_prior(nlpar = "critLeftAsym", prior = "normal(0, 1.5)", class = "b", coef = "Intercept"),
            set_prior(nlpar = "critLeftAsym", prior = "normal(0, 0.5)", class = "b"),
            #
            # transformation: identity
            # * intercept to be kept roughly in [-3; 3], with ~95% prob
            # * other coefficients should reflect differences of no more than 1 unit (or 0.5) with 95% prob
            set_prior(nlpar = "critRightAsym", prior = "normal(0, 1.5)", class = "b", coef = "Intercept"),
            set_prior(nlpar = "critRightAsym", prior = "normal(0, 0.5)", class = "b"),
            #
            # transformation: exp(x)
            # * intercept to be kept roughly in [0.5; 1.5], with ~95% prob
            #   -> (0.35 * c(-2,0,2) ) %>% add(0.0) %>% exp()
            # * other coefficients should reflect differences of no more than factor 2 (or 0.5) with 99% prob
            #   -> ((0.35 * c(-2/2,0,2/2) ) %>% add(0.0) %>% exp()) %>% {.[3]/.[1]}
            set_prior(nlpar = "critInvrate", prior = "normal(0, 0.35)", class = "b", coef = "Intercept"),
            set_prior(nlpar = "critInvrate", prior = "normal(0, 0.35)", class = "b"),
            #
            # transformation: exp(x - 1)
            # * intercept to be kept roughly in [0.1; 1.2], with ~95% prob
            #   -> (0.6 * c(-2,0,2) ) %>% add(-1) %>% exp()
            # * other coefficients should reflect differences of no more than factor 2 (or 0.5) with 99% prob
            #   -> ((0.35 * c(-2/2,0,2/2) ) %>% add(-1) %>% exp()) %>% {.[3]/.[1]}
            set_prior(nlpar = "critIntercept", prior = "normal(0, .35)", class = "b", coef = "Intercept"),
            set_prior(nlpar = "critIntercept", prior = "normal(0, .35)", class = "b")
          )
priors$rho = c(  set_prior(dpar = "rho", "normal(1, 1)",  class = "Intercept") 
			  	      #set_prior(dpar = "rho", "normal(0.5, 0.75)",  class = "Intercept") 
			        )
priors$p_rep = c(  set_prior(nlpar = "pRepIcpt", "normal(1, 1)", class = "b"),
                   set_prior(nlpar = "pRepSlope", "normal(-1, 1)", class = "b")
                )

# ggdist::qstudent_t(1-c(.05)/2, df=50, mu=0, sigma=.3)
# ggdist::qstudent_t(1-.01/2, df=50, mu=0, sigma=.4)
# ggdist::qstudent_t(1-.01/2, df=50, mu=0, sigma=.2)


priors$dprime_randef_subj = c(
            # Let the by-subject intercept vary from 1/3 to 3 times the grand mean with prob of 95%,
            # which means that the SD should be allowed to go up to 0.55, see below
            # -> ((0.55 * c(-2/2,0,2/2) ) %>% exp()) %>% {.[3]/.[1]}
            # Let's use a prior where ~99% of the mass is below ~0.55.
            # The same logic applies to all the log-transformed priors below (all d', and invrate and intercept for crit).
            set_prior(nlpar = "dprimeAsymptote", prior = "student_t(50, 0, .2)", class = "sd", group = "subject"),
            #
            set_prior(nlpar = "dprimeInvrate", prior = "student_t(50, 0, .2)", class = "sd", group = "subject"),
            #
            set_prior(nlpar = "dprimeIntercept", prior = "student_t(50, 0, .2)", class = "sd", group = "subject")
          )
priors$crit_randef_subj = c(
          # Let the by-subject intercept vary from -2 to 2 with prob of 95%, 
          # which means that the SD should be allowed to go up to 1.
          # Let's use a prior where ~99% of the mass is below ~1.
          set_prior(nlpar = "critLeftAsym", prior = "student_t(50, 0, .4)", class = "sd", group = "subject"),
          #
          # Same logic as above parameter.
          set_prior(nlpar = "critRightAsym", prior = "student_t(50, 0, .4)", class = "sd", group = "subject"),
          #
          # priors for the parameters below follow the logic outlined for the d' priors above.
          set_prior(nlpar = "critInvrate", prior = "student_t(50, 0, .2)", class = "sd", group = "subject"),
          #
          set_prior(nlpar = "critIntercept", prior = "student_t(50, 0, .2)", class = "sd", group = "subject")
        )
priors$dprime_randef_item = c(
          # Let the by-item intercept vary from 1/2 to 2 times the grand mean with prob of 95%,
          # which means that the SD should be allowed to go up to 0.35, see below
          # -> ((0.35 * c(-2/2,0,2/2) ) %>% exp()) %>% {.[3]/.[1]}
          # Let's use a prior where ~99% of the mass is below ~0.35.
          # The same logic applies to all the log-transformed priors below (all d', and invrate and intercept for crit).
          set_prior(nlpar = "dprimeAsymptote", prior = "student_t(50, 0, .125)", class = "sd", group = "item"),
          #
          set_prior(nlpar = "dprimeInvrate", prior = "student_t(50, 0, .125)", class = "sd", group = "item"),
          #
          set_prior(nlpar = "dprimeIntercept", prior = "student_t(50, 0, .125)", class = "sd", group = "item")
        )
priors$crit_randef_item = c(
          # Let the by-subject intercept vary from -1 to 1 with prob of 95%, 
          # which means that the SD should be allowed to go up to 0.5.
          # Let's use a prior where ~99% of the mass is below ~0.5.
          set_prior(nlpar = "critLeftAsym", prior = "student_t(50, 0, .2)", class = "sd", group = "item"),
          #
          set_prior(nlpar = "critRightAsym", prior = "student_t(50, 0, .2)", class = "sd", group = "item"),
          #
          # priors for the parameters below follow the logic outlined for the d' priors above.
          set_prior(nlpar = "critInvrate", prior = "student_t(50, 0, .125)", class = "sd", group = "item"),
          #
          set_prior(nlpar = "critIntercept", prior = "student_t(50, 0, .125)", class = "sd", group = "item")
        )
# priors$rho_randef = c(set_prior(dpar = "rho", "student_t(3, 0, 1)",  class = "sd") )

# ### code for playing around with priors ###
# ###########################################
# 
# ggdist::qstudent_t(1-c(.05)/2, df=50, mu=0, sigma=1)
# ggdist::qstudent_t(1-c(.05)/2, df=50, mu=0, sigma=.5)
# ggdist::qstudent_t(1-c(.05)/2, df=50, mu=0, sigma=.25)
# ggdist::qstudent_t(1-c(.05)/2, df=1000, mu=0, sigma=.5)





sat_bf <- function(dprime, crit,
                   dprime_asymptote=NULL, dprime_invrate=NULL, dprime_intercept=NULL, 
                   crit_leftasym=NULL, crit_rightasym=NULL, crit_invrate=NULL, crit_intercept=NULL)
{
  uf <- function(dv_formula, formula_def, formula_alt = NULL) {
      formula <- if(is.null(formula_alt)) { formula_def } else { formula_alt }
      update.formula(dv_formula, formula)
  }
  ufd <- function(dv_formula, formula_alt = NULL) uf(dv_formula, dprime, formula_alt)
  ufc <- function(dv_formula, formula_alt = NULL) uf(dv_formula, crit, formula_alt)
  
  ret <- bf( responseGrammatical ~ 
                  criterion_fn(time, critLeftAsym, critRightAsym, critInvrate, critIntercept) -
                  (1-is_ungrammatical)*dprime_fn(time, is_ungrammatical, dprimeAsymptote, dprimeInvrate, dprimeIntercept),
              ufd( dprimeAsymptote ~ ., dprime_asymptote),
                  ufd( dprimeInvrate ~ ., dprime_invrate),
                  ufd( dprimeIntercept ~ ., dprime_intercept),
              ufc( critLeftAsym ~ ., crit_leftasym),
                  ufc( critRightAsym ~ ., crit_rightasym),
                  ufc( critInvrate ~ ., crit_invrate),
                  ufc( critIntercept ~ ., crit_intercept),
             # dprime_asymptote, dprime_invrate, dprime_intercept,
             # crit_leftasym, crit_rightasym, crit_invrate, crit_intercept,
              family = bernoulli_normal,
              nl = TRUE)
  print(ret)
  ret
}


sat_bf_binomial <- function(dprime, crit,
                   dprime_asymptote=NULL, dprime_invrate=NULL, dprime_intercept=NULL, 
                   crit_leftasym=NULL, crit_rightasym=NULL, crit_invrate=NULL, crit_intercept=NULL)
{
  uf <- function(dv_formula, formula_def, formula_alt = NULL) {
    formula <- if(is.null(formula_alt)) { formula_def } else { formula_alt }
    update.formula(dv_formula, formula)
  }
  ufd <- function(dv_formula, formula_alt = NULL) uf(dv_formula, dprime, formula_alt)
  ufc <- function(dv_formula, formula_alt = NULL) uf(dv_formula, crit, formula_alt)
  
  
  # 
  ret <- bf( n_yes | trials(n) ~ 
               -criterion_fn(time, critLeftAsym, critRightAsym, critInvrate, critIntercept) +
               (1-is_ungrammatical)*dprime_fn(time, is_ungrammatical, dprimeAsymptote, dprimeInvrate, dprimeIntercept),
             ufd( dprimeAsymptote ~ ., dprime_asymptote),
             ufd( dprimeInvrate ~ ., dprime_invrate),
             ufd( dprimeIntercept ~ ., dprime_intercept),
             ufc( critLeftAsym ~ ., crit_leftasym),
             ufc( critRightAsym ~ ., crit_rightasym),
             ufc( critInvrate ~ ., crit_invrate),
             ufc( critIntercept ~ ., crit_intercept),
             # dprime_asymptote, dprime_invrate, dprime_intercept,
             # crit_leftasym, crit_rightasym, crit_invrate, crit_intercept,
             family = binomial(link = "probit"),
             nl = TRUE)
  print(ret)
  ret
}


#
# sat_bf_rep <- function(dprime, crit, p_rep,
#                        dprime_asymptote=NULL, dprime_invrate=NULL, dprime_intercept=NULL, 
#                        crit_leftasym=NULL, crit_rightasym=NULL, crit_invrate=NULL, crit_intercept=NULL)
# {
#   uf <- function(dv_formula, formula_def, formula_alt = NULL) {
#     formula <- if(is.null(formula_alt)) { formula_def } else { formula_alt }
#     update.formula(dv_formula, formula)
#   }
#   ufd <- function(dv_formula, formula_alt = NULL) uf(dv_formula, dprime, formula_alt)
#   ufc <- function(dv_formula, formula_alt = NULL) uf(dv_formula, crit, formula_alt)
#   
#   ret <- bf( responseGrammatical ~ 
#              criterion_fn(time, critLeftAsym, critRightAsym, critInvrate, critIntercept) -
#                 (1-is_ungrammatical)*dprime_fn(time, is_ungrammatical, dprimeAsymptote, dprimeInvrate, dprimeIntercept),
#              ufd( dprimeAsymptote ~ ., dprime_asymptote),
#                ufd( dprimeInvrate ~ ., dprime_invrate),
#                ufd( dprimeIntercept ~ ., dprime_intercept),
#              ufc( critLeftAsym ~ ., crit_leftasym),
#                ufc( critRightAsym ~ ., crit_rightasym),
#                ufc( critInvrate ~ ., crit_invrate),
#                ufc( critIntercept ~ ., crit_intercept),
#              
#              nlf(pRep ~ pRepIcpt + pRepSlope*delta_dprime_crit_fn(last_time, time, is_ungrammatical,
#                                                                    dprimeAsymptote, dprimeInvrate, dprimeIntercept,
#                                                                    critLeftAsym, critRightAsym, critInvrate, critIntercept) ),
#              uf( pRepIcpt ~ 1, p_rep ),
#              uf( pRepSlope ~ 1, p_rep ),
#              
#              family = bernoulli_normal_rep,
#              nl = TRUE)
#   print(ret)
#   ret
# }
# 
# 
# sat_bf_corr <- function(dprime, crit, rho,
#                         dprime_asymptote=NULL, dprime_invrate=NULL, dprime_intercept=NULL, 
#                         crit_leftasym=NULL, crit_rightasym=NULL, crit_invrate=NULL, crit_intercept=NULL)
# {
#   uf <- function(dv_formula, formula_def, formula_alt = NULL) {
#     formula <- if(is.null(formula_alt)) { formula_def } else { formula_alt }
#     update.formula(dv_formula, formula)
#   }
#   ufd <- function(dv_formula, formula_alt = NULL) uf(dv_formula, dprime, formula_alt)
#   ufc <- function(dv_formula, formula_alt = NULL) uf(dv_formula, crit, formula_alt)
#   
#     ret <- bf( # current response: d' + criterion formula
#               formula = responseGrammatical ~ criterion_fn(time, critLeftAsym, critRightAsym, critInvrate, critIntercept) -
#                                               dprime_fn(time, is_ungrammatical, dprimeAsymptote, dprimeInvrate, dprimeIntercept),
# 
#               # last response: d' + criterion formula
#               nlf( lastMu ~ criterion_fn(last_time, critLeftAsym, critRightAsym, critInvrate, critIntercept) -
#                               dprime_fn(last_time, is_ungrammatical, dprimeAsymptote, dprimeInvrate, dprimeIntercept) ),
# 
#               ufd( dprimeAsymptote ~ ., dprime_asymptote),
#                   ufd( dprimeInvrate ~ ., dprime_invrate),
#                   ufd( dprimeIntercept ~ ., dprime_intercept),
#               ufc( critLeftAsym ~ ., crit_leftasym),
#                   ufc( critRightAsym ~ ., crit_rightasym),
#                   ufc( critInvrate ~ ., crit_invrate),
#                   ufc( critIntercept ~ ., crit_intercept),
#               uf( rho ~ ., rho ),
# 
#               family = bernoulli_normal_rep,
#               nl = TRUE
#             )
# 
#   ret
# }
# 
# 
# # cumulative conditional normal distribution 
# bernoulli_normal_cc <- custom_family(
#   "bernoulli_normal_cc", dpars = c("mu", "lastMu", "rho"),
#   links = c("identity", "identity", "identity"),
#   lb = c(NA, NA, NA), ub = c(NA, NA, NA),
#   type = "int", vars = c("last_responseGrammatical[n]", "delta_time[n]")
# )
# 
# # assumes repetition of responses
# bernoulli_normal_rep <- custom_family(
#   "bernoulli_normal_rep", dpars = c("mu", "pRep"),
#   links = c("identity", "identity"),
#   lb = c(NA, NA), ub = c(NA, NA),
#   type = "int", vars = c("last_responseGrammatical[n]", "delta_time[n]")
# )

# cumulative normal distribution (equivalent to a probit link, but the transformation by Phi() that brms automatically applies - 
# instead, we go right for the *log*-probabilities of the responses).
bernoulli_normal <- custom_family(
  "bernoulli_normal", dpars = c("mu"),
  links = c("identity"),
  lb = c(NA), ub = c(NA),
  type = "int"
)

