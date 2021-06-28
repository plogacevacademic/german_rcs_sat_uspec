// functions {
  
  // note: new paper with a different approximation: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2924071  
  //       and: https://www.tandfonline.com/doi/pdf/10.1080/03610926.2011.611316
  

  // ------------------------------------------------------------------------------------------------------- //
  
  real transform_asymptote_dprime(real asymptote_unconstrained) {
      real shift_center = +0;
      return exp(asymptote_unconstrained + shift_center);
  }

  real transform_invrate_dprime(real invrate_unconstrained) {
      real shift_center = 0;
      return exp(invrate_unconstrained + shift_center);
  }

  real transform_intercept_dprime(real intercept_unconstrained) {
      real shift_center = -1;
      return exp(intercept_unconstrained + shift_center);
  }



  real transform_lasymptote_criterion(real asymptote_unconstrained) {
      real shift_center = 0;
      return asymptote_unconstrained + shift_center;
  }

  real transform_rasymptote_criterion(real asymptote_unconstrained) {
      real shift_center = 0;
      return asymptote_unconstrained + shift_center;
  }

  real transform_invrate_criterion(real invrate_unconstrained) {
      real shift_center = 0;
      return exp(invrate_unconstrained + shift_center);
  }

  real transform_intercept_criterion(real intercept_unconstrained) {
      real shift_center = -1;
      return exp(intercept_unconstrained + shift_center);
  }

  real transform_rho(real lo_rho_intercept, real delta_time) {
      real rho_intercept = 0.7 * inv_logit(lo_rho_intercept);
      real rho = sqrt( 1/(1 + delta_time * (1/rho_intercept^2 - 1) ) );
      return rho;
  }

  // ------------------------------------------------------------------------------------------------------- //

  real dprime_fn(real time, real is_noise_dist, real asymptote_unconstrained, 
                                                 real invrate_unconstrained, 
                                                 real intercept_unconstrained)
  {
      if (is_noise_dist == 0.0) {
          real intercept = transform_intercept_dprime( intercept_unconstrained );
          if (time >= intercept)
          {
              real asymptote = transform_asymptote_dprime( asymptote_unconstrained );
              real invrate = transform_invrate_dprime( invrate_unconstrained );
              return asymptote * (1 - exp(-1/invrate * (time - intercept)) );
          }
      }
      return 0;
  }

  real criterion_fn(real time, real left_asymptote_unconstrained, real right_asymptote_unconstrained, 
                                real invrate_unconstrained, real intercept_unconstrained)
  {
      real intercept = transform_intercept_criterion( intercept_unconstrained );
      real left_asymptote = transform_lasymptote_criterion( left_asymptote_unconstrained );
    
      if (time >= intercept) {
          real right_asymptote = transform_rasymptote_criterion( right_asymptote_unconstrained );
          real invrate = transform_invrate_criterion( invrate_unconstrained );
          real delta_asymptotes = right_asymptote - left_asymptote;
    
           return delta_asymptotes * (1 - exp(-1/invrate * (time - intercept)) ) + left_asymptote;
      }
      
      return left_asymptote;
  }
  
  real delta_dprime_crit_fn(real last_time, real cur_time, real is_noise_dist, 
                            real dprime_asymptote_unconstrained, real dprime_invrate_unconstrained, real dprime_intercept_unconstrained,
                            real crit_left_asymptote_unconstrained, real crit_right_asymptote_unconstrained, real crit_invrate_unconstrained, real crit_intercept_unconstrained)
  {
      real last_pos;
      real cur_pos;
      real delta_pos;

      if (last_time == -10) {
        return (0); // won't be used anyway, since the log-likelihood fuction will call the version which is not conditioned on anything
      }
      
      last_pos = criterion_fn(last_time, crit_left_asymptote_unconstrained, crit_right_asymptote_unconstrained, crit_invrate_unconstrained, crit_intercept_unconstrained) - 
                    (0.5-is_noise_dist)*dprime_fn(last_time, 0, dprime_asymptote_unconstrained, dprime_invrate_unconstrained, dprime_intercept_unconstrained);
      cur_pos = criterion_fn(cur_time, crit_left_asymptote_unconstrained, crit_right_asymptote_unconstrained, crit_invrate_unconstrained, crit_intercept_unconstrained) - 
                    (0.5-is_noise_dist)*dprime_fn(cur_time, 0, dprime_asymptote_unconstrained, dprime_invrate_unconstrained, dprime_intercept_unconstrained);
      delta_pos = cur_pos - last_pos;

      return (delta_pos);
}


  // real criterion_fn(real time, real avg_crit_unconstrained, real delta_crit_unconstrained,
  //                              real invrate_unconstrained,  real midpoint_unconstrained)
  // {
  //     real midpoint = transform_midpoint_criterion( midpoint_unconstrained );
  //     real invrate = transform_invrate_criterion( invrate_unconstrained );
  //     
  //     real right_asymptote = avg_crit_unconstrained + 0.5*delta_crit_unconstrained;
  //     real left_asymptote = avg_crit_unconstrained - delta_crit_unconstrained * (0.5 + 1.0/exp(midpoint/invrate));
  //     real delta_asymptotes = right_asymptote - left_asymptote;
  //   
  //     return delta_asymptotes / ( 1 + exp( -1/invrate * (time - midpoint) ) ) + left_asymptote;
  // }


  // ------------------------------------------------------------------------------------------------------- //

  real lpnorm_conditional_response_lo_rho(real lo_rho_intercept, 
                                          real relative_criterion, real last_relative_criterion,
                                          int response_above_criterion, int last_response_above_criterion,
                                          real delta_time)
  {
      real rho = transform_rho(lo_rho_intercept, delta_time);
      return lpnorm_conditional_response(rho, 
                                         relative_criterion, last_relative_criterion,
                                         response_above_criterion, last_response_above_criterion);
  }
  
  

  real bernoulli_normal_lpmf(int x, real mu)
  {
      if (x == 0) {
          return (normal_lcdf( mu | 0, 1));
      } else {
          return (normal_alt_lccdf( mu | 0, 1)); 
      }
  }

  real bernoulli_normal_rep_lpmf(int x, real mu, real lo_rep, int last_x, real delta_time)
  {
      real ll_rep_response;
      real ll_new_response;
      real ll_rep = log( inv_logit(lo_rep) );
      real ll_nonrep = log( inv_logit(-lo_rep) );

      if (last_x == -1) {
        return bernoulli_normal_lpmf(x| mu);
      }
      
      if (x == 0) {
          ll_new_response = normal_lcdf( mu | 0, 1);
      } else {
          ll_new_response = normal_alt_lccdf( mu | 0, 1);
      }
      
      if (last_x == x) {
          return log_sum_exp(ll_nonrep+ll_new_response, ll_rep);
      } else {
          return (ll_nonrep + ll_new_response);
      }
  }

  real bernoulli_normal_cc_lpmf(int x, real mu, real last_mu, real lo_rho, int last_x, real delta_time)
  {
      if (last_x == -1) {
        return bernoulli_normal_lpmf(x| mu);
      } 
      
      return lpnorm_conditional_response_lo_rho(lo_rho, mu, last_mu, x, last_x, delta_time);
  }

// } model { }
