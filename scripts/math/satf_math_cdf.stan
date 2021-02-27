// functions {



real exp_student_t(real x, real a, real b, real c) {
    return student_t_lpdf(x|a, b, c);
}

// normal_alt_lccdf() uses normal_lcdf() under the hood, because
// normal_lccdf() starts returning -inf for any log-likelihood below -37,
// unlike normal_lcdf() [although the documentation says that both, normal_lcdf, 
// and normal_lccdf do]

real normal_alt_lccdf(real x, real mu, real sigma) {
    return normal_lcdf(-x | -mu, sigma);
}

real exp_normal_lcdf(real x, real mu, real sigma) {
  return normal_lcdf(x|mu,sigma);
}

real exp_normal_lccdf(real x, real mu, real sigma) {
  return normal_lccdf(x|mu,sigma);
}

real lbinormal_conditional_cdf(real rho, real upper1, real upper2, 
                               int condition_on_positive_z1,
                               int loglik_complemen);


real lbinormal_conditional_cdf_as_normal(real rho, real upper1, real upper2,
                               int condition_on_positive_z1,
                               real upper2_probe_point_1, real upper2_probe_point_2,
                               int loglik_complement)
{
    real a;
    real b;
    real sigma;
    real mu;

    // to-do???
    real ll1 = lbinormal_conditional_cdf(rho, upper1, upper2_probe_point_1, condition_on_positive_z1, 0);
    real ll2 = lbinormal_conditional_cdf(rho, upper1, upper2_probe_point_2, condition_on_positive_z1, 0);
    if (is_nan(ll1) || exp(ll1) <= 0 || exp(ll1) >= 1 || 
        is_nan(ll2) || exp(ll2) <= 0 || exp(ll2) >= 1 ) {
        return not_a_number();
    }
    
    a = inv_Phi(exp(ll1));
    b = inv_Phi(exp(ll2));
    if (is_nan(a) || is_nan(b)) {
        return not_a_number();
    }
    
    sigma = (upper2_probe_point_1 - upper2_probe_point_2)/(a - b);
    mu = -(b*upper2_probe_point_1 - a*upper2_probe_point_2)/(a - b);
    // print("(", upper1, " ", upper2, " ", rho, ") -- (", 
    //       upper2_probe_point_1, " ", upper2_probe_point_2, ") -- (", 
    //       mu, " ", sigma, ")");
    if (is_nan(mu) || is_nan(sigma)) {
        return not_a_number();
    }
    if ( sigma <= 0) {
        return not_a_number();
    }

    
    if (loglik_complement) {
        return normal_alt_lccdf(upper2 | mu, sigma);
    } else {
        return normal_lcdf(upper2 | mu, sigma);
    }
}


// real lbinormal_conditional_cdf_as_linear(real upper1, real upper2, real rho,
//                                int condition_on_positive_z1,
//                                real upper2_probe_point_1, real upper2_probe_point_2)
// {
//     real intercept;
//     real slope;
//     real sigma;
//     real mu;
// 
//     real ll1 = lbinormal_conditional_cdf(upper1, upper2_probe_point_1, rho, condition_on_positive_z1);
//     real ll2 = lbinormal_conditional_cdf(upper1, upper2_probe_point_2, rho, condition_on_positive_z1);
//     if (is_nan(ll1) || exp(ll1) <= 0 || exp(ll1) >= 1 || 
//         is_nan(ll2) || exp(ll2) <= 0 || exp(ll2) >= 1 ) {
//         return not_a_number();
//     }
//     
//     slope = (exp(ll1) - exp(ll2)) / (upper2_probe_point_1 - upper2_probe_point_2);
//     intercept = exp(ll1) - slope * upper2_probe_point_1;
//     
//     return (intercept + slope * upper2);
// }


real lbinormal_conditional_cdf(real rho, 
                               real upper1, real upper2,
                               int condition_on_positive_z1,
                               int loglik_complement)
{
    real z_1;
    real z_2;
    real theta;
    real ret;
    real loglik;
    
    if ( fabs(rho) >= 1.0) {
       return not_a_number();
    }

    if (upper1 == -0.0) { z_1 = 0.0; } else { z_1 = upper1; }
    if (upper2 == -0.0) { z_2 = 0.0; } else { z_2 = upper2; }


    // use -5 and 5 as boundaries if we are outside of the interval in which this approximation 
    // seems to work well (the boundary computed below only works well for crit and crit [-5; +5]
    //
    if ( fabs(upper1) > 5 || fabs(upper2) > 5 )
    {
          return not_a_number();

    //     real new_upper1 = upper1;
    //     real new_upper2 = upper2;
    //     real scale_upper1 = 1;
    //     real scale_upper2 = 1;
    //     print("out of range");
    // 
    //     if (upper1 > 5) {
    //         new_upper1 = 5;
    //         scale_upper1 = 1 + (upper1 - 5);
    //       
    //     } else if (upper1 < -5) {
    //         new_upper1 = -5;
    //     }
    //   
    //     if (upper2 > 5) {
    //         new_upper2 = 5;
    //         
    //     } else if (upper2 < -5) {
    //         new_upper2 = -5;
    //     }
    //     
    //     // to-do: problem with complement?
    //     loglik = lbinormal_conditional_cdf(rho, new_upper1, new_upper2, condition_on_positive_z1, loglik_complement);
    //     loglik = loglik /scale_upper1 ;
    //     return loglik;
    // 
    } else
    { // if the distance between the criteria is too large for the approximation to work well,
             // let's approximate it with a normal distribution based on the log-likelihood of the primary
             // approximation at two points right before the boundary
      real max_delta = (10.5*(1-rho)^0.55);

      if (upper2 > upper1 + max_delta)
      {
          return not_a_number();

          // real probe_point_delta = 0.1;
          // real upper2_probe_point_1 = upper1 + max_delta - probe_point_delta;
          // real upper2_probe_point_2 = upper1 + max_delta;
          // 
          // // this approximation to the apprixmation *may* be causing divergent transitions in stan
          // // loglik = lbinormal_conditional_cdf_as_normal(rho, upper1, upper2,
          // //                                              condition_on_positive_z1,
          // //                                              upper2_probe_point_1, upper2_probe_point_2,
          // //                                              loglik_complement);
          // loglik = lbinormal_conditional_cdf_as_normal(rho, upper1, upper2,
          //                                              condition_on_positive_z1,
          //                                              upper2_probe_point_1, upper2_probe_point_2,
          //                                              loglik_complement);
          // 
          // print("approximation");
          // print(loglik);
          // 
          // if (is_nan(loglik)) {
          //   print ("Appproximation to approximation returning NaN.");
          // }
          // 
          // return loglik;
      }
    }
    
    if (condition_on_positive_z1) {
        theta = normal_lcdf(z_1 | 0, 1);
    } else {
        theta = normal_lcdf(-z_1 | 0, 1);
    }

    if (z_1 != 0 || z_2 != 0)
    {
        real denom = sqrt((1 + rho) * (1 - rho));
        real a1 = (z_2 / z_1 - rho) / denom;
        real a2 = (z_1 / z_2 - rho) / denom;
        real product = z_1 * z_2;
        real delta = product < 0 || (product == 0 && (z_1 + z_2) < 0);
        real eta = 2*owens_t(z_1, a1) + 2*owens_t(z_2, a2);

        if (delta) {
            if (fabs(z_2) > fabs(z_1) ) {
                ret = Phi(z_2) - Phi(-z_1) - eta;
            } else {
                ret = Phi(z_1) - Phi(-z_2) - eta;
            }
        } else {
            ret = Phi(z_1) + Phi(z_2) - eta;
        }
        
        ret = log(ret);
        
    } else {
        ret = log(2) + log( 0.25 + asin(rho) / (2 * pi()) );
    }

    loglik = (log(0.5) + ret - theta);
    if (loglik_complement) {
      loglik = log1m_exp(loglik);
    }
    return loglik;
}

// https://github.com/stan-dev/stan/issues/2356
//


real lbinormal_conditional(real rho, real z1, real z2, real x1, real x2)
{
  real loglik;

  if (x1 == 0) {
      loglik = lbinormal_conditional_cdf(rho, z1, z2, (x1 == 0), (x2 == 1) );

  } else if (x1 == 1) {
      loglik = lbinormal_conditional_cdf(rho, -z1, -z2, (x1 == 1), (x2 == 0) );
  }

  return loglik;
}

real lpnorm_conditional_response(real rho,
                                 real relative_criterion, real last_relative_criterion,
                                 int response_above_criterion, int last_response_above_criterion)
{
  return lbinormal_conditional(rho, 
                               last_relative_criterion, relative_criterion, 
                               last_response_above_criterion, response_above_criterion);
}


// } model { }

