functions 
{
  vector diagSPD_EQ(real alpha, real rho, real L, int M)
  {
    return alpha * 
      sqrt(sqrt(2*pi()) * rho) * 
      exp(-0.25*(rho*pi()/2/L)^2 * 
      linspaced_vector(M, 1, M)^2);
  }

  matrix PHI(int N, int M, real L, vector x)
  {
    return sin(
        diag_post_multiply(
          rep_matrix(pi()/(2*L) * (x+L), M), linspaced_vector(M, 1, M)
          )
        ) /
      sqrt(L);
  }
}
    
data
{
    int<lower=1> T;               // number of observations
    vector[T] y;                  // observed countsvalues
    
    // data for GP
    vector[T] standardised_t;               // year t in range[-1, 1]

    // additional distinct inputs for prediction
    int<lower=1> T_star;                // forecast year
    vector[T_star] standardised_t_star; // forecast year t_star in range[-1, 1]
    
    // HSGP arguments
    real<lower=0> hsgp_c;   // factor c to determine the boundary value L for the HSGP
    int<lower=1> hsgp_M;    // number of basis functions for the HSGP
}

transformed data
{
    matrix[T, hsgp_M] hsgp_PHI;
    matrix[T_star, hsgp_M] hsgp_PHI_star;
    
    int T_all = T + T_star;
    
    // precompute HSGP basis functions at inputs to fit
    real hsgp_L = hsgp_c*max(standardised_t);
    hsgp_PHI = PHI(T, hsgp_M, hsgp_L, standardised_t);
    
    // precompute HSGP basis functions at inputs to predict
    real hsgp_L_star = hsgp_c*max(standardised_t_star);
    hsgp_PHI_star = PHI(T_star, hsgp_M, hsgp_L_star, standardised_t_star);
}

parameters
{
    real<lower=0> sigma;
    real beta0;
    real beta1;
    real beta2;
    
    real<lower=0> gp_lengthscale;
    
    // non-centred parameterisation for gp_sigma ~ cauchy(0,1)
    real<lower=0> gp_sigma_tau1;
    real<lower=0> gp_sigma_tau2;
    vector[hsgp_M] z; 
}

transformed parameters
{
    real<lower=0> gp_sigma;
    gp_sigma = gp_sigma_tau1 * sqrt(gp_sigma_tau2);
    
    vector[T] f;
    vector[hsgp_M] hsgp_sqrt_spd;
      
    // square root of spectral densities
    hsgp_sqrt_spd = diagSPD_EQ( gp_sigma, gp_lengthscale, hsgp_L, hsgp_M);
    // construct HSGP at inputs for fitting
    f = hsgp_PHI * (hsgp_sqrt_spd .* z);
    
    vector[T] mu = beta0 +
                   beta1 * standardised_t +
                   beta2 * square(standardised_t) +
                   f;
}

model
{
    // priors for observation noise
    sigma ~ normal(0, 1);
    
    beta0 ~ normal(0, 10);
    beta1 ~ normal(0, 1);
    beta2 ~ normal(0, 1);
    
    // priors for GP
    gp_lengthscale ~ inv_gamma( 5, 1 );
    gp_sigma_tau1 ~ normal( 0.0 , 1.0 );
    gp_sigma_tau2 ~ inv_gamma(0.5, 0.5);
    z ~ std_normal();
    
    // likelihood
    y ~ normal(mu, sigma);
}

generated quantities{
    vector[T_star] mu_star;
    array[T_all] real y_all_pred;
    
    {
      // sample GP at prediction inputs
      mu_star = beta0 +
                beta1 * standardised_t_star + 
                beta2 * square(standardised_t_star) +
                hsgp_PHI_star * (hsgp_sqrt_spd .* z);
      
      // predict
      y_all_pred = normal_rng(append_row(mu, mu_star), sigma);
    }
}