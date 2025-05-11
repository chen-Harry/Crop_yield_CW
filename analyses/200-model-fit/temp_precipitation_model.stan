data{
    int<lower=1> N;                 // number of observations
    int<lower=1> K;                 // number of crops
    vector[N] y;                    // crop yield
    array[N] int<lower=1, upper=K> crop; // crop index
    vector[N] temp;
    vector[N] precipitation;
}

transformed data{
    // centre variables for efficiency
    real mean_temp = mean(temp);
    real mean_precipitation = mean(precipitation);
    
    real<lower=0> sd_temp = sd(temp);
    real<lower=0> sd_precipitation = sd(precipitation);

    vector[N] temp_std = (temp - mean_temp) / sd_temp;
    vector[N] precipitation_std = (precipitation - mean_precipitation) / sd_precipitation;
}

parameters{
  
    vector[K] alpha_std;
    vector[K] beta_std;
    vector[K] gamma_std;
    
    vector<lower=0>[K] sigma;
}
transformed parameters{
    vector[N] mu;
    
    mu = alpha_std[crop] +
         beta_std[crop] .* temp_std +
         gamma_std[crop] .* precipitation_std;
}
model{
    // priors for observation noise
    sigma ~ normal( 0 , 1 ); 
    
    // priors for baseline and fixed effects
    alpha_std ~ normal(0, 10 );
    beta_std ~ normal(0, 1 );
    gamma_std ~ normal(0, 1 );
    
    // likelihood
    y ~ lognormal( mu , sigma[crop] );
}

generated quantities {
    vector[K] alpha = alpha_std -
                      beta_std * mean_temp / sd_temp -
                      gamma_std * mean_precipitation / sd_precipitation;
                    
    vector[K] beta = beta_std / sd_temp;
    vector[K] gamma = gamma_std / sd_precipitation;
    
    array[N] real y_pred = lognormal_rng(mu, sigma[crop]);
}

