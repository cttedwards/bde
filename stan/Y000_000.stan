// two-part regression of 
// by-catch data 

// NOTATION
//
// N: number of data records
// Y: number of years
// X: design matrices
// gamma: bernoulli regression coefficients
// beta: log-normal regression coefficients

functions {
	int num_nonzero(int[] y) {
		int np = 0;
		for (n in 1:size(y))
			if (y[n] > 0)
				np += 1;
		return np;
	}
	
	real calc_sigma_z(real mu, real sigma, int n) {
            
        return sqrt(log((exp(square(sigma)) - 1) / n + 1));
    }
            
	real calc_mu_z(real mu, real sigma, int n) {
	
		real sigma_z = calc_sigma_z(mu, sigma, n);
	
		return log(n) + mu + square(sigma) / 2 - square(sigma_z) / 2;
	}
	
	real vector_norm(vector x) {
	    
	    real i = 0.0;
	    
	    for (j in 1:num_elements(x))
	        i += pow(x[j], 2.0);
	        
	    return pow(i, 0.5);
	}
}
data {

	// DIMENSIONS
	int N[2]; 
	int Y;
	
	// LOOK-UP VECTORS
	int XY_sample[N[1]];
	int XY_predict[N[2]];
	
	// OBSERVER DATA
	real pos[N[1]]; 
	int  bin[N[1]];
	int  eff_sample[N[1]];
	
	// COMMERCIAL DATA
	int eff_predict[N[2]];
	
	// LOGICALS
	// none
}
transformed data {

	// number of positive 
	// catch records
	int N_nz = num_nonzero(bin);
	
	// positive catch vector
	// and look-up vectors
	real pos_nz[N_nz];
	int XY_sample_nz[N_nz];
	
	// aggregated data
	real pos_sum[Y];
	int  bin_sum[Y];
	int  eff_sample_sum[Y];
	int  eff_predict_sum[Y];
	int  eff_resid_sum[Y];

	// augmented location
	// priors for intercept 
	// terms
	real loc_prior[2];
	{
		real n[2];
		n[1] = N[1];
		n[2] = N[2];
	
		// binomial intercept
		loc_prior[1] = logit(sum(bin) / n[1]);
		
		// lognormal intercept
		loc_prior[2] = 0.0;
		for (i in 1:N[1]) {
			if (pos[i] > 0)
				loc_prior[2] += log(pos[i]);
		}
		loc_prior[2] /= sum(bin);
	}
	
	// initialise to zero and
	// sum data for each 
	// year and area
	for (i in 1:Y) {
		
		bin_sum[i] = 0;
		pos_sum[i] = 0.0;
			
		eff_sample_sum[i]  = 0;
		eff_predict_sum[i] = 0;
	}
	
	for (i in 1:N[1]) {
	
		bin_sum[XY_sample[i]] += bin[i];
		pos_sum[XY_sample[i]] += pos[i];
		
		// observer sampling effort
		eff_sample_sum[XY_sample[i]] += eff_sample[i];
	}
	
	for (i in 1:N[2]) {
			
		// commercial effort
		eff_predict_sum[XY_predict[i]] += eff_predict[i];
	}
	
	for (i in 1:Y) {
		
		// residual (unobserved) commercial effort
		eff_resid_sum[i] = max(eff_predict_sum[i] - eff_sample_sum[i], 0);
	}
	
	{
		int loc = 1;
		for (i in 1:N[1]) {
			if (bin[i]) {
			
				// positive catch record
				pos_nz[loc]       = pos[i];
				
				// look-up vector
				XY_sample_nz[loc] = XY_sample[i];
				
				loc += 1;
			}
		}
	}
}
parameters {

	// binomial catch coefficients
	real      gamma0;
	vector[Y] gammaY;
	
	// positive catch coefficients
	real      beta0;
	vector[Y] betaY;
}
transformed parameters {
	
	vector[Y] sigma = rep_vector(1.0, Y);
}
model {
	
	real theta_logit;
	real mu_log[N_nz];
	
	// binomial model using aggregated
	// sufficient statistic
	for (i in 1:Y) {
	
		theta_logit = gamma0 + gammaY[i];

		bin_sum[i] ~ binomial_logit(eff_sample_sum[i], theta_logit);
	}
	
	// positive catch log-normal model
   	for(i in 1:N_nz) {
    	
    	mu_log[i] = beta0 + betaY[XY_sample_nz[i]];
	}
	
	pos_nz ~ lognormal(mu_log, sigma[XY_sample_nz]);
	
	// augmented priors for
	// intercept parameters
	gamma0 ~ normal(loc_prior[1], 1);
	beta0  ~ normal(loc_prior[2], 1);
	
	// logistic regression priors
	gammaY ~ normal(0, 1);
	
	// log-normal regression priors
	betaY ~ normal(0, 1);
	
	// error terms
	sigma ~ normal(0, 1);

}
generated quantities {
	
	// parameter summary statistics for
	// convergence diagnostics
	real gamma_summary[2];
	real beta_summary[2];
	
	// predictands
	real theta_logit;
	real mu_log;
	
	// transformed parameters
	real mu_z;
	real sigma_z;
		
	// expected values
	real bin_hat;
	real pos_hat;
	real bin_hat_sum[Y];
	real pos_hat_sum[Y];
	
	// simulated data
	// for diagnostics
	int  bin_sim;
	real pos_sim;
	int  bin_sim_sum[Y];
	real pos_sim_sum[Y];
	int  bin_sim_agg[Y];
	real pos_sim_agg[Y];
	
	// results
	int  bin_sim_obs[Y];
	real pos_sim_obs[Y];
	int  bin_sim_com[Y];
	real pos_sim_com[Y];
	
	// discrepancy measure
	real DBN[3] = {0.0, 0.0, 0.0};
	real DLN[3] = {0.0, 0.0, 0.0};
	
	// model output
	real predicted_catch[Y];
	
	// INITIALISE TO ZERO
	for (i in 1:Y) {
		
			bin_hat_sum[i] = 0;
			pos_hat_sum[i] = 0.0;
			
			bin_sim_sum[i] = 0;
			pos_sim_sum[i] = 0.0;
			
			pos_sim_agg[i] = 0.0;
			pos_sim_obs[i] = 0.0;
			pos_sim_com[i] = 0.0;
	}
	
	// CALCULATE EXPECTED VALUES
	// BY RECORD AND SUM BY YEAR
	for(i in 1:N[1]) {
			
		theta_logit = gamma0 + gammaY[XY_sample[i]];
		mu_log      = beta0 + betaY[XY_sample[i]];
		
		bin_hat = inv_logit(theta_logit);
		pos_hat = bin_hat * exp(mu_log + square(sigma[XY_sample[i]]) / 2);
		
		bin_hat_sum[XY_sample[i]] += bin_hat;
		pos_hat_sum[XY_sample[i]] += pos_hat;
	}
	
	// SIMULATE OBSERVATIONAL DATA 
	// BY RECORD AND SUM BY YEAR
	for(i in 1:N[1]) {
	
		theta_logit = gamma0 + gammaY[XY_sample[i]];
		mu_log      = beta0  + betaY[XY_sample[i]];
		
		bin_sim = bernoulli_logit_rng(theta_logit);
		pos_sim = bin_sim * lognormal_rng(mu_log, sigma[XY_sample[i]]);
			
		bin_sim_sum[XY_sample[i]] += bin_sim;
		pos_sim_sum[XY_sample[i]] += pos_sim;
	}
	
	// SIMULATE AGGREGATED OBSERVATIONAL DATA
	// FOR MODEL DIAGNOSTICS
	for (i in 1:Y) {
		
		theta_logit = gamma0 + gammaY[i];
		mu_log      = beta0  + betaY[i];
				
		bin_sim_agg[i] = binomial_rng(eff_sample_sum[i], inv_logit(theta_logit));
		
		if (bin_sim_agg[i] > 0) {
		
			sigma_z = calc_sigma_z(mu_log, sigma[i], bin_sim_agg[i]);
			mu_z    = calc_mu_z(mu_log, sigma[i], bin_sim_agg[i]);
			//print(mu_z);
			pos_sim_agg[i] = lognormal_rng(mu_z, sigma_z);
		}				
	}
	
	// SIMULATE AGGREGATED OBSERVATIONAL DATA
	// AND COMMERCIAL CATCH PREDICTION USING
	// RESIDUAL EFFORT
	for (i in 1:Y) {
		
		theta_logit = gamma0 + gammaY[i];
		mu_log      = beta0  + betaY[i];
		
		bin_sim_obs[i] = binomial_rng(eff_sample_sum[i], inv_logit(theta_logit));
		
		if (bin_sim_obs[i] > 0) {
		
			sigma_z = calc_sigma_z(mu_log, sigma[i], bin_sim_obs[i]);
			mu_z    = calc_mu_z(mu_log, sigma[i], bin_sim_obs[i]);
			
			pos_sim_obs[i] = lognormal_rng(mu_z, sigma_z);
		}
			
		bin_sim_com[i] = binomial_rng(eff_resid_sum[i], inv_logit(theta_logit));
		
		if (bin_sim_com[i] > 0) {
		
			sigma_z = calc_sigma_z(mu_log, sigma[i], bin_sim_com[i]);
			mu_z    = calc_mu_z(mu_log, sigma[i], bin_sim_com[i]);
			
			pos_sim_com[i] = lognormal_rng(mu_z, sigma_z);
		}			
	}
	
	// CALCULATE DISCREPANCIES
	for (i in 1:Y) {
		
		DBN[1] += bin_sum[i]     - bin_hat_sum[i];
		DBN[2] += bin_sim_sum[i] - bin_hat_sum[i];
		DBN[3] += bin_sim_agg[i] - bin_hat_sum[i];
		
		DLN[1] += pos_sum[i]     - pos_hat_sum[i];
		DLN[2] += pos_sim_sum[i] - pos_hat_sum[i];
		DLN[3] += pos_sim_agg[i] - pos_hat_sum[i];
	}
	
	// MODEL OUTPUT
	for (i in 1:Y) {
		//predicted_catch[i] = pos_sim_obs[i] + pos_sim_com[i];
		predicted_catch[i] = pos_sum[i] + pos_sim_com[i];
	}
	
	// PARAMETER SUMMARY STATISTICS 
	// FOR TRACE DIAGNOSTICS
	gamma_summary[1] = gamma0;
	gamma_summary[2] = vector_norm(gammaY);
	beta_summary[1]  = beta0;
	beta_summary[2]  = vector_norm(betaY);
}

