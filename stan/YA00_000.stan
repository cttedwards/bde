// regression of 
// by-catch and discard 
// data using a two-part
// binomial/log-normal model 

// NOTATION
//
// N: number of data records (estimation; prediction)
// A: number of areas (sampling; estimation)
// Y: number of years
// X: design matrices represented as look-up vectors
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
	
	real vector_norm(vector x) {
	    
	    real i = 0.0;
	    
	    for (j in 1:num_elements(x))
	        i += pow(x[j], 2.0);
	        
	    return pow(i, 0.5);
	}
	
	real calc_sigma_z(real mu, real sigma, int n) {
            
        return sqrt(log((exp(square(sigma)) - 1) / n + 1));
    }
            
	real calc_mu_z(real mu, real sigma, int n) {
	
		real sigma_z = calc_sigma_z(mu, sigma, n);
	
		return log(n) + mu + square(sigma) / 2 - square(sigma_z) / 2;
	}
}
data {

	// DIMENSIONS
	int N[2]; 
	int A;
	int Y;
	
	// LOOK-UP VECTORS
	int XY_sample[N[1]];
	int XA_sample[N[1]];
	int XY_predict[N[2]];
	int XA_predict[N[2]];
	
	// SAMPLING AREA
	// TO ESTIMATION AREA
	// MAPPING VECTOR
	// none
	
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
	int XA_sample_nz[N_nz];
	int XY_sample_nz[N_nz];
	
	// aggregated data
	// by sampling area
	int  eff_sample_sum[Y, A];
	int  eff_predict_sum[Y, A];
	int  eff_resid_sum[Y, A];
	
	// aggregated data by 
	// estimation area
	int  eff_estimate_sum[Y, A];
	real pos_sum[Y, A];
	int  bin_sum[Y, A];
	
	// augmented location
	// priors for intercept 
	// terms
	real loc_prior[2];
	{
		real n[2];
		n[1] = N[1];
		n[2] = N[2];
	
		// binomial intercept
		loc_prior[1] = (sum(bin) == n[1]) ? 1e3 : logit(sum(bin) / n[1]);
		
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
	
		// sampling areas
		for (j in 1:A) {
		
			eff_sample_sum[i, j]  = 0;
			eff_predict_sum[i, j] = 0;
		}
		
		// estimation areas
		for (j in 1:A) {
		        
			eff_estimate_sum[i, j]  = 0;
		
			bin_sum[i, j] = 0;
			pos_sum[i, j] = 0.0;
		}
	}
	
	// sum data for sampling 
	// and estimation areas
	for (i in 1:N[1]) {
	
		// data
		bin_sum[XY_sample[i], XA_sample[i]] += bin[i];
		pos_sum[XY_sample[i], XA_sample[i]] += pos[i];
		
		// observer sampling effort
		eff_estimate_sum[XY_sample[i], XA_sample[i]] += eff_sample[i];
		eff_sample_sum[XY_sample[i], XA_sample[i]] += eff_sample[i];
	}
	
	// sum commercial effort data
	// for sampling/prediction areas
	for (i in 1:N[2]) {
			
		// commercial effort
		eff_predict_sum[XY_predict[i], XA_predict[i]] += eff_predict[i];
	}
	
	// calculate residual 
	// commercial effort
	for (i in 1:Y) {
		for (j in 1:A) {
		
			// residual (unobserved) commercial effort
			eff_resid_sum[i, j] = max(eff_predict_sum[i, j] - eff_sample_sum[i, j], 0);
		}
	}
	
	// re-define look-up 
	// vectors for non-zero
	// positive catch vector
	// (with zeros stripped out)
	{
		int loc = 1;
		for (i in 1:N[1]) {
			if (bin[i]) {
			
				// positive catch record
				pos_nz[loc] = pos[i];
				
				// look-up vectors
				XY_sample_nz[loc] = XY_sample[i];
				XA_sample_nz[loc] = XA_sample[i];
				
				loc += 1;
			}
		}
	}	
}
parameters {

	// binomial catch coefficients
	real      gamma0;
	vector[Y] gammaY;
	vector[A] gammaA;
	
	// positive catch coefficients
	real      beta0;
	vector[Y] betaY;
	vector[A] betaA;
	
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
		for (j in 1:A) {
			
			theta_logit = gamma0 + gammaY[i] + gammaA[j];

			bin_sum[i, j] ~ binomial_logit(eff_estimate_sum[i, j], theta_logit);
		}
	}
	
	// positive catch log-normal model
    for(i in 1:N_nz) {
    
    	mu_log[i] = beta0 + betaY[XY_sample_nz[i]] + betaA[XA_sample_nz[i]];
    	
    } 
	
	pos_nz ~ lognormal(mu_log, sigma[XY_sample_nz]);
	
	// augmented priors for
	// intercept parameters
	gamma0 ~ normal(loc_prior[1], 1);
	beta0  ~ normal(loc_prior[2], 1);
	
	// logistic regression priors
	gammaY ~ normal(0, 1);
	gammaA ~ normal(0, 1);
	
	// log-normal regression priors
	betaY ~ normal(0, 1);
	betaA ~ normal(0, 1);

}
generated quantities {
	
	// parameter summary statistics for
	// convergence diagnostics
	real gamma_summary[3];
	real beta_summary[3];
	
	// predictands
	real theta_logit;
	real mu_log;
		
	// expected values
	real bin_hat;
	real pos_hat;
	real bin_hat_sum[Y, A];
	real pos_hat_sum[Y, A];
	
	// simulated data
	// by record
	int  bin_sim;
	real pos_sim;
	
	// simulated data
	// by factor
	int  bin_sim_sum[Y, A];
	real pos_sim_sum[Y, A];
	int  bin_sim_agg[Y, A];
	real pos_sim_agg[Y, A];
	
	// results
	int  bin_sim_obs[Y, A];
	real pos_sim_obs[Y, A];
	int  bin_sim_com[Y, A];
	real pos_sim_com[Y, A];
	
	// discrepancy measures
	real DBN[3] = {0.0, 0.0, 0.0};
	real DLN[3] = {0.0, 0.0, 0.0};
	
	// model output
	real predicted_catch[Y, A];
	real predicted_catch_sum[Y];
	
	// INITIALISE TO ZERO
	for (i in 1:Y) {
	
		predicted_catch_sum[i] = 0.0;
	    
	    // by estimation area
		for (j in 1:A) {
		        
			bin_hat_sum[i, j] = 0;
			pos_hat_sum[i, j] = 0.0;
			
			bin_sim_sum[i, j] = 0;
			pos_sim_sum[i, j] = 0.0;
			
			pos_sim_agg[i, j] = 0.0;
		}
	    
	    // by sampling area
		for (j in 1:A) {
		
			pos_sim_obs[i, j] = 0.0;
			pos_sim_com[i, j] = 0.0;
		}
	}
	
	// CALCULATE EXPECTED VALUES
	// BY RECORD AND SUM
	for(i in 1:N[1]) {
			
		theta_logit = gamma0 + gammaY[XY_sample[i]] + gammaA[XA_sample[i]];
		mu_log      = beta0 + betaY[XY_sample[i]] + betaA[XA_sample[i]];
		
		bin_hat = inv_logit(theta_logit);
		pos_hat = bin_hat * exp(mu_log + square(sigma[XY_sample[i]]) / 2);
				
		bin_hat_sum[XY_sample[i], XA_sample[i]] += bin_hat;
		pos_hat_sum[XY_sample[i], XA_sample[i]] += pos_hat;
	}
	
	// SIMULATE OBSERVATIONAL DATA 
	// BY RECORD AND SUM
	for(i in 1:N[1]) {
	
		theta_logit = gamma0 + gammaY[XY_sample[i]] + gammaA[XA_sample[i]];
		mu_log      = beta0  + betaY[XY_sample[i]] + betaA[XA_sample[i]];
		
		bin_sim = bernoulli_logit_rng(theta_logit);
		pos_sim = bin_sim * lognormal_rng(mu_log, sigma[XY_sample[i]]);
					
		bin_sim_sum[XY_sample[i], XA_sample[i]] += bin_sim;
		pos_sim_sum[XY_sample[i], XA_sample[i]] += pos_sim;
	}
	
	// SIMULATE AGGREGATED OBSERVATIONAL DATA
	// FOR MODEL DIAGNOSTICS USING ESTIMATION AREAS
	for (i in 1:Y) {
		for (j in 1:A) {
	
			theta_logit = gamma0 + gammaY[i] + gammaA[j];
			mu_log      = beta0  + betaY[i] + betaA[j];
	
			bin_sim_agg[i, j] = binomial_rng(eff_estimate_sum[i, j], inv_logit(theta_logit));
			if (bin_sim_agg[i, j] > 0) 
				for (l in 1:bin_sim_agg[i, j])
					pos_sim_agg[i, j] += lognormal_rng(mu_log, sigma[i]);
		}
	}
	
	// SIMULATE OBSERVER DATA
	// AND COMMERCIAL CATCH PREDICTION USING
	// RESIDUAL EFFORT FROM SAMPLING AREAS
	for (i in 1:Y) {
		for (j in 1:A) {
		
    			theta_logit = gamma0 + gammaY[i] + gammaA[j];
    			mu_log      = beta0  + betaY[i] + betaA[j];
    		
    			bin_sim_obs[i, j] = binomial_rng(eff_sample_sum[i, j], inv_logit(theta_logit));
    			if (bin_sim_obs[i, j] > 0) 
			        for (l in 1:bin_sim_obs[i, j])
				        pos_sim_obs[i, j] += lognormal_rng(mu_log, sigma[i]);
    			
    			bin_sim_com[i, j] = binomial_rng(eff_resid_sum[i, j], inv_logit(theta_logit));
    			if (bin_sim_com[i, j] > 0) 
			        for (l in 1:bin_sim_com[i, j])
				        pos_sim_com[i, j] += lognormal_rng(mu_log, sigma[i]);
		}
	}
	
	// CALCULATE DISCREPANCIES
	{
		int denom = 0;
		
		for (i in 1:Y) {
			for (j in 1:A) {
			
				denom += 1;
								
				DBN[1] += bin_sum[i, j]     - bin_hat_sum[i, j];
				DBN[2] += bin_sim_sum[i, j] - bin_hat_sum[i, j];
				DBN[3] += bin_sim_agg[i, j] - bin_hat_sum[i, j];
				
				DLN[1] += pos_sum[i, j]     - pos_hat_sum[i, j];
				DLN[2] += pos_sim_sum[i, j] - pos_hat_sum[i, j];
				DLN[3] += pos_sim_agg[i, j] - pos_hat_sum[i, j];
			}
		}
	
		DBN[1] /= denom;
		DBN[2] /= denom;
		DBN[3] /= denom;
		DLN[1] /= denom;
		DLN[2] /= denom;
		DLN[3] /= denom;
	}
		
	// MODEL OUTPUT
	for (i in 1:Y) {
		for (j in 1:A) {
		
		    //predicted_catch[i, j] = pos_sim_obs[i, j] + pos_sim_com[i, j];
			predicted_catch[i, j] = pos_sum[i, j] + pos_sim_com[i, j];
			
			predicted_catch_sum[i] += predicted_catch[i, j];
		}
	}
	
	// PARAMETER SUMMARY STATISTICS 
	// FOR TRACE DIAGNOSTICS
	gamma_summary[1] = gamma0;
	gamma_summary[2] = vector_norm(gammaY);
	gamma_summary[3] = vector_norm(gammaA);
	beta_summary[1]  = beta0;
	beta_summary[2]  = vector_norm(betaY);
	beta_summary[3]  = vector_norm(betaA);
}

