// regression of 
// by-catch data 

// NOTATION
//
// N: number of data records
// A: number of areas
// Y: number of years
// G: fishing method
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
}
data {

	// DIMENSIONS
	int N[2]; 
	int Y;
	int A;
	int G;
	int V;
	
	// LOOK-UP VECTORS
	int XA_sample[N[1]];
	int XY_sample[N[1]];
	int XG_sample[N[1]];
	int XV_sample[N[1]];
	int XA_predict[N[2]];
	int XY_predict[N[2]];
	int XG_predict[N[2]];
	int XV_predict[N[2]];
	
	// OBSERVER DATA
	real pos[N[1]]; 
	int  bin[N[1]];
	int  eff_sample[N[1]];
	
	// COMMERCIAL DATA
	int eff_predict[N[2]];
	
	// LOGICALS
	int fit_interaction;
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
	int XG_sample_nz[N_nz];
	int XV_sample_nz[N_nz];
	
	// aggregated data
	real pos_sum[Y, A, G, V];
	int  bin_sum[Y, A, G, V];
	int  eff_sample_sum[Y, A, G, V];
	int  eff_predict_sum[Y, A, G, V];
	int  eff_resid_sum[Y, A, G, V];
	
	// estimation of interaction
	// terms
	real tau = fit_interaction ? 1.0 : 0.0001;
	
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
		for (j in 1:A) {
			for (k in 1:G) {
				for (l in 1:V) {
		
					bin_sum[i, j, k, l] = 0;
					pos_sum[i, j, k, l] = 0.0;
					
					eff_sample_sum[i, j, k, l]  = 0;
					eff_predict_sum[i, j, k, l] = 0;
				}
			}
		}
	}
	
	for (i in 1:N[1]) {
	
		bin_sum[XY_sample[i], XA_sample[i], XG_sample[i], XV_sample[i]] += bin[i];
		pos_sum[XY_sample[i], XA_sample[i], XG_sample[i], XV_sample[i]] += pos[i];
		
		// observer sampling effort
		eff_sample_sum[XY_sample[i], XA_sample[i], XG_sample[i], XV_sample[i]] += eff_sample[i];
	}
	
	for (i in 1:N[2]) {
			
		// commercial effort
		eff_predict_sum[XY_predict[i], XA_predict[i], XG_predict[i],, XV_sample[i]] += eff_predict[i];
	}
	
	for (i in 1:Y) {
		for (j in 1:A) {
			for (k in 1:G) {
				for (l in 1:V) {
		
					// residual (unobserved) commercial effort
					eff_resid_sum[i, j, k, l] = max(eff_predict_sum[i, j, k, l] - eff_sample_sum[i, j, k, l], 0);
				}
			}
		}
	}
	
	{
		int loc = 1;
		for (i in 1:N[1]) {
			if (bin[i]) {
			
				// positive catch record
				pos_nz[loc]       = pos[i];
				
				// look-up vectors
				XY_sample_nz[loc] = XY_sample[i];
				XA_sample_nz[loc] = XA_sample[i];
				XG_sample_nz[loc] = XG_sample[i];
				XV_sample_nz[loc] = XV_sample[i];
				
				loc += 1;
			}
		}
	}
	
	// TO DO?: for each year and area create a positive record look up value
	// such that i, j vector = 1 but is zero otherwise (to allow more 
	// efficient access to pos vector). 
	

}
parameters {

	// binomial catch coefficients
	real      gamma0;
	vector[Y] gammaY;
	vector[A] gammaA;
	vector[G] gammaG;
	vector[V] gammaV;
	
	// positive catch coefficients
	real      beta0;
	vector[Y] betaY;
	vector[A] betaA;
	vector[G] betaG;
	vector[V] betaV;
	vector[A] betaI[Y];
	
	// observation error
	real<lower=0> sigma[Y];

}
transformed parameters {
	
}
model {
	
	real theta_logit;
	real mu_log[N_nz];
	
	// binomial model using aggregated
	// sufficient statistic
	for (i in 1:Y) {
		for (j in 1:A) {
			for (k in 1:G) {
				for (l in 1:V) {
	
					theta_logit = gamma0 + gammaY[i] + gammaA[j] + gammaG[k] + gammaV[l];

					bin_sum[i, j, k, l] ~ binomial_logit(eff_sample_sum[i, j, k, l], theta_logit);
				}
			}
		}
	}
	
	// positive catch log-normal model
	if (fit_interaction) {
	    
    	for(i in 1:N_nz) {
    	
    		mu_log[i] = beta0 + betaY[XY_sample_nz[i]] + betaA[XA_sample_nz[i]] + betaG[XG_sample_nz[i]] + betaV[XV_sample_nz[i]] + betaI[XY_sample_nz[i], XA_sample_nz[i]];
    	} 
	} else {
	    
	    for(i in 1:N_nz) {
    	
    		mu_log[i] = beta0 + betaY[XY_sample_nz[i]] + betaA[XA_sample_nz[i]] + betaG[XG_sample_nz[i]] + betaV[XV_sample_nz[i]];
    	}
	}
	
	pos_nz ~ lognormal(mu_log, sigma[XY_sample_nz]);
	
	// augmented priors for
	// intercept parameters
	gamma0 ~ normal(loc_prior[1], 1);
	beta0  ~ normal(loc_prior[2], 1);
	
	// logistic regression priors
	gammaY ~ normal(0, 1);
	gammaA ~ normal(0, 1);
	gammaG ~ normal(0, 1);
	gammaV ~ normal(0, 1);
	
	// log-normal regression priors
	betaY ~ normal(0, 1);
	betaA ~ normal(0, 1);
	betaG ~ normal(0, 1);
	betaV ~ normal(0, 1);
	for (i in 1:Y)
		betaI[i] ~ normal(0, tau);
	
	// error terms
	sigma ~ normal(0,1);

}
generated quantities {
	
	// predictands
	real theta_logit;
	real mu_log;
		
	// expected values
	real bin_hat[N[1]];
	real pos_hat[N[1]];
	real bin_hat_sum[Y, A, G, V];
	real pos_hat_sum[Y, A, G, V];
	
	// simulated data
	int  bin_sim[N[1]];
	real pos_sim[N[1]];
	int  bin_sim_sum[Y, A, G, V];
	real pos_sim_sum[Y, A, G, V];
	
	int  bin_sim_agg[Y, A, G, V];
	real pos_sim_agg[Y, A, G, V];
	
	int  bin_sim_com[Y, A, G, V];
	real pos_sim_com[Y, A, G, V];
	
	// discrepancy measures
	real D_bin[2, Y, A, G, V];
	real D_pos[2, Y, A, G, V];
	
	// model output
	real predicted_catch[Y, A];
	
	// INITIALISE TO ZERO
	for (i in 1:Y) {
		for (j in 1:A) {
			for (k in 1:G) {
				for (l in 1:V) {
			
					bin_hat_sum[i, j, k, l] = 0;
					pos_hat_sum[i, j, k, l] = 0.0;
					
					bin_sim_sum[i, j, k, l] = 0;
					pos_sim_sum[i, j, k, l] = 0.0;
					
					D_bin[1, i, j, k, l] = 0.0;
					D_bin[2, i, j, k, l] = 0.0;
					
					D_pos[1, i, j, k, l] = 0.0;
					D_pos[2, i, j, k, l] = 0.0;
				}
			}
		}
	}
	
	// CALCULATE EXPECTED VALUES
	// BY RECORD AND SUM BY
	// YEAR AND AREA
	for(i in 1:N[1]) {
			
		theta_logit = gamma0 + gammaY[XY_sample[i]] + gammaA[XA_sample[i]] + gammaG[XG_sample[i]] + gammaV[XV_sample[i]];
		mu_log      = beta0 + betaY[XY_sample[i]] + betaA[XA_sample[i]] + betaG[XG_sample[i]] + betaV[XV_sample[i]] + betaI[XY_sample[i], XA_sample[i]];
		
		bin_hat[i] = inv_logit(theta_logit);
		pos_hat[i] = bin[i] > 0 ? exp(mu_log + square(sigma[XY_sample[i]]) / 2) : 0.0;
		
		bin_hat_sum[XY_sample[i], XA_sample[i], XG_sample[i], XV_sample[i]] += bin_hat[i];
		pos_hat_sum[XY_sample[i], XA_sample[i], XG_sample[i], XV_sample[i]] += pos_hat[i];
	}
	
	// SIMULATE OBSERVATIONAL DATA 
	// BY RECORD AND SUM BY YEAR
	// AND AREA
	for(i in 1:N[1]) {
	
		theta_logit = gamma0 + gammaY[XY_sample[i]] + gammaA[XA_sample[i]] + gammaG[XG_sample[i]] + gammaV[XV_sample[i]];
		mu_log      = beta0 + betaY[XY_sample[i]] + betaA[XA_sample[i]] + betaG[XG_sample[i]] + betaV[XV_sample[i]] + betaI[XY_sample[i], XA_sample[i]];
		
		bin_sim[i] = bernoulli_logit_rng(theta_logit);
		pos_sim[i] = bin_sim[i] > 0 ? lognormal_rng(mu_log, sigma[XY_sample[i]]) : 0.0;
			
		bin_sim_sum[XY_sample[i], XA_sample[i], XG_sample[i], XV_sample[i]] += bin_sim[i];
		pos_sim_sum[XY_sample[i], XA_sample[i], XG_sample[i], XV_sample[i]] += pos_sim[i];
	}
	
	// SIMULATE AGGREGATED OBSERVATIONAL DATA
	// AND COMMERCIAL CATCH PREDICTION USING
	// RESIDUAL EFFORT
	for (i in 1:Y) {
		for (j in 1:A) {
			for (k in 1:G) {
				for (l in 1:V) {
			
					theta_logit = gamma0 + gammaY[i] + gammaA[j] + gammaG[k] + gammaV[l];
					mu_log      = beta0  + betaY[i] + betaA[j] + betaG[k] + betaV[l] + betaI[i, j];
				
					bin_sim_agg[i, j, k, l] = binomial_rng(eff_sample_sum[i, j, k, l], inv_logit(theta_logit));
					pos_sim_agg[i, j, k, l] = bin_sim_agg[i, j, k, l] > 0 ? lognormal_rng(mu_log + log(bin_sim_agg[i, j, k, l]), sigma[i]) : 0.0;
					
					bin_sim_com[i, j, k, l] = binomial_rng(eff_resid_sum[i, j, k, l], inv_logit(theta_logit));
					pos_sim_com[i, j, k, l] = bin_sim_com[i, j, k, l] > 0 ? lognormal_rng(mu_log + log(bin_sim_com[i, j, k, l]), sigma[i]) : 0.0;
				}
			}
		}
	}
	
	// CALCULATE DISCREPANCIES
	for (i in 1:Y) {
		for (j in 1:A) {
			for (k in 1:G) {
				for (l in 1:V) {
		
					D_bin[1, i, j, k, l] += pow(pow(bin_sum[i, j, k, l], 0.5)     - pow(bin_hat_sum[i, j, k, l], 0.5), 2.0);
					D_bin[2, i, j, k, l] += pow(pow(bin_sim_sum[i, j, k, l], 0.5) - pow(bin_hat_sum[i, j, k, l], 0.5), 2.0);
					
					D_pos[1, i, j, k, l] += pow(pow(pos_sum[i, j, k, l], 0.5)     - pow(pos_hat_sum[i, j, k, l], 0.5), 2.0);
					D_pos[2, i, j, k, l] += pow(pow(pos_sim_sum[i, j, k, l], 0.5) - pow(pos_hat_sum[i, j, k, l], 0.5), 2.0);
				}
			}
		}
	}
	
	// MODEL OUTPUT
	for (i in 1:Y) {
		for (j in 1:A) {
		
			predicted_catch[i, j] = 0.0;
			
			for (k in 1:G) {
				for (l in 1:V) {
					
					predicted_catch[i, j] += pos_sim_agg[i, j, k, l] + pos_sim_com[i, j, k, l];
				}
			}
		}
	}
}

