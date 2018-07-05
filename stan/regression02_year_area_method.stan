// regression of 
// by-catch and discard 
// data using a two-part
// binomial/log-normal model 

// NOTATION
//
// N: number of data records (estimation; prediction)
// A: number of areas (sampling; estimation)
// Y: number of years
// M: number of fishing methods
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
	int A[2];
	int Y;
	int M;
	
	// LOOK-UP VECTORS
	int XY_sample[N[1]];
	int XA_sample[N[1]];
	int XA_estimate[N[1]];
	int XM_sample[N[1]];
	int XY_predict[N[2]];
	int XA_predict[N[2]];
	int XM_predict[N[2]];
	
	// ESTIMATION AREA
	// TO SAMPLING AREA
	// MAPPING VECTOR
	int area_to_area[A[1]];
	
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
	int XA_estimate_nz[N_nz];
	int XY_sample_nz[N_nz];
	int XM_sample_nz[N_nz];
	
	// aggregated data
	// by sampling area
	int  eff_sample_sum[Y, A[1]];
	int  eff_predict_sum[Y, A[1]];
	int  eff_resid_sum[Y, A[1]];
	
	// aggregated data by 
	// estimation area
	int  eff_estimate_sum[Y, A[2]];
	real pos_sum[Y, A[2]];
	int  bin_sum[Y, A[2]];
	
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
		for (j in 1:A[1]) {
			for (k in 1:M) {
		
				eff_sample_sum[i, j, k]  = 0;
				eff_predict_sum[i, j, k] = 0;
			}
		}
		
		// estimation areas
		for (j in 1:A[2]) {
			for (k in 1:M) {
		
				eff_estimate_sum[i, j, k]  = 0;
			
				bin_sum[i, j, k] = 0;
				pos_sum[i, j, k] = 0.0;
			}
		}
	}
	
	// sum data for sampling 
	// and estimation areas
	for (i in 1:N[1]) {
	
		// data
		bin_sum[XY_sample[i], XA_estimate[i], XM_sample[i]] += bin[i];
		pos_sum[XY_sample[i], XA_estimate[i], XM_sample[i]] += pos[i];
		
		// observer sampling effort
		eff_estimate_sum[XY_sample[i], XA_estimate[i], XM_sample[i]] += eff_sample[i];
		eff_sample_sum[XY_sample[i], XA_sample[i], XM_sample[i]] += eff_sample[i];
	}
	
	// sum commercial effort data
	// for sampling/prediction areas
	for (i in 1:N[2]) {
			
		// commercial effort
		eff_predict_sum[XY_predict[i], XA_predict[i], XM_predict[i]] += eff_predict[i];
	}
	
	// calculate residual 
	// commercial effort
	for (i in 1:Y) {
		for (j in 1:A[1]) {
			for (k in 1:M) {
		
				// residual (unobserved) commercial effort
				eff_resid_sum[i, j, k] = max(eff_predict_sum[i, j, k] - eff_sample_sum[i, j, k], 0);
			}
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
				XY_sample_nz[loc]   = XY_sample[i];
				XA_estimate_nz[loc] = XA_estimate[i];
				XM_sample_nz[loc]   = XM_sample[i];
				
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
	real         gamma0;
	vector[Y]    gammaY;
	vector[A[2]] gammaA;
	vector[M]    gammaM;
	
	// positive catch coefficients
	real         beta0;
	vector[Y]    betaY;
	vector[A[2]] betaA;
	vector[M]    betaM;
	vector[A[2]] betaI[Y];
	
	// observation error
	// per year
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
		for (j in 1:A[2]) {
			for (k in 1:M) {
	
				theta_logit = gamma0 + gammaY[i] + gammaA[j] + gammaM[k];
	
				bin_sum[i, j, k] ~ binomial_logit(eff_estimate_sum[i, j, k], theta_logit);
			}
		}
	}
	
	// positive catch log-normal model
    for(i in 1:N_nz) {
    
    	mu_log[i] = beta0 + betaY[XY_sample_nz[i]] + betaA[XA_estimate_nz[i]] + betaM[XM_sample_nz[i]] + betaI[XY_sample_nz[i], XA_estimate_nz[i]];
    } 
	
	pos_nz ~ lognormal(mu_log, sigma[XY_sample_nz]);
	
	// augmented priors for
	// intercept parameters
	gamma0 ~ normal(loc_prior[1], 1);
	beta0  ~ normal(loc_prior[2], 1);
	
	// logistic regression priors
	gammaY ~ normal(0, 1);
	gammaA ~ normal(0, 1);
	gammaM ~ normal(0, 1);
	
	// log-normal regression priors
	betaY ~ normal(0, 1);
	betaA ~ normal(0, 1);
	betaM ~ normal(0, 1);
	for (i in 1:Y)
		betaI[i] ~ normal(0, 1);
	
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
	real bin_hat_sum[Y, A[2], M];
	real pos_hat_sum[Y, A[2], M];
	
	// simulated data
	// by record
	int  bin_sim[N[1]];
	real pos_sim[N[1]];
	
	// simulated data
	// by estimation area
	int  bin_sim_sum[Y, A[2], M];
	real pos_sim_sum[Y, A[2], M];
	
	int  bin_sim_agg[Y, A[2], M];
	real pos_sim_agg[Y, A[2], M];
	
	// simulated data
	// by sampling area
	int  bin_sim_obs[Y, A[1], M];
	real pos_sim_obs[Y, A[1], M];
	
	int  bin_sim_com[Y, A[1], M];
	real pos_sim_com[Y, A[1], M];
	
	// discrepancy measure
	real D[2] = {0.0, 0.0};
	
	// model output
	real predicted_catch[Y, A[1]];
	
	// INITIALISE TO ZERO
	for (i in 1:Y) {
		for (j in 1:A[2]) {
			for (k in 1:M) {
		
				bin_hat_sum[i, j, k] = 0;
				pos_hat_sum[i, j, k] = 0.0;
				
				bin_sim_sum[i, j, k] = 0;
				pos_sim_sum[i, j, k] = 0.0;
			}
		}
	}
	
	// CALCULATE EXPECTED VALUES
	// BY RECORD AND SUM BY
	// YEAR AND AREA
	for(i in 1:N[1]) {
			
		theta_logit = gamma0 + gammaY[XY_sample[i]] + gammaA[XA_estimate[i]] + gammaM[XM_sample[i]];
		mu_log      = beta0 + betaY[XY_sample[i]] + betaA[XA_estimate[i]] + betaM[XM_sample[i]] + betaI[XY_sample[i], XA_estimate[i]];
		
		bin_hat[i] = inv_logit(theta_logit);
		pos_hat[i] = bin[i] > 0 ? exp(mu_log + square(sigma[XY_sample[i]]) / 2) : 0.0;
		
		bin_hat_sum[XY_sample[i], XA_estimate[i], XM_sample[i]] += bin_hat[i];
		pos_hat_sum[XY_sample[i], XA_estimate[i], XM_sample[i]] += pos_hat[i];
	}
	
	// SIMULATE OBSERVATIONAL DATA 
	// BY RECORD AND SUM BY YEAR
	// AND AREA
	for(i in 1:N[1]) {
	
		theta_logit = gamma0 + gammaY[XY_sample[i]] + gammaA[XA_estimate[i]] + gammaM[XM_sample[i]];
		mu_log      = beta0  + betaY[XY_sample[i]] + betaA[XA_estimate[i]] + betaM[XM_sample[i]] + betaI[XY_sample[i], XA_estimate[i]];
		
		bin_sim[i] = bernoulli_logit_rng(theta_logit);
		pos_sim[i] = bin_sim[i] > 0 ? lognormal_rng(mu_log, sigma[XY_sample[i]]) : 0.0;
			
		bin_sim_sum[XY_sample[i], XA_estimate[i], XM_sample[i]] += bin_sim[i];
		pos_sim_sum[XY_sample[i], XA_estimate[i], XM_sample[i]] += pos_sim[i];
	}
	
	// SIMULATE AGGREGATED OBSERVATIONAL DATA
	// FOR ESTIMATION AREAS
	for (i in 1:Y) {
		for (j in 1:A[2]) {
			for (k in 1:M) {
		
				theta_logit = gamma0 + gammaY[i] + gammaA[j] + gammaM[k];
				mu_log      = beta0  + betaY[i] + betaA[j] + betaM[k] + betaI[i, j];
		
				bin_sim_agg[i, j, k] = binomial_rng(eff_estimate_sum[i, j, k], inv_logit(theta_logit));
				pos_sim_agg[i, j, k] = bin_sim_agg[i, j, k] > 0 ? lognormal_rng(mu_log + log(bin_sim_agg[i, j, k]), sigma[i]) : 0.0;
			}
		}
	}
	
	// SIMULATE AGGREGATED OBSERVER DATA
	// AND COMMERCIAL CATCH PREDICTION USING
	// RESIDUAL EFFORT FROM SAMPLING AREAS
	for (i in 1:Y) {
		for (j in 1:A[1]) {
			for (k in 1:M) {
		
				theta_logit = gamma0 + gammaY[i] + gammaA[area_to_area[j]] + gammaM[k];
				mu_log      = beta0  + betaY[i] + betaA[area_to_area[j]] + betaM[k] + betaI[i, area_to_area[j]];
			
				bin_sim_obs[i, j, k] = binomial_rng(eff_sample_sum[i, j, k], inv_logit(theta_logit));
				pos_sim_obs[i, j, k] = bin_sim_obs[i, j, k] > 0 ? lognormal_rng(mu_log + log(bin_sim_obs[i, j, k]), sigma[i]) : 0.0;
				
				bin_sim_com[i, j, k] = binomial_rng(eff_resid_sum[i, j, k], inv_logit(theta_logit));
				pos_sim_com[i, j, k] = bin_sim_com[i, j, k] > 0 ? lognormal_rng(mu_log + log(bin_sim_com[i, j, k]), sigma[i]) : 0.0;
			}
			
		}
	}
	
	// CALCULATE DISCREPANCIES
	for (i in 1:Y) {
		for (j in 1:A[2]) {
			for (k in 1:M) {
					
				D[1] += pow(pos_sum[i, j, k]    - pos_hat_sum[i, j, k], 2.0); //pow(pow(pos_sum[i, j], 0.5)     - pow(pos_hat_sum[i, j], 0.5), 2.0);
				D[2] += pow(pos_sim_sum[i, j, k]- pos_hat_sum[i, j, k], 2.0); //pow(pow(pos_sim_sum[i, j], 0.5) - pow(pos_hat_sum[i, j], 0.5), 2.0);
			}
		}
	}
	
	// MODEL OUTPUT
	for (i in 1:Y) {
		for (j in 1:A[1]) {
		
			predicted_catch[i, j] = 0.0;
			
			 for (k in 1:M) {
		
				predicted_catch[i, j] += pos_sim_obs[i, j, k] + pos_sim_com[i, j, k];
			}
		}
	}
}

