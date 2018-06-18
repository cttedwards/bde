// regression of 
// by-catch data 

// NOTATION
//
// N: number of data records
// A: number of areas
// Y: number of years
// P: periods
// M: meal processing plant
// X: design matrices
// gamma: bernoulli regression coefficients
// beta: log-normal regression coefficients


data {

	// DIMENSIONS
	int N[2]; 
	int A;
	int Y;
	
	// LOOK-UP VECTORS
	int XA_sample[N[1]];
	int XY_sample[N[1]];
	int XA_predict[N[2]];
	int XY_predict[N[2]];
	
	// OBSERVER DATA
	real pos[N[1]]; 
	int  bin[N[1]];
	int  eff_sample[N[1]];
	
	// COMMERCIAL DATA
	int eff_predict[N[2]];
	
	// LOGICALS
	int fit_by_tow;
}
transformed data {

	// aggregated data
	real pos_sum[Y, A];
	int  bin_sum[Y, A];
	int  eff_sample_sum[Y, A];
	int  eff_predict_sum[Y, A];
	int  eff_resid_sum[Y, A];
	
	// estimation of interaction
	// terms
	real tau = fit_by_tow ? 1.0 : 0.0001;
	
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
		
			bin_sum[i, j] = 0;
			pos_sum[i, j] = 0.0;
			
			eff_sample_sum[i, j]  = 0;
			eff_predict_sum[i, j] = 0;
		}
	}
	
	for (i in 1:N[1]) {
	
		bin_sum[XY_sample[i], XA_sample[i]] += bin[i];
		pos_sum[XY_sample[i], XA_sample[i]] += pos[i];
		
		// observer sampling effort
		eff_sample_sum[XY_sample[i], XA_sample[i]] += eff_sample[i];
	}
	
	for (i in 1:N[2]) {
			
		// commercial effort
		eff_predict_sum[XY_predict[i], XA_predict[i]] += eff_predict[i];
	}
	
	for (i in 1:Y) {
		for (j in 1:A) {
		
			// residual (unobserved) commercial effort
			eff_resid_sum[i, j] = max(eff_predict_sum[i, j] - eff_sample_sum[i, j], 0);
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
	vector[A] betaI[Y];
	
	// observation error
	real<lower=0> sigma[Y];

}
transformed parameters {
	
}
model {
	
	if (fit_by_tow) {
	
		real theta_logit;
		real mu_log;
	
		// likelihood
		for(i in 1:N[1]) {
		
			theta_logit = gamma0 + gammaY[XY_sample[i]] + gammaA[XA_sample[i]];
			mu_log      = beta0 + betaY[XY_sample[i]] + betaA[XA_sample[i]] + betaI[XY_sample[i], XA_sample[i]];
		
			bin[i] ~ bernoulli_logit(theta_logit);
			if (bin[i] > 0) {
				pos[i] ~ lognormal(mu_log, sigma[XY_sample[i]]);
			}
		}
	} else {
	
		real theta_logit;
		real mu_log;
	
		// likelihood
		for (i in 1:Y) {
			for (j in 1:A) {
		
				theta_logit = gamma0 + gammaY[i] + gammaA[j];
				mu_log      = beta0 + betaY[i] + betaA[j];
		
				bin_sum[i, j] ~ binomial_logit(eff_sample_sum[i, j], theta_logit);
				if (bin_sum[i, j] > 0) {
					pos_sum[i, j] ~ lognormal(mu_log + log(bin_sum[i, j]), sigma[XY_sample[i]]);
				}
			}
		}
	}
	
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
	real bin_hat_sum[Y, A];
	real pos_hat_sum[Y, A];
	
	// simulated data
	int  bin_sim[N[1]];
	real pos_sim[N[1]];
	int  bin_sim_sum[Y, A];
	real pos_sim_sum[Y, A];
	
	int  bin_sim_agg[Y, A];
	real pos_sim_agg[Y, A];
	
	int  bin_sim_com[Y, A];
	real pos_sim_com[Y, A];
	
	// discrepancy measures
	real D_bin[2, Y, A];
	real D_pos[2, Y, A];
	
	// model output
	real predicted_bycatch[Y, A];
	
	// INITIALISE TO ZERO
	for (i in 1:Y) {
		for (j in 1:A) {
		
			bin_hat_sum[i, j] = 0;
			pos_hat_sum[i, j] = 0.0;
			
			bin_sim_sum[i, j] = 0;
			pos_sim_sum[i, j] = 0.0;
			
			D_bin[1, i, j] = 0.0;
			D_bin[2, i, j] = 0.0;
			
			D_pos[1, i, j] = 0.0;
			D_pos[2, i, j] = 0.0;
		}
	}
	
	// CALCULATE EXPECTED VALUES
	// BY RECORD AND SUM BY
	// YEAR AND AREA
	for(i in 1:N[1]) {
			
		theta_logit = gamma0 + gammaY[XY_sample[i]] + gammaA[XA_sample[i]];
		mu_log      = beta0 + betaY[XY_sample[i]] + betaA[XA_sample[i]] + betaI[XY_sample[i], XA_sample[i]];
		
		bin_hat[i] = inv_logit(theta_logit);
		pos_hat[i] = bin[i] > 0 ? exp(mu_log + square(sigma[XY_sample[i]]) / 2) : 0.0;
		
		bin_hat_sum[XY_sample[i], XA_sample[i]] += bin_hat[i];
		pos_hat_sum[XY_sample[i], XA_sample[i]] += pos_hat[i];
	}
	
	// SIMULATE OBSERVATIONAL DATA 
	// BY RECORD AND SUM BY YEAR
	// AND AREA
	for(i in 1:N[1]) {
	
		theta_logit = gamma0 + gammaY[XY_sample[i]] + gammaA[XA_sample[i]];
		mu_log      = beta0  + betaY[XY_sample[i]] + betaA[XA_sample[i]] + betaI[XY_sample[i], XA_sample[i]];
		
		bin_sim[i] = bernoulli_logit_rng(theta_logit);
		pos_sim[i] = bin_sim[i] > 0 ? lognormal_rng(mu_log, sigma[XY_sample[i]]) : 0.0;
			
		bin_sim_sum[XY_sample[i], XA_sample[i]] += bin_sim[i];
		pos_sim_sum[XY_sample[i], XA_sample[i]] += pos_sim[i];
	}
	
	// SIMULATE AGGREGATED OBSERVATIONAL DATA
	// AND COMMERCIAL CATCH PREDICTION USING
	// RESIDUAL EFFORT
	for (i in 1:Y) {
		for (j in 1:A) {
		
			theta_logit = gamma0 + gammaY[i] + gammaA[j];
			mu_log      = beta0  + betaY[i] + betaA[j] + betaI[i, j];
		
			bin_sim_agg[i, j] = binomial_rng(eff_sample_sum[i, j], inv_logit(theta_logit));
			pos_sim_agg[i, j] = bin_sim_agg[i, j] > 0 ? lognormal_rng(mu_log + log(bin_sim_agg[i, j]), sigma[i]) : 0.0;
			
			bin_sim_com[i, j] = binomial_rng(eff_resid_sum[i, j], inv_logit(theta_logit));
			pos_sim_com[i, j] = bin_sim_com[i, j] > 0 ? lognormal_rng(mu_log + log(bin_sim_com[i, j]), sigma[i]) : 0.0;
			
		}
	}
	
	// CALCULATE DISCREPANCIES
	for (i in 1:Y) {
		for (j in 1:A) {
		
			D_bin[1, i, j] += pow(pow(bin_sum[i, j], 0.5)     - pow(bin_hat_sum[i, j], 0.5), 2.0);
			D_bin[2, i, j] += pow(pow(bin_sim_sum[i, j], 0.5) - pow(bin_hat_sum[i, j], 0.5), 2.0);
			
			D_pos[1, i, j] += pow(pow(pos_sum[i, j], 0.5)     - pow(pos_hat_sum[i, j], 0.5), 2.0);
			D_pos[2, i, j] += pow(pow(pos_sim_sum[i, j], 0.5) - pow(pos_hat_sum[i, j], 0.5), 2.0);
		}
	}
	
	// MODEL OUTPUT
	for (i in 1:Y) {
		for (j in 1:A) {
		
			predicted_bycatch[i, j] = pos_sim_agg[i, j] + pos_sim_com[i, j];
		}
	}
}

