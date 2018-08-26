


library(rstan)

cat(  
    "
	data{
		int<lower=1> n;
		vector[n] x;
		}	
	parameters{
		real mu_log;
		real<lower=0> sigma;
		}	
	model{
		x ~ lognormal(mu_log, sigma);
	}
    generated quantities {
        
        vector[n] x_sim;
        real x_sim_sum;
        real x_sim_agg;

        for (i in 1:n) {
            x_sim[i] = lognormal_rng(mu_log, sigma);
        }

        x_sim_sum = sum(x_sim);
        x_sim_agg = lognormal_rng(mu_log + log(n), sigma);

    }
	\n"	
    , file = "modelLogNormal.stan")

mystanmodel = stan_model("modelLogNormal.stan")


### generate data
set.seed(100)
n = 200
mu_log = 2.0
sd_log = 0.2
x = rlnorm(n, mu_log, sd_log)

### sampling function in rstan
fit = sampling(mystanmodel, init =  function() list(mu_log = mu_log, sigma = sd_log), chains = 1, iter = 1000)

# compare model estimates with data
mean(log(x)); summary(fit)$summary["mu_log", "mean"]
sd(log(x));   summary(fit)$summary["sigma", "mean"]

# compare simulated values with data
x_sim <- extract(fit, "x_sim")[[1]]

# distribution of mean and sd values from simulated data
windows(); par(mfrow = c(2, 2))
hist(apply(x_sim, 1, mean))
abline(v = mean(x), col = 2)
hist(apply(x_sim, 1, sd))
abline(v = sd(x), col = 2)

# distribution of summed values
x_sim_sum <- extract(fit, "x_sim_sum")[[1]]
x_sim_agg <- extract(fit, "x_sim_agg")[[1]]

plot(x_sim_agg, x_sim_sum, xlim = range(x_sim_sum, x_sim_agg), ylim = range(x_sim_sum, x_sim_agg))
abline(0, 1, col = 2)
abline(v = sum(x), h = sum(x), col = 4)







