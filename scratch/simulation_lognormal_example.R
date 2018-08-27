


library(rstan)

cat(  
    "
    functions {

        real sigma_lnsum(real mu_log, real sigma_log, int n) {

            return sqrt(log((exp(square(sigma_log)) - 1) / square(n) + 1));
        }

        real mu_lnsum(real mu_log, real sigma_log, int n) {

            real sigma = sigma_lnsum(mu_log, sigma_log, n);

            return log(n * exp(mu_log)) + square(sigma_log) / 2 - square(sigma) / 2;
        }

    }
	data{
		int<lower=1> n;
		vector[n] x;
	}	
    transformed data {

        real x_sum = sum(x);
    }
	parameters{
		real mu_log;
		real<lower=0> sigma_log;
		}	
	model{
		x ~ lognormal(mu_log, sigma_log);
	}
    generated quantities {
        
        vector[n] x_sim;
        vector[n] x_hat;

        real x_sim_sum;
        real x_sim_agg;

        real x_hat_sum;
        real x_hat_agg;

        real sigma_log_sum = sigma_lnsum(mu_log, sigma_log, n);
        real mu_log_sum    = mu_lnsum(mu_log, sigma_log, n);

        real D1[2] = { 0.0, 0.0 };
        real D2[2] = { 0.0, 0.0 };

        for (i in 1:n) {
            x_hat[i] = exp(mu_log + square(sigma_log) / 2);
            x_sim[i] = lognormal_rng(mu_log, sigma_log);
        }

        x_sim_sum = sum(x_sim);
        x_sim_agg = lognormal_rng(mu_log_sum, sigma_log_sum);

        x_hat_sum = sum(x_hat);
        x_hat_agg = exp(mu_log_sum + square(sigma_log_sum) / 2);

        // discrepancy for sum of simulations
        D1[1] = x_sum     - x_hat_sum;
        D1[2] = x_sim_sum - x_hat_sum;

        // discrepancy for simulation of sums
        D2[1] = x_sum     - x_hat_agg;
        D2[2] = x_sim_agg - x_hat_agg;

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
sd(log(x));   summary(fit)$summary["sigma_log", "mean"]

# compare simulated values with data
x_sim <- extract(fit, "x_sim")[[1]]

# distribution of mean and sd values from simulated data
windows(); par(mfrow = c(1, 2))
hist(apply(x_sim, 1, mean), probability = TRUE, main = expression(paste("Estimation of ", mu)), xlab = expression(hat(mu)))
abline(v = mean(x), col = 2)
hist(apply(x_sim, 1, sd), probability = TRUE, main = expression(paste("Estimation of ", sigma)), xlab = expression(hat(sigma)))
abline(v = sd(x), col = 2)

# distribution of summed values
windows(); par(mfrow = c(2, 2))

x_sim_sum <- extract(fit, "x_sim_sum")[[1]]
x_sim_agg <- extract(fit, "x_sim_agg")[[1]]

x_hat_sum <- extract(fit, "x_hat_sum")[[1]]
x_hat_agg <- extract(fit, "x_hat_agg")[[1]]

xlims <- range(x_sim_sum, x_sim_agg, x_hat_sum, x_hat_agg)
ylims <- range(x_sim_sum, x_sim_agg, x_hat_sum, x_hat_agg)

plot(x_hat_agg, x_sim_sum, xlim = xlims, ylim = ylims, xlab = expression(paste(E(sum(x)), "=", E(z))), ylab = expression(sum(tilde(x))))
abline(0, 1, col = 2)
abline(v = sum(x), h = sum(x), col = 4)

plot(x_hat_agg, x_sim_agg, xlim = xlims, ylim = ylims, xlab = expression(paste(E(sum(x)), "=", E(z))), ylab = expression(tilde(z)))
abline(0, 1, col = 2)
abline(v = sum(x), h = sum(x), col = 4)

plot(x_hat_sum, x_sim_sum, xlim = xlims, ylim = ylims, xlab = expression(sum(E(x))), ylab = expression(sum(tilde(x))))
abline(0, 1, col = 2)
abline(v = sum(x), h = sum(x), col = 4)

plot(x_hat_sum, x_sim_agg, xlim = xlims, ylim = ylims, xlab = expression(sum(E(x))), ylab = expression(tilde(z)))
abline(0, 1, col = 2)
abline(v = sum(x), h = sum(x), col = 4)

# discrepancy values
windows(); par(mfrow = c(1, 2))

D1 <- extract(fit, "D1")[[1]]
D2 <- extract(fit, "D2")[[1]]

xlims <- range(D1, D2)
ylims <- range(D1, D2)

plot(D1[,1], D1[,2], xlim = xlims, ylim = ylims, xlab = expression(sum(x) - sum(E(x))), ylab = expression(sum(tilde(x)) - sum(E(x)))); mtext("Sum of simulations\n")
abline(0, 1, col = 2)
legend("bottomright", paste("p = ", mean(D1[,1] > D1[,2])), bty = "n")
plot(D2[,1], D2[,2], xlim = xlims, ylim = ylims, xlab = expression(z - E(z)), ylab = expression(tilde(z) - E(z))); mtext("Simulation of sums\n")
abline(0, 1, col = 2)
legend("bottomright", paste("p = ", mean(D2[,1] > D2[,2])), bty = "n")

# clean up
file.remove("modelLogNormal.stan")
