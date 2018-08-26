
library(rstan)

cat(  
	"
	data{
		int<lower=1> n;
		vector[n] x;
		}	
	parameters{
		real mu;
		real<lower=0> sigma;
		}	
	model{
		x ~ normal(mu, sigma);
		}
	\n"	
	, file = "modelNormal.stan")

mystanmodel = stan_model("modelNormal.stan")


### generate data
set.seed(100)
n = 200
x = rnorm(n, 0, 2)
sdmle = sqrt((n-1)/n)*sd(x)

### exact mles and sd of mles
exact_mles = c(mean(x), sdmle)
exact_sd_mles = c(sd(x)/sqrt(n), sdmle/sqrt(n) * sqrt( (n-1- 2*(gamma(n/2)/gamma((n-1)/2))^2) ) )

### optim function in R (stats)
loglike = function(x, par) -sum(log(dnorm(x,par[1],par[2])))
lower = c(-Inf, 0)
upper = c(Inf, Inf)
fit1 = optim(par=c(mean(x), sd(x)), loglike, x=x , hessian = TRUE, method = "L-BFGS-B", lower=lower, upper=upper)
optim_mles= fit1$par;
optim_sd_mles = sqrt(diag(abs(solve(-(fit1$hessian)))));

### optimizing function in rstan
fit2 = optimizing(mystanmodel,hessian=TRUE,data=list(n=n,x=x), init=list(mu=mean(x),sigma=sd(x)), draws = 20000)
optimizing_mles = fit2$par;
optimizing_sd_mles = sqrt(diag(cov(fit2$theta_tilde)))

### sampling function in rstan
fit3 = sampling(mystanmodel, init =  list(list(mu = mean(x), sigma = sd(x))), chains = 1, iter = 5000)
sampling_parhat = summary(fit3)$summary[,"mean"][1:2]
sampling_sdparhat = summary(fit3)$summary[,"sd"][1:2]

# 95% CIs
optimzing_par95CI = rbind(optimizing_mles - 1.96 * optimizing_sd_mles, optimizing_mles + 1.96 * optimizing_sd_mles)
sampling_par95CI  = rbind(summary(fit3)$summary[,"2.5%"][1:2], summary(fit3)$summary[,"97.5%"][1:2])


#### all the results

exact_mles
optim_mles
optimizing_mles
sampling_parhat

exact_sd_mles
optim_sd_mles
optimizing_sd_mles
sampling_sdparhat


# check map function
map(fit2, pars = c("mu", "sigma"), dims = c(0, 0))




