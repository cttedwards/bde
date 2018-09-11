#'
#' @title Summary function for maximum a posteriori estiamte
#' 
#' @description Takes as argument a return from a call to \code{\link{map}} and summarises the mean, sd and quantile values.
#' 
#' @param object return from a call to \code{\link{map}}
#' @param pars   parameters to be summarised
#' 
#' @note This function will fail if the \code{draws} argument is not specified in the optimisation, i.e. \code{optimizing(..., draws = <n>)}, where \code{n} is a large number.
#' 
#' @include map.R
#' 
#' @method summary map
#' 
#' @examples 
#' require(rstan)
#' 
#' mdl <- "data{ int n; vector[n] x; } parameters{ real mu; real sigma;} model{ x ~ normal(mu, sigma);} \n"	
#' mdl <- stan_model(model_code = mdl)
#' n = 20
#' x = rnorm(n, 0, 2)
#' 
#' mdl.fit <- optimizing(mdl, data = list(n = n, x = x), init = list(mu = 0, sigma = 1), draws = 2000)
#' 
#' mdl.map <- map(mdl.fit, pars = c("mu", "sigma"), dims = list("mu" = 0, "sigma" = 0))
#' 
#' summary(mdl.map, pars = c("mu", "sigma"))
#'
#' @export
"summary.map" <- function(object, pars) {
    
    t(vapply(pars, function(x) c("mean" = object$estimate[[x]], 
                                 "sd"   = object$sd[[x]],
                                 object$quantiles[[x]][2],
                                 object$quantiles[[x]][1],
                                 object$quantiles[[x]][3]), numeric(5)))    
    
}

