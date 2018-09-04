#'
#' @title Extract posterior density
#'
#' @description Takes as input a \code{stanfit} object from call to \code{sampling} and extracts the posterior or functions thereof.
#' 
#' @param object    output from call to \code{rstan::sampling()}.
#' @param pars      character vector of posterior parameter samples to be extracted.
#' @param dim.names optional list of named lists containing dimension names for each parameter, including the number of iterations over the first dimension. If only a single \code{list(list())} entry is given it is applied to all parameters. Not all parameters need to be provided with dimension names.
#' @param melt      logical value indicating whether output arrays should be converted to long format using \code{rehape2::melt.array()}
#' @param fun       one of either "mean" or "median", which will be calculated across iterations if supplied.
#' 
#' @return Returns a list of posterior samples for each parameter. If \code{melt = TRUE} then these are returned as data.frames, otherwise they are arrays. If \code{fun} is specificed then the output is summarised across iterations.
#'
#' @export
"posterior" <- function(object, pars, ...) UseMethod("posterior")
#' @rdname posterior
#' @export
"posterior.stanfit" <- function(object, pars, dim.names, melt, fun, ...) {

    if (missing(pars)) {
        pars <- object@model_pars
    }
    
    object <- rstan::extract(object, pars = pars, permuted = TRUE, inc_warmup = FALSE)
    
    if (!missing(dim.names)) {
        
        if (length(dim.names) == 1) {
            
            object <- lapply(object, function(x) { dimnames(x) <- dim.names[[1]]; x })   
            
        } else {
            
            for (i in 1:length(pars)) {
                
                dimnames(object[[pars[i]]]) <- dim.names[[pars[i]]]
                
            }
        }
    } 
    
    if (!missing(fun)) {
        
        object <- switch(fun,
                         "median"    = lapply(object, function(x) apply(x, 2:length(dim(x)), median)),
                         "mean"      = lapply(object, function(x) apply(x, 2:length(dim(x)), mean)),
                         "quantiles" = lapply(object, function(x) apply(x, 2:length(dim(x)), quantile, c(0.5, 0.025, 0.975))),
                         object)
    }
    
    if (!missing(melt)) {
        if (melt) {
            object <- lapply(object, function(x) reshape2::melt(x))
        }
    }
    
    return(object)
}

