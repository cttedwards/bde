#'
#' @title Extract posterior density
#'
#' @description Takes as input a \code{stanfit} object from call to \code{sampling} and extracts the posterior or functions thereof.
#' 
#' @param object    output from call to \code{rstan::sampling()}.
#' @param pars      character vector of posterior parameter samples to be extracted.
#' @param dim.names optional list of dimension names for each parameter. If only a single list entry is given it is applied to all parameters.
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
            
            object <- lapply(object, function(x) { dimnames(x) <- dim.names; x })   
            
        } else {
            
            for (i in 1:length(dim.names)) {
                
                dimnames(object[[names(dim.names)[i]]]) <- dim.names[[i]]
                
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

