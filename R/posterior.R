#'
#' @title Extract posterior density
#'
#' @description Takes as input a \code{stanfit} object from call to \code{sampling} and extracts the posterior or functions thereof
#' 
#' @param object   output from call to \code{rstan::sampling()}
#' @param pars     character vector of map parameter esimates to be extracted
#' @param dimnames list of dimension names for each parameter
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
        
        object <- lapply(object, function(x) { dimnames(x) <- dim.names; x })    
    } 
    
    if (!missing(fun)) {
        
        object <- switch(fun,
                         "median" = lapply(object, function(x) apply(x, 2:length(dim(x)), median)),
                         "mean"   = lapply(object, function(x) apply(x, 2:length(dim(x)), mean)),
                         object)
    }
    
    if (!missing(melt)) {
        if (melt) {
            object <- lapply(object, function(x) reshape2::melt(x))
        }
    }
    
    return(object)
}

