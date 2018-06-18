#'
#' @title Extract maximum a posterior fit
#'
#' @description Takes as input a list from call to \code{optimizing} and extracts MAP parameter estimates
#' 
#' @param object output from call to \code{rstan::optimizing()}
#' @param pars   character vector of map parameter esimates to be extracted
#' @param dims   list of dimensions for each parameter
#'
#' @export
"map" <- function(object, pars, ...) UseMethod("map")
#' @rdname map
#' @export
"map.list" <- function(object, pars, dims, ...) {
    
    object <- object[['par']]
    
    if (missing(pars)) {
        pars <- names(object)
    }
    
    out <- vector('list', length(pars))
    names(out) <- pars
    
    for (i in 1:length(out)) {
        
        m <- regexpr(pars[i], names(object), fixed = TRUE)
        m <- object[m > 0]
        
        if (all(dims[[i]] > 0)) { out[[i]] <- structure(as.numeric(m), dim = dims[[i]])
        } else out[[i]] <- as.numeric(m)
    }
    
    return(out)
}
