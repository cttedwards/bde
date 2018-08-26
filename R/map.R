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
    
	out <- vector('list', 2)
	names(out) <- c('estimate', 'sd')
	
	if ("theta_tilde" %in% names(object)) { SD <- TRUE
	} else { SD <- FALSE }
	    
    if (missing(pars)) {
        pars <- names(object[['par']])
    }
    
    out[['estimate']] <- vector('list', length(pars))
    names(out[['estimate']]) <- pars
	
	if (SD) {
		out[['sd']] <- vector('list', length(pars))
		names(out[['sd']]) <- pars
	}
    
    for (i in 1:length(pars)) {
        
        m <- regexpr(pars[i], names(object[['par']]), fixed = TRUE)
        m <- object[['par']][m > 0]
        
        if (all(dims[[i]] > 0)) { out[['estimate']][[i]] <- structure(as.numeric(m), dim = dims[[i]])
        } else out[['estimate']][[i]] <- as.numeric(m)
		
		if (SD) {
			
			m <- regexpr(pars[i], colnames(object[['theta_tilde']]), fixed = TRUE)
			m <- object[['theta_tilde']][, m > 0]
        
			if (all(dims[[i]] > 0)) { 
			    m <- apply(as.numeric(m), 2, sd)
			    out[['sd']][[i]] <- structure(m, dim = dims[[i]])
			} else {
			    m <- sd(as.numeric(m))
			    out[['sd']][[i]] <- m
			}
		}
    }
    
    return(out)
}
