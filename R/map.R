#'
#' @title Extract maximum a posterior fit
#'
#' @description Takes as input a list from call to \code{optimizing} and extracts MAP parameter estimates.
#' 
#' @details A call to \code{optimizing} returns maximum a posteriori parameter estimates, which are equivalent to the maximum penalised likelihood or maximum posterior density. If 
#' the call was made with \code{optimizing(..., draws = <n>)} then it will draw \code{n} samples from the assumed multivariate normal posterior density on the untransformed scale, where \code{n} is
#' a large number (>2000). These are then used by the \code{map()} function to calculate standard errors and confidence intervals.
#' 
#' @param object    output from call to \code{rstan::optimizing()}
#' @param pars      character vector of map parameter esimates to be extracted
#' @param dims      named list of dimensions for each parameter. If only a single list entry is given it is applied to all parameters.
#' @param dim.names optional list of dimension names for each parameter. If only a single list entry is given it is applied to all parameters.
#'
#' @export
"map" <- function(object, pars, ...) UseMethod("map")
#' @rdname map
#' @export
"map.list" <- function(object, pars, dims, dim.names, ...) {
    
	out <- vector('list', 3)
	names(out) <- c('estimate', 'sd', 'quantiles')
	
	if ("theta_tilde" %in% names(object)) { ERROR <- TRUE
	} else { ERROR <- FALSE }
	    
    if (missing(pars)) {
        pars <- names(object[['par']])
    }
    
    out[['estimate']] <- vector('list', length(pars))
    names(out[['estimate']]) <- pars
	
	if (ERROR) {
	    
		out[['sd']] <- vector('list', length(pars))
		names(out[['sd']]) <- pars
		
		out[['quantiles']] <- vector('list', length(pars))
		names(out[['quantiles']]) <- pars
	}
    
    for (i in 1:length(pars)) {
        
        if (length(dims) > 1) {
            ds <- dims[[pars[i]]]
        } else ds <- dims[[1]]
        
        if (!missing(dim.names)) {
            if (length(dim.names) > 1) {
                dn <- dim.names[[pars[i]]]
            } else dn <- dim.names[[1]]
        } else dn <- NULL
        
        m <- regexpr(pars[i], names(object[['par']]), fixed = TRUE)
        m <- object[['par']][m > 0]
        
        if (all(ds > 0)) { out[['estimate']][[i]] <- structure(as.numeric(m), dim = ds, names = NULL, dimnames = dn)
        } else out[['estimate']][[i]] <- as.numeric(m)
		
		if (ERROR) {
			
			m <- regexpr(pars[i], colnames(object[['theta_tilde']]), fixed = TRUE)
			m <- object[['theta_tilde']][, m > 0]
        
			if (all(ds > 0)) { 
			    
			    m1 <- apply(as.matrix(m), 2, sd)
			    out[['sd']][[i]] <- structure(m1, dim = ds, names = NULL, dimnames = dn)
			    
			    ds <- c(3, ds)
			    dn <- c(list(quantile = c("50%", "2.5%", "97.5%")), dn)
			    m2 <- apply(as.matrix(m), 2, quantile, c(0.5, 0.025, 0.975))
			    out[['quantiles']][[i]] <- structure(m2, dim = ds, names = NULL, dimnames = dn)
			    
			} else {
			    
			    m1 <- sd(as.numeric(m))
			    out[['sd']][[i]] <- m1
			    
			    m2 <- quantile(as.numeric(m), c(0.5, 0.025, 0.975))
			    out[['quantiles']][[i]] <- structure(m2, dim = 3, names = c("50%", "2.5%", "97.5%"))
			}
		}
    }
    
    return(out)
}
