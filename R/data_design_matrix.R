#'
#' @title Create look-up vectors for cofactors
#' 
#' @description Creates a look-up integer vector with which to locate different cofactor levels within a data.frame. Analogous to a design matrix X, but more efficient.
#' 
#' @param data input data.frame
#' @param X.dims vector of cofactor dimensions from call to \code{data_dims}
#' @param X.dimnames vector of cofactor names from call to \code{data_dims}
#' 
#' @export
data_design_matrix <- function(data, X.dims, X.dimnames) {
    
    # check
    stopifnot(length(X.dims) == length(X.dimnames))
    stopifnot(all(names(X.dims) == names(X.dimnames)))
    
    # names
    cofac.names <- names(X.dims)
    
    # look-up (design matrix)
    X <- structure(vector('list', length(cofac.names)), names = cofac.names)
    
    for (i in cofac.names) {
        
        X[[i]] <- match(data[, i], X.dimnames[[i]])    
        
    }
    
    return(X)
}
