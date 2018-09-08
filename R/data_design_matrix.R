#'
#' @title Create look-up vectors for cofactors
#' 
#' @description Creates a look-up integer vector with which to locate different cofactor levels within a data.frame. Analogous to a design matrix X, but more efficient.
#' 
#' @param data input data.frame
#' @param X.dims vector of cofactor dimensions from call to \code{data_dims}
#' @param X.dimnames vector of cofactor names from call to \code{data_dims}
#' 
#' @examples
#' # example data
#' dfr <- expand.grid(year = c(1, 2), area = c("N", "S", "E", "W"))
#' 
#' # coerce to factors
#' dfr <- data_prep(dfr, cofac.names = list(from = c("year", "area"), to = c("Y", "A")))
#' 
#' # get dimensions and dimension names
#' xdims     <- data_dims(dfr, cofac.names = c("Y", "A"), dims = TRUE)
#' xdimnames <- data_dims(dfr, cofac.names = c("Y", "A"), dimnames = TRUE)
#' 
#' # create look-up vectors
#' data_design_matrix(dfr, xdims, xdimnames)
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
        
        if (i %in% colnames(data))
            X[[i]] <- match(data[, i], X.dimnames[[i]])    
        
    }
    
    return(X)
}
