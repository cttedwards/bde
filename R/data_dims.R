#'
#' @title Calculate dimensions and dimension names for regression cofactors
#' 
#' @param data input data frame
#' @param ... additional data frames
#' @param cofac.names character vector of names for which to return levels and dimensions
#' @param dims return dimensions?
#' @param dimnames return dimnames?
#'
#'
#' @export
data_dims <- function(data, ..., cofac.names, dims = TRUE, dimnames = !dims) {
    
    # combine data frames
    data <- plyr::rbind.fill(list(data, ...))
    
    if (missing(cofac.names))
        cofac.names <- names(data)[!vapply(data[1,], is.numeric, logical(1))]
    
    # number of cofactors
    n <- length(cofac.names)
    
    X_dims     <- structure(numeric(n), dim = n, names = cofac.names)
    X_dimnames <- structure(vector('list', n), names = cofac.names)
    
    # labels
    for (i in 1:n) {
        
        cofac.lvls <- levels(data[, cofac.names[i]])
        cofac.lvls <- cofac.lvls[order(cofac.lvls)]
        
        X_dimnames[[cofac.names[i]]] <- cofac.lvls
        X_dims[[cofac.names[i]]]     <- length(cofac.lvls)
    }
    
    # return
    if (dims & !dimnames)     return(X_dims)
    if (dimnames) return(X_dimnames)
    
}
