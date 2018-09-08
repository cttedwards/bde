#'
#' @title Coerce data
#' 
#' @description Data in data.frame are coerced into numeric or character vectors and renamed
#' 
#' @param var.names list(from, to) of dependent variables
#' @param covar.names list(from, to) of continuous covariates
#' @param cofac.names list(from, to) of discrete covariates
#' 
#' @return A data frame for model input, with correctly labelled columns
#'
#' @examples
#' dfr <- data.frame(observation = runif(3), temperature = runif(3), site = sample(letters[1:2], 3, replace = TRUE))
#' 
#' data_prep(dfr, var.names = list(from = "observation", to = "y"), covar = list(from = "temperature", to = "x"), cofac = list(from = "site", to = "area")) 
#'
#'
#' @export
data_prep <- function(data, var.names = list(), covar.names = list(), cofac.names = list()) {
    
    loc_var   <- match(var.names[['from']],   colnames(data))
    loc_covar <- match(covar.names[['from']], colnames(data))
    loc_cofac <- match(cofac.names[['from']], colnames(data))
    
    # coerce (continuous) dependent variable
    if (!is.null(var.names)) {
        for (i in loc_var)
            data[, i] <- as.numeric(data[, i])
        colnames(data)[loc_var] <- var.names[['to']]
    }
    
    # coerce continuous covariates
    if (!is.null(covar.names)) {
        for (i in loc_covar)
            data[, i] <- as.numeric(data[, i])
        colnames(data)[loc_covar] <- covar.names[['to']]
    }
    
    # coerce discrete covariates
    if (!is.null(cofac.names)) {
        for (i in loc_cofac)
            data[, i] <- factor(data[, i])
        colnames(data)[loc_cofac] <- cofac.names[['to']]
    }
    
    
    # return cleaned data
    return(data[, c(loc_var, loc_cofac, loc_covar)])
    
}
