#'
#' @title Prepare model data for input
#'
#' @description Data are formatted into a list for input into \code{stan} model
#' 
#' @param data.sample data.frame containing observer sampling data for estimation of catch rates
#' @param data.predict data.frame containing commercial effort data for prediction of catch
#' @param X.dims model dimensions from call to \code{data_dims()}
#' @param X.sample model design matrix for sampling data from call to \code{data_design_matrix()}
#' @param X.predict model design matrix for commerical data from call to \code{data_design_matrix()}
#' @param fit_by_tow logical value indicating whether model should be fitted per tow or using the aggregated data
#' 
#' @return list containing data for \code{stan} model run. 
#'
#' @export
model_data <- function(data.sample, data.predict, X.dims, X.sample, X.predict, fit_by_tow = TRUE) {
    
    stopifnot(nrow(data.sample)  == length(X.sample[[1]]))
    stopifnot(nrow(data.predict) == length(X.predict[[1]]))
    
    if (any(c(nlevels(data.sample$area),  nlevels(data.sample$year))  < as.integer(X.dims))) warning("No data for some model covariates: predicted catch will follow intercept terms")
    if (any(c(nlevels(data.predict$area), nlevels(data.predict$year)) > as.integer(X.dims))) stop("Some covariates needed for prediction are not in the design matrix")
    
    list(N = c(nrow(data.sample), nrow(data.predict)), 
         Y = X.dims['year'], 
         A = X.dims['area'],
         XY_sample  = X.sample[['year']],  XA_sample  = X.sample[['area']], 
         XY_predict = X.predict[['year']], XA_predict = X.predict[['area']], 
         pos = as.numeric(data.sample$biomass), 
         bin = as.integer(data.sample$bin), 
         eff_sample  = as.integer(data.sample$effort), 
         eff_predict = as.integer(data.predict$effort),
         fit_by_tow = as.integer(fit_by_tow))
}
