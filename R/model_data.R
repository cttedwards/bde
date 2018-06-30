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
#' @param fit_interaction logical value indicating whether year:area interaction term should be estimated
#' 
#' @return list containing data for \code{stan} model run. 
#'
#' @export
model_data <- function(data.sample, data.predict, X.dims, X.sample, X.predict, fit_interaction = TRUE) {
    
    stopifnot(nrow(data.sample)  == length(X.sample[[1]]))
    stopifnot(nrow(data.predict) == length(X.predict[[1]]))
    
    for (i in names(X.dims)) {
        if (nlevels(data.sample[,i])  < as.integer(X.dims[[i]])) warning("No data for some model covariates: predicted catch will follow intercept terms")
        if (nlevels(data.predict[,i]) > as.integer(X.dims[[i]])) stop("Some covariates needed for prediction are not in the design matrix")
    }
    
	if ("year" %in% names(X.dims)) {
		ind <- "year"
			if ("area" %in% names(X.dims))
				ind <- c(ind, "area")
			if ("gear" %in% names(X.dims))
			    ind <- c(ind, "method")
			if ("category" %in% names(X.dims))
			    ind <- c(ind, "category")
	}
	
	ind <- paste(ind, collapse = "_")
	
    switch(ind,
           
           "year_area_method_category" = list(N = c(nrow(data.sample), nrow(data.predict)), 
                                     Y = X.dims['year'], 
                                     A = X.dims['area'],
                                     G = X.dims['gear'],
                                     V = X.dims['category'],
                                     XY_sample  = X.sample[['year']],  XA_sample  = X.sample[['area']],  XG_sample  = X.sample[['gear']],  XV_sample  = X.sample[['category']], 
                                     XY_predict = X.predict[['year']], XA_predict = X.predict[['area']], XG_predict = X.predict[['gear']], XV_predict = X.predict[['category']], 
                                     pos = as.numeric(data.sample$biomass), 
                                     bin = as.integer(data.sample$bin), 
                                     eff_sample  = as.integer(data.sample$effort), 
                                     eff_predict = as.integer(data.predict$effort),
                                     fit_interaction = as.integer(fit_interaction)),
           
           "year_area_method" = list(N = c(nrow(data.sample), nrow(data.predict)), 
                              Y = X.dims['year'], 
                              A = X.dims['area'],
                              G = X.dims['gear'],
                              XY_sample  = X.sample[['year']],  XA_sample  = X.sample[['area']],  XG_sample  = X.sample[['gear']], 
                              XY_predict = X.predict[['year']], XA_predict = X.predict[['area']], XG_predict = X.predict[['gear']], 
                              pos = as.numeric(data.sample$biomass), 
                              bin = as.integer(data.sample$bin), 
                              eff_sample  = as.integer(data.sample$effort), 
                              eff_predict = as.integer(data.predict$effort),
                              fit_interaction = as.integer(fit_interaction)),
	
		"year_area" = list(N = c(nrow(data.sample), nrow(data.predict)), 
							Y = X.dims['year'], 
							A = X.dims['area'],
							XY_sample  = X.sample[['year']],  XA_sample  = X.sample[['area']], 
							XY_predict = X.predict[['year']], XA_predict = X.predict[['area']], 
							pos = as.numeric(data.sample$biomass), 
							bin = as.integer(data.sample$bin), 
							eff_sample  = as.integer(data.sample$effort), 
							eff_predict = as.integer(data.predict$effort),
							fit_interaction = as.integer(fit_interaction)),
							 
		"year" = list(N = c(nrow(data.sample), nrow(data.predict)), 
						Y = X.dims['year'], 
						XY_sample  = X.sample[['year']], 
						XY_predict = X.predict[['year']], 
						pos = as.numeric(data.sample$biomass), 
						bin = as.integer(data.sample$bin), 
						eff_sample  = as.integer(data.sample$effort), 
						eff_predict = as.integer(data.predict$effort),
						fit_interaction = 0L)
	)							 
}
