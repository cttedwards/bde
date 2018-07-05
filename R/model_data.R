#'
#' @title Prepare model data for input
#'
#' @description Data are formatted into a list for input into \code{stan} model
#' 
#' @param data.sample data.frame containing observer sampling data for estimation of catch rates
#' @param data.predict data.frame containing commercial effort data for prediction of catch
#' @param X.dims model dimensions from call to \code{data_dims()}
#' @param X.sample model design matrix for sampling data from call to \code{data_design_matrix()}
#' @param X.predict model design matrix for commercial data from call to \code{data_design_matrix()}
#' @param area.sample.to.estimate integer vector mapping sampling/prediction area to area definition used for coefficient estimation
#' 
#' @return list containing data for \code{stan} model run. 
#'
#' @export
model_data <- function(data.sample, data.predict, X.dims, X.sample, X.predict, area.sample.to.estimate) {
    
    stopifnot(nrow(data.sample)  == length(X.sample[[1]]))
    stopifnot(nrow(data.predict) == length(X.predict[[1]]))
    
    for (i in names(X.dims)) {
        if (nlevels(data.sample[,i])  < as.integer(X.dims[[i]])) warning("No data for some model covariates: predicted catch will follow intercept terms")
        if (nlevels(data.predict[,i]) > as.integer(X.dims[[i]])) stop("Some covariates needed for prediction are not in the design matrix")
    }
	
	# if there is more than one area definition
	# in the design matrix then we consider
	# the model to assume disjunct
	# sampling and estimation areas and check
	# they are labelled correctly
	area.disjunct <- FALSE
	if (length(grep("area", names(X.dims))) > 1) {
		
		if ("area_sample" %in% names(X.dims) & "area_estimate" %in% names(X.dims)) {
			area.disjunct <- TRUE
		} else {
			stop("area disjunct model should have area factors 'area_sample' and 'area_estimate' in the data")
		}
	}
	area.conjunct <- !area.disjunct
	
	if (area.disjunct & missing(area.sample.to.estimate)) {
		area.sample.to.estimate <- 1:X.dims['area_sample']
		warning("using the area disjunct model but sample and estimation areas are the same")
	}
    
	if ("year" %in% names(X.dims)) {
		ind <- "year"
			if ("area" %in% names(X.dims) | area.disjunct)
				ind <- c(ind, "area")
			if ("gear" %in% names(X.dims))
			    ind <- c(ind, "method")
			if ("category" %in% names(X.dims))
			    ind <- c(ind, "category")
	}
	
	ind <- paste(ind, collapse = "_")
	
	def <- switch(ind, 
				  "year"                      = "reg01a", 
				  "year_area"                 = ifelse(area.conjunct, "reg01b", "reg02b")
				  "year_area_method"          = ifelse(area.conjunct, "reg01c", "reg02c")
				  "year_area_method_category" = ifelse(area.conjunct, "reg01d", "reg02d"))
	
	switch(def,
			   
		"reg01d" = list(N = c(nrow(data.sample), nrow(data.predict)), 
						Y = X.dims['year'], 
						A = X.dims['area'],
						M = X.dims['gear'],
						V = X.dims['category'],
						XY_sample  = X.sample[['year']],  XA_sample  = X.sample[['area']],  XM_sample  = X.sample[['gear']],  XV_sample  = X.sample[['category']], 
						XY_predict = X.predict[['year']], XA_predict = X.predict[['area']], XM_predict = X.predict[['gear']], XV_predict = X.predict[['category']], 
						pos = as.numeric(data.sample$biomass), 
						bin = as.integer(data.sample$bin), 
						eff_sample  = as.integer(data.sample$effort), 
						eff_predict = as.integer(data.predict$effort)),
			   
		"reg01c" = list(N = c(nrow(data.sample), nrow(data.predict)), 
						Y = X.dims['year'], 
						A = X.dims['area'],
						M = X.dims['gear'],
						XY_sample  = X.sample[['year']],  XA_sample  = X.sample[['area']],  XM_sample  = X.sample[['gear']], 
						XY_predict = X.predict[['year']], XA_predict = X.predict[['area']], XM_predict = X.predict[['gear']], 
						pos = as.numeric(data.sample$biomass), 
						bin = as.integer(data.sample$bin), 
						eff_sample  = as.integer(data.sample$effort), 
						eff_predict = as.integer(data.predict$effort)),

		"reg01b" = list(N = c(nrow(data.sample), nrow(data.predict)), 
						Y = X.dims['year'], 
						A = X.dims['area'],
						XY_sample  = X.sample[['year']],  XA_sample  = X.sample[['area']], 
						XY_predict = X.predict[['year']], XA_predict = X.predict[['area']], 
						pos = as.numeric(data.sample$biomass), 
						bin = as.integer(data.sample$bin), 
						eff_sample  = as.integer(data.sample$effort), 
						eff_predict = as.integer(data.predict$effort)),
								 
		"reg01a" = list(N = c(nrow(data.sample), nrow(data.predict)), 
						Y = X.dims['year'], 
						XY_sample  = X.sample[['year']], 
						XY_predict = X.predict[['year']], 
						pos = as.numeric(data.sample$biomass), 
						bin = as.integer(data.sample$bin), 
						eff_sample  = as.integer(data.sample$effort), 
						eff_predict = as.integer(data.predict$effort)),
	
		"reg02b" = list(N = c(nrow(dat.sample), nrow(dat.predict)), 
						Y = as.integer(X.dims['year']), 
						A = as.integer(c(X.dims['area_sample'], X.dims['area_estimate'])),
						XY_sample  = X.sample[['year']],  XA_sample  = X.sample[['area_sample']], XA_estimate = X.sample[['area_estimate']], 
						XY_predict = X.predict[['year']], XA_predict = X.predict[['area_sample']], 
						pos = as.numeric(dat.sample$biomass), 
						bin = as.integer(dat.sample$bin), 
						eff_sample  = as.integer(dat.sample$effort), 
						eff_predict = as.integer(dat.predict$effort),
						area_to_area = structure(as.integer(area.sample.to.estimate), dim = X.dims['area_sample'])),
						
		"reg02c" = list(N = c(nrow(dat.sample), nrow(dat.predict)), 
						Y = as.integer(X.dims['year']), 
						A = as.integer(c(X.dims['area_sample'], X.dims['area_estimate'])),
						M = X.dims['gear'],
						XY_sample  = X.sample[['year']],  XA_sample  = X.sample[['area_sample']], XA_estimate = X.sample[['area_estimate']], XM_sample  = X.sample[['gear']], 
						XY_predict = X.predict[['year']], XA_predict = X.predict[['area_sample']], XM_predict  = X.predict[['gear']], 
						pos = as.numeric(dat.sample$biomass), 
						bin = as.integer(dat.sample$bin), 
						eff_sample  = as.integer(dat.sample$effort), 
						eff_predict = as.integer(dat.predict$effort),
						area_to_area = structure(as.integer(area.sample.to.estimate), dim = X.dims['area_sample']))
		)
	
}
