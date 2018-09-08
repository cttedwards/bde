#'
#' @title Prepare model data for input
#'
#' @description Data are formatted into a list for input into \code{stan} model. A call to this function is typically preceeded by a call to \code{\link{data_prep}}, which will format the \code{data.sample} and \code{data.predict} data frames correctly. 
#' 
#' @param data.sample data.frame containing observer sampling data for estimation of catch rates
#' @param data.predict data.frame containing commercial effort data for prediction of catch
#' @param X.dims model dimensions from call to \code{\link{data_dims}}
#' @param X.sample model design matrix for sampling data from call to \code{\link{data_design_matrix}}
#' @param X.predict model design matrix for commercial data from call to \code{\link{data_design_matrix}}
#' @param area.sample.to.estimate integer vector mapping sampling/prediction area to area definition used for coefficient estimation
#' 
#' @details This function will use the covariate names in \code{X.dims} to generate a list of data appropriate for model input. If an area-disjunct model is being used, then the input data should contain columns headed \code{area_sample} and \code{area_estimate}. The \code{area.sample.to.estimate} argument is then used to map one to the other. For example, if we have \code{area_sample = c("NORTH", "SOUTH", "EAST", "WEST")} and \code{area_estimate = c("NORTH", "SOUTH", "EAST_WEST", "EAST_WEST")} in the \code{data.sample} and \code{data.predict} data frames, then we would specify \code{area.sample.to.estimate = c(1, 2, 3, 3)}, which gives \code{area_sample[area.sample.to.estimate] == area_estimate}. In this case, only three area covariates would be estimated, with the \code{EAST_WEST} covariate used to predict catches in both \code{EAST} and \code{WEST}.
#'
#' @seealso \code{\link{data_prep}}, \code{\link{data_dims}}, \code{\link{data_design_matrix}}
#'
#' @return list containing data for \code{stan} model run. 
#'
#' @export
model_data <- function(data.sample, data.predict, X.dims, X.sample, X.predict, area.sample.to.estimate, verbose = TRUE) {
    
    stopifnot(nrow(data.sample)  == length(X.sample[[1]]))
    stopifnot(nrow(data.predict) == length(X.predict[[1]]))
    
    for (i in names(X.dims)[names(X.dims) %in% names(dat.sample)]) {
        if (nlevels(data.sample[,i])  < as.integer(X.dims[[i]])) warning("No data for some model covariates: predicted catch will follow intercept terms")
    }
    for (i in names(X.dims)[names(X.dims) %in% names(dat.predict)]) {
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
	
	if (area.disjunct) {
		if (missing(area.sample.to.estimate)) {
			area.sample.to.estimate <- 1:X.dims['area_sample']
			warning("you are using the area disjunct model but sample and estimation areas are the same")
		} else {
			stopifnot(length(area.sample.to.estimate) == X.dims['area_sample'])
		}
	}
	
	if (verbose & area.disjunct) {
		
		message("Print sampling and estimation areas")
		print(data.frame(area_sample = X.dimnames$area_sample, area_estimate = X.dimnames$area_estimate[area.sample.to.estimate]))
	}
    
	if ("year" %in% names(X.dims)) {
		ind <- "year"
		if ("area" %in% names(X.dims) | area.disjunct)
			ind <- c(ind, "area")
		if ("method" %in% names(X.dims))
			ind <- c(ind, "method")
		if ("category" %in% names(X.dims))
			ind <- c(ind, "category")
	}
	
	ind <- paste(ind, collapse = "_")
	
	def <- switch(ind, 
			"year"                      = "Y000_0", 
			"year_area"                 = ifelse(area.conjunct, "YA00_0", "YA00_D"),
			"year_area_method"          = ifelse(area.conjunct, "YAM0_0", "YAM0_D"),
			"year_area_method_category" = ifelse(area.conjunct, "YAMC_0", "YAMC_D"))
	
	switch(def,
			   
		"YAMC_0" = list(N = c(nrow(data.sample), nrow(data.predict)), 
						Y = X.dims['year'], 
						A = X.dims['area'],
						M = X.dims['method'],
						C = X.dims['category'],
						XY_sample  = X.sample[['year']],  XA_sample  = X.sample[['area']],  XM_sample  = X.sample[['method']],  XC_sample  = X.sample[['category']], 
						XY_predict = X.predict[['year']], XA_predict = X.predict[['area']], XM_predict = X.predict[['method']], XC_predict = X.predict[['category']], 
						pos = as.numeric(data.sample$biomass), 
						bin = as.integer(data.sample$bin), 
						eff_sample  = as.integer(data.sample$effort), 
						eff_predict = as.integer(data.predict$effort)),
			   
		"YAM0_0" = list(N = c(nrow(data.sample), nrow(data.predict)), 
						Y = X.dims['year'], 
						A = X.dims['area'],
						M = X.dims['method'],
						XY_sample  = X.sample[['year']],  XA_sample  = X.sample[['area']],  XM_sample  = X.sample[['method']], 
						XY_predict = X.predict[['year']], XA_predict = X.predict[['area']], XM_predict = X.predict[['method']], 
						pos = as.numeric(data.sample$biomass), 
						bin = as.integer(data.sample$bin), 
						eff_sample  = as.integer(data.sample$effort), 
						eff_predict = as.integer(data.predict$effort)),

		"YA00_0" = list(N = c(nrow(data.sample), nrow(data.predict)), 
						Y = X.dims['year'], 
						A = X.dims['area'],
						XY_sample  = X.sample[['year']],  XA_sample  = X.sample[['area']], 
						XY_predict = X.predict[['year']], XA_predict = X.predict[['area']], 
						pos = as.numeric(data.sample$biomass), 
						bin = as.integer(data.sample$bin), 
						eff_sample  = as.integer(data.sample$effort), 
						eff_predict = as.integer(data.predict$effort)),
								 
		"Y000_0" = list(N = c(nrow(data.sample), nrow(data.predict)), 
						Y = X.dims['year'], 
						XY_sample  = X.sample[['year']], 
						XY_predict = X.predict[['year']], 
						pos = as.numeric(data.sample$biomass), 
						bin = as.integer(data.sample$bin), 
						eff_sample  = as.integer(data.sample$effort), 
						eff_predict = as.integer(data.predict$effort)),
	
		"YA00_D" = list(N = c(nrow(dat.sample), nrow(dat.predict)), 
						Y = as.integer(X.dims['year']), 
						A = as.integer(c(X.dims['area_sample'], X.dims['area_estimate'])),
						XY_sample  = X.sample[['year']],  XA_sample  = X.sample[['area_sample']], XA_estimate = X.sample[['area_estimate']], 
						XY_predict = X.predict[['year']], XA_predict = X.predict[['area_sample']], 
						pos = as.numeric(dat.sample$biomass), 
						bin = as.integer(dat.sample$bin), 
						eff_sample  = as.integer(dat.sample$effort), 
						eff_predict = as.integer(dat.predict$effort),
						area_to_area = structure(as.integer(area.sample.to.estimate), dim = X.dims['area_sample'])),
						
		"YAM0_D" = list(N = c(nrow(dat.sample), nrow(dat.predict)), 
						Y = as.integer(X.dims['year']), 
						A = as.integer(c(X.dims['area_sample'], X.dims['area_estimate'])),
						M = X.dims['method'],
						XY_sample  = X.sample[['year']],  XA_sample  = X.sample[['area_sample']], XA_estimate = X.sample[['area_estimate']], XM_sample  = X.sample[['method']], 
						XY_predict = X.predict[['year']], XA_predict = X.predict[['area_sample']], XM_predict  = X.predict[['method']], 
						pos = as.numeric(dat.sample$biomass), 
						bin = as.integer(dat.sample$bin), 
						eff_sample  = as.integer(dat.sample$effort), 
						eff_predict = as.integer(dat.predict$effort),
						area_to_area = structure(as.integer(area.sample.to.estimate), dim = X.dims['area_sample'])),
	       
	   "YAMC_D" = list(N = c(nrow(dat.sample), nrow(dat.predict)), 
					Y = as.integer(X.dims['year']), 
					A = as.integer(c(X.dims['area_sample'], X.dims['area_estimate'])),
					M = X.dims['method'],
					C = X.dims['category'],
					XY_sample  = X.sample[['year']],  XA_sample  = X.sample[['area_sample']], XA_estimate = X.sample[['area_estimate']], XM_sample  = X.sample[['method']],
					XC_sample  = X.sample[['category']], 
					XY_predict = X.predict[['year']], XA_predict = X.predict[['area_sample']], XM_predict  = X.predict[['method']],  XC_predict  = X.predict[['category']], 
					pos = as.numeric(dat.sample$biomass), 
					bin = as.integer(dat.sample$bin), 
					eff_sample  = as.integer(dat.sample$effort), 
					eff_predict = as.integer(dat.predict$effort),
					area_to_area = structure(as.integer(area.sample.to.estimate), dim = X.dims['area_sample']))
		)
	
}
