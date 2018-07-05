#'
#' @title Get model code and write to file
#'
#' @description \code{stan} model code is extracted from the \code{bde} package and written to file for compilation with \code{rstan}.
#'
#' @param id one of \code{'year'}, \code{'year_area'}, \code{'year_area_method'} or \code{'year_area_method_category'}
#' @param path where to save the model code
#' @param area.conjunct logical indicating whether observer sampling area definitions are the same as the estimation areas used to fit the model. For example, sampling data may be grouped by area for estimation purposes and coefficient values then shared across areas for prediction.
#' 
#' @return text file with \code{*.stan} suffix 
#'
#' @export
model_code <- function(id, path = ".", area.conjunct = TRUE) {
   	
	if (missing(id)) 
		stop("supply one of: id = 'year' or 'year_area' or 'year_area_method' or 'year_area_method_category'")
		
	def <- switch(id, 
				  "year"                      = "year",
				  "year_area"                 = "year_area",
				  "year_area_method"          = "year_area_method",
				  "year_area_method_category" = "year_area_method_category",
				  "bycatch_HHL"               = "year_area_method")
				  
	if (def == "year" & !area.conjunct) {
		
		warning("for 'year' model area.conjunct argument is ignored")
		
		area.conjunct <- TRUE
	}
		
	output.filename <- paste0(path, "/", id, ".stan")
	
	if (area.conjunct) {
	
		message(paste("writing", def, "model to", path, "assuming observer sampling and estimation areas are the same"))
	
		switch(def,
			 "year"                      = { data("reg01a", package = "bde", envir = environment()); writeLines(reg01a, con = output.filename) },
			 "year_area"                 = { data("reg01b", package = "bde", envir = environment()); writeLines(reg01b, con = output.filename) },
			 "year_area_method"          = { data("reg01c", package = "bde", envir = environment()); writeLines(reg01c, con = output.filename) },
			 "year_area_method_category" = { data("reg01d", package = "bde", envir = environment()); writeLines(reg01d, con = output.filename) })
		 
	} else {
	
		message(paste("writing", def, "model to", path, "assuming observer sampling and estimation areas are different"))
	
		switch(def,
			 "year_area"                 = { data("reg02b", package = "bde", envir = environment()); writeLines(reg02b, con = output.filename) },
			 "year_area_method"          = { data("reg02c", package = "bde", envir = environment()); writeLines(reg02c, con = output.filename) })
	}
    
}
