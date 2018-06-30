#'
#' @title Get model code and write to file
#'
#' @description \code{stan} model code is extracted from the \code{bde} package and written to file for compilation with \code{rstan}.
#' 
#' @return Text file with \code{*.stan} suffix 
#'
#' @export
model_code <- function(id, path = ".") {
    	
	if (missing(id)) 
		stop("supply one of: id = 'year' or 'year_area' or 'year_area_method' or 'year_area_method_category'")
		
	def <- switch(id, 
				  "year"                      = "year",
				  "year_area"                 = "year_area",
				  "year_area_method"          = "year_area_method",
				  "year_area_method_category" = "year_area_method_category",
				  "bycatch_HHL"               = "year_area_method")
		
	output.filename <- paste0(path, "/", id, ".stan")
	
	switch(def,
         "year"                      = { data("reg01", package = "bde", envir = environment()); writeLines(reg01, con = output.filename) },
         "year_area"                 = { data("reg02", package = "bde", envir = environment()); writeLines(reg02, con = output.filename) },
		 "year_area_method"          = { data("reg03", package = "bde", envir = environment()); writeLines(reg03, con = output.filename) },
		 "year_area_method_category" = { data("reg04", package = "bde", envir = environment()); writeLines(reg04, con = output.filename) })
    
}
