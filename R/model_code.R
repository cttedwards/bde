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
		stop("supply one of: id = 'year' or 'year_area'")
		
	switch(id,
         "bycatch_HHL" = { data("reg02", package = "bde", envir = environment()); writeLines(reg02, con = paste0(path, "/", id, ".stan")) },
         "year"        = { data("reg01", package = "bde", envir = environment()); writeLines(reg01, con = paste0(path, "/", id, ".stan")) },
         "year_area"   = { data("reg02", package = "bde", envir = environment()); writeLines(reg02, con = paste0(path, "/", id, ".stan")) })
    
}
