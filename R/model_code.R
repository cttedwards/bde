#'
#' @title Get model code and write to file
#'
#' @description \code{stan} model code is extracted from the \code{bde} package and written to file for compilation with \code{rstan}.
#' 
#' @return Text file with \code{*.stan} suffix 
#'
#' @export
model_code <- function(id, path = ".") {
    	
	switch(id,
         "bycatch_HHL" = data("model_year_area", package = "bde"))
    
    writeLines(mdl, con = paste0(path, "/", id, ".stan"))
	
	rm(mdl)
}
