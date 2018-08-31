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
model_code <- function(covariates = c("year", "area"), path = ".", area.conjunct = TRUE, interaction = TRUE) {
   	
	if (missing(covariates)) {
		message("loading year:area model")
	}

	id <- paste(covariates, collapse = "_")
	
	def <- switch(id, 
				  "year"                      = "Y000_00",
				  "year_area"                 = ifelse(interaction, ifelse(area.conjunct, "YA00_0I", "YA00_DI"), ifelse(area.conjunct, "YA00_00", "YA00_D0")),
				  "year_area_method"          = ifelse(interaction, ifelse(area.conjunct, "YAM0_0I", "YAM0_DI"), ifelse(area.conjunct, "YAM0_00", "YAM0_D0")),
				  "year_area_method_category" = ifelse(interaction, ifelse(area.conjunct, "YAMC_0I", "YAMC_DI"), ifelse(area.conjunct, "YAMC_00", "YAMC_D0")),
				  stop("model not found"))
				  
	if (def == "year" & !area.conjunct) {
		
		warning("for 'year' model area.conjunct argument ignored")
		
		area.conjunct <- TRUE
	}
		
	output.filename <- paste0(path, "/", id, ".stan")
	
	if (area.conjunct) {
		message(paste("writing", def, "model to", path, "assuming observer sampling and estimation areas are the same"))
	} else {
		message(paste("writing", def, "model to", path, "assuming observer sampling and estimation areas are different"))
	}
	
	switch(def,
		 "Y000_00"                      = { data("reg_Y000_00", package = "bde", envir = environment()); writeLines(reg_Y000_00, con = output.filename) },
		 "YA00_00"                      = { data("reg_YA00_00", package = "bde", envir = environment()); writeLines(reg_YA00_00, con = output.filename) },
		 "YA00_0I"                      = { data("reg_YA00_0I", package = "bde", envir = environment()); writeLines(reg_YA00_0I, con = output.filename) },
		 "YA00_D0"                      = { data("reg_YA00_D0", package = "bde", envir = environment()); writeLines(reg_YA00_D0, con = output.filename) },
		 "YA00_DI"                      = { data("reg_YA00_DI", package = "bde", envir = environment()); writeLines(reg_YA00_DI, con = output.filename) },
		 "YAM0_00"                      = { data("reg_YAM0_00", package = "bde", envir = environment()); writeLines(reg_YAM0_00, con = output.filename) },
		 "YAM0_0I"                      = { data("reg_YAM0_0I", package = "bde", envir = environment()); writeLines(reg_YAM0_0I, con = output.filename) },
		 "YAM0_D0"                      = { data("reg_YAM0_D0", package = "bde", envir = environment()); writeLines(reg_YAM0_D0, con = output.filename) },
		 "YAM0_DI"                      = { data("reg_YAM0_DI", package = "bde", envir = environment()); writeLines(reg_YAM0_DI, con = output.filename) },
		 "YAMC_00"                      = { data("reg_YAMC_00", package = "bde", envir = environment()); writeLines(reg_YAMC_00, con = output.filename) },
		 "YAMC_0I"                      = { data("reg_YAMC_0I", package = "bde", envir = environment()); writeLines(reg_YAMC_0I, con = output.filename) },
		 "YAMC_D0"                      = { data("reg_YAMC_D0", package = "bde", envir = environment()); writeLines(reg_YAMC_D0, con = output.filename) },
		 "YAMC_DI"                      = { data("reg_YAMC_DI", package = "bde", envir = environment()); writeLines(reg_YAMC_DI, con = output.filename) })
		     
}
