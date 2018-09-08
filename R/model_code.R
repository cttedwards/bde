#'
#' @title Get model code and write to file
#'
#' @description \code{stan} model code is extracted from the \code{bde} package and written to file for compilation with \code{rstan}.
#'
#' @param covariates character vector of covariates to include (see Details) one of \code{'year'}, \code{'year_area'}, \code{'year_area_method'} or \code{'year_area_method_category'}
#' @param path where to save the model code
#' @param area.conjunct logical indicating whether observer sampling area definitions are the same as the estimation areas used to fit the model.
#' @param interaction logical indicating whether an interaction between year and area covariates should be estimated.
#' @param hierarchical logical indicating whether the model has a hierachical error structure.log-normal observation error should be estimated or fixed at one.
#'
#' @details 
#' Model covariates can be \code{'year'}, \code{'area'}, \code{'method'} and \code{'category'}, and must be included in that hierarchical order (i.e. all models must include a year covariate, and all models with a category covariate must also include method, area and year. 
#' 
#' If \code{area.disjunct = TRUE}, then the area definitions used to estimate area covariate parameter values are not the same as the area definitions used for observer sampling or commercial catch prediction. 
#' Usually sampling areas will be combined and then covariate estimates shared across neighbouring areas for prediction.
#'
#' If \code{hierarchical = TRUE} then error terms are estimated, namely the log-normal observation error and, if present, the variance of the distribution of year:area interactions.
#'
#' @return text file with \code{*.stan} suffix 
#'
#' @export
model_code <- function(covariates, path = ".", area.conjunct = TRUE, interaction = TRUE, hierarchical = TRUE) {
   	
	if (missing(covariates)) {
	    covariates = c("year", "area")
		message("loading year:area model by default")
	}

	id <- paste(covariates, collapse = "_")
	
	# construct model definitions
	def <- switch(id, 
	              "year"                      = "Y000_000",
	              "year_area"                 = "YA00_000",
	              "year_area_method"          = "YAM0_000",
	              "year_area_method_category" = "YAMC_000",
	              stop("model not found"))
				  
	if (def == "year" & !area.conjunct) {
		
		warning("for 'year' model 'area.conjunct' argument is always TRUE")
		area.conjunct <- TRUE
	}
	
	if (def == "year" & interaction) {
		
		warning("for 'year' model 'interaction' argument is always FALSE")
		interaction <- FALSE
	}
	
	if (("method" %in% covariates | "category" %in% covariates) & !hierarchical) {
	    warning("for, 'method' and 'category' models the 'hierachical' argument is always TRUE")
	    hierarchical <- TRUE
	}
	
	if (!area.conjunct) substr(def, 6, 6) <- "D"
	if (interaction)    substr(def, 7, 7) <- "I"
	if (hierarchical)   substr(def, 8, 8) <- "H"
	
	# construct output file path
	output.filename <- file.path(path, paste0(id, ".stan"))
	
	# report model definition
	message(paste("writing", ifelse(hierarchical, "hierarchical", ""), id, "model to", ifelse(path == ".", "current directory", path), ifelse(interaction, "with", "without"), "interaction term and assuming observer sampling and estimation areas are the", ifelse(area.conjunct, "same", "different")))
	
	# write model code according to model definition
	switch(def,
		 
		 "Y000_000"                      = { data("reg_Y000_000", package = "bde", envir = environment()); writeLines(reg_Y000_000, con = output.filename) },
		 "Y000_00H"                      = { data("reg_Y000_00H", package = "bde", envir = environment()); writeLines(reg_Y000_00H, con = output.filename) },
		 
		 "YA00_00H"                      = { data("reg_YA00_00H", package = "bde", envir = environment()); writeLines(reg_YA00_00H, con = output.filename) },
		 "YA00_0IH"                      = { data("reg_YA00_0IH", package = "bde", envir = environment()); writeLines(reg_YA00_0IH, con = output.filename) },
		 "YA00_D0H"                      = { data("reg_YA00_D0H", package = "bde", envir = environment()); writeLines(reg_YA00_D0H, con = output.filename) },
		 "YA00_DIH"                      = { data("reg_YA00_DIH", package = "bde", envir = environment()); writeLines(reg_YA00_DIH, con = output.filename) },
		 
		 "YA00_000"                      = { data("reg_YA00_000", package = "bde", envir = environment()); writeLines(reg_YA00_000, con = output.filename) },
		 "YA00_0I0"                      = { data("reg_YA00_0I0", package = "bde", envir = environment()); writeLines(reg_YA00_0I0, con = output.filename) },
		 "YA00_D00"                      = { data("reg_YA00_D00", package = "bde", envir = environment()); writeLines(reg_YA00_D00, con = output.filename) },
		 "YA00_DI0"                      = { data("reg_YA00_DI0", package = "bde", envir = environment()); writeLines(reg_YA00_DI0, con = output.filename) },
		 
		 "YAM0_00H"                      = { data("reg_YAM0_00H", package = "bde", envir = environment()); writeLines(reg_YAM0_00H, con = output.filename) },
		 "YAM0_0IH"                      = { data("reg_YAM0_0IH", package = "bde", envir = environment()); writeLines(reg_YAM0_0IH, con = output.filename) },
		 "YAM0_D0H"                      = { data("reg_YAM0_D0H", package = "bde", envir = environment()); writeLines(reg_YAM0_D0H, con = output.filename) },
		 "YAM0_DIH"                      = { data("reg_YAM0_DIH", package = "bde", envir = environment()); writeLines(reg_YAM0_DIH, con = output.filename) },
		 
		 "YAMC_00H"                      = { data("reg_YAMC_00H", package = "bde", envir = environment()); writeLines(reg_YAMC_00H, con = output.filename) },
		 "YAMC_0IH"                      = { data("reg_YAMC_0IH", package = "bde", envir = environment()); writeLines(reg_YAMC_0IH, con = output.filename) },
		 "YAMC_D0H"                      = { data("reg_YAMC_D0H", package = "bde", envir = environment()); writeLines(reg_YAMC_D0H, con = output.filename) },
		 "YAMC_DIH"                      = { data("reg_YAMC_DIH", package = "bde", envir = environment()); writeLines(reg_YAMC_DIH, con = output.filename) })
}
