#'
#' @title Get model code and write to file
#'
#' @description \code{stan} model code is extracted from the \code{bde} package and written to file for compilation with \code{rstan}.
#'
#' @param covariates character vector of covariates to include (see Details)
#' @param path where to save the model code
#' @param area.conjunct logical indicating whether observer sampling area definitions are the same as the estimation areas used to fit the model.
#' @param interaction logical indicating whether an interaction between year and area covariates should be estimated.
#' @param hierarchical logical indicating whether the model has a hierachical error structure.
#'
#' @details 
#' Model covariates can be \code{'year'}, \code{'area'}, \code{'method'} and \code{'category'}, and must be included in that hierarchical order (i.e. all models must include a year covariate, and all models with a category covariate must also include method, area and year. 
#' 
#' If \code{area.conjunct = FALSE}, then the area definitions used to estimate area covariate parameter values are not the same as the area definitions used for observer sampling or commercial catch prediction. 
#' Usually sampling areas will be combined for better estimation, and then covariate estimates shared across neighbouring areas for prediction. If \code{area.conjunct = FALSE} be sure to correctly specify the \code{area.sample.to.estimate} argument in \code{\link[bde]{model_data}}.
#'
#' If \code{hierarchical = TRUE} then error terms are estimated, namely the log-normal observation error and, if present, the variance of the distribution of year:area interactions.
#'
#' @seealso \code{\link{model_data}}
#' 
#' @return text file with \code{*.stan} suffix 
#' 
#' @examples 
#' require(rstan)
#' model_code(covariates = c("year", "area"), interaction = TRUE)
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
				  
	if (id == "year" & !area.conjunct) {
		
		warning("for 'year' model 'area.conjunct' argument is always TRUE")
		area.conjunct <- TRUE
	}
	
	if (id == "year" & interaction) {
		
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
	message(paste("writing", ifelse(hierarchical, "hierarchical", ""), id, "model to", ifelse(path == ".", "current directory", path), ifelse(interaction, "with", "without"), "interaction term and assuming observer sampling and estimation areas are", ifelse(area.conjunct, "the same", "different")))
	
	# write model code according to model definition
	switch(def,
		 
		 "Y000_000"                      = { writeLines(readLines(system.file("extdata/stan", "Y000_000.stan", package = "bde")), con = output.filename) },
		 "Y000_00H"                      = { writeLines(readLines(system.file("extdata/stan", "Y000_00H.stan", package = "bde")), con = output.filename) },
		 
		 "YA00_00H"                      = { writeLines(readLines(system.file("extdata/stan", "YA00_00H.stan", package = "bde")), con = output.filename) },
		 "YA00_0IH"                      = { writeLines(readLines(system.file("extdata/stan", "YA00_0IH.stan", package = "bde")), con = output.filename) },
		 "YA00_D0H"                      = { writeLines(readLines(system.file("extdata/stan", "YA00_D0H.stan", package = "bde")), con = output.filename) },
		 "YA00_DIH"                      = { writeLines(readLines(system.file("extdata/stan", "YA00_DIH.stan", package = "bde")), con = output.filename) },
		 
		 "YA00_000"                      = { writeLines(readLines(system.file("extdata/stan", "YA00_000.stan", package = "bde")), con = output.filename) },
		 "YA00_0I0"                      = { writeLines(readLines(system.file("extdata/stan", "YA00_0I0.stan", package = "bde")), con = output.filename) },
		 "YA00_D00"                      = { writeLines(readLines(system.file("extdata/stan", "YA00_D00.stan", package = "bde")), con = output.filename) },
		 "YA00_DI0"                      = { writeLines(readLines(system.file("extdata/stan", "YA00_DI0.stan", package = "bde")), con = output.filename) },
		 
		 "YAM0_00H"                      = { writeLines(readLines(system.file("extdata/stan", "YAM0_00H.stan", package = "bde")), con = output.filename) },
		 "YAM0_0IH"                      = { writeLines(readLines(system.file("extdata/stan", "YAM0_0IH.stan", package = "bde")), con = output.filename) },
		 "YAM0_D0H"                      = { writeLines(readLines(system.file("extdata/stan", "YAM0_D0H.stan", package = "bde")), con = output.filename) },
		 "YAM0_DIH"                      = { writeLines(readLines(system.file("extdata/stan", "YAM0_DIH.stan", package = "bde")), con = output.filename) },
		 
		 "YAMC_00H"                      = { writeLines(readLines(system.file("extdata/stan", "YAMC_00H.stan", package = "bde")), con = output.filename) },
		 "YAMC_0IH"                      = { writeLines(readLines(system.file("extdata/stan", "YAMC_0IH.stan", package = "bde")), con = output.filename) },
		 "YAMC_D0H"                      = { writeLines(readLines(system.file("extdata/stan", "YAMC_D0H.stan", package = "bde")), con = output.filename) },
		 "YAMC_DIH"                      = { writeLines(readLines(system.file("extdata/stan", "YAMC_DIH.stan", package = "bde")), con = output.filename) })
}
