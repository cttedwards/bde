#'
#' @title Make directories
#' 
#' @description Generates a list of directories of the form \code{file.path(root, model, lab, type)} and creates these directories if they are missing.
#'
#' @param model optional model name
#' @param labels character vector of labels for each model run
#' @param root root path to the new directory or directories
#' @type type of model estimation being one or more of \code{MAP}, \code{VB}, \code{MCMC}
#' 
#' @note by default an additional directory is also created of the form \code{file.path(root, model, lab, '.')} (i.e. without the \code{type}) specification for storage of model data inputs common to each estimation type.
#' 
#' @export
make_dirs <- function(model, labels, root = "../../results", types = c("MAP", "VB", "MCMC")[3]) {
    
    paths <- list()
	
	types <- c(".", types)
    
    for (lab in labels) {
        
        paths[[lab]] <- list()
        
        for (type in types) {
    
            if (!missing(model)) { path <- file.path(root, model, tolower(lab), tolower(type))
            } else { path <- file.path(root, tolower(lab), tolower(type)) }
            if (!dir.exists(path)) dir.create(path, recursive = TRUE)
            
            paths[[lab]][[type]] <- path
        
        }
    }
    
    return(paths)
}
