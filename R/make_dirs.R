#'
#' @title Make directories
#' 
#' @description Upon assignment, generates a list of directories of the form \code{file.path(root, model, label, type)} and creates these directories if they are missing.
#'
#' @param model optional character string giving the model name
#' @param labels character vector of labels for each model run
#' @param root root path to the new directory or directories
#' @param type type of model estimation being one or more of \code{MAP}, \code{VB}, \code{MCMC}
#' @param clean logical indicating whether previously existent directories in the specified path locations should be deleted
#' 
#' @note By default an additional directory is also created of the form \code{file.path(root, model, label, ".")} (i.e. without the \code{type}) specification for storage of model data inputs common to each estimation type.
#' 
#' @export
make_dirs <- function(model, labels, root = "../../results", types = c("MAP", "VB", "MCMC")[3], clean = FALSE) {
    
    paths <- list()
	
    for (lab in labels) {
        
        paths[[lab]] <- list()
        
        if (!missing(model)) { paths[[lab]][["."]] <- file.path(root, model, tolower(lab))
        } else { paths[[lab]][["."]] <- file.path(root, tolower(lab)) }
        
        for (type in types) {
    
            if (!missing(model)) { path <- file.path(root, model, tolower(lab), tolower(type))
            } else { path <- file.path(root, tolower(lab), tolower(type)) }
            
            if (dir.exists(path)) {
                
                if (clean) {
                    
                    warning("deleted contents of ", path, " directory")
                    unlink(path, recursive = TRUE) 
                    
                    message("creating clean ", path, " directory")
                    dir.create(path, recursive = TRUE)
                    
                } else {
                    
                    message(path, " directory already exists, run with clean = TRUE?")    
                }
            } else {
                
                message("creating ", path, " directory")
                dir.create(path, recursive = TRUE)
            }
            
            paths[[lab]][[type]] <- path
        }
    }
    
    invisible(paths)
}
