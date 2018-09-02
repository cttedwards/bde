#'
#' @title Make directories
#' 
#' @param model model
#' @param labels character vector of labels for each model run
#' @param root root path to the directory
#' @type type of model estimation being one of \code{MAP}, \code{VB}, \code{MCMC}
#' 
#' @export
make_dirs <- function(model, labels, root = "../../results", types = c("MAP", "VB", "MCMC")[3]) {
    
    paths <- list()
    
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
