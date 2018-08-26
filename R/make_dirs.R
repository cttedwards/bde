#'
#' @title Make directories
#' 
#' @param model model
#' @param labels character vector of labels for each model run
#' @param root root path to the directory
#' @type type of model estimation being one of \code{MAP}, \code{VB}, \code{MCMC}
#' 
#' @export
make_dirs <- function(model, labels, root = "../../results", type = c("MAP", "VB", "MCMC")[3]) {
    
    paths <- list()
    
    for (lab in labels) {
    
        path <- file.path(root, model, tolower(lab), tolower(type), "/")
        if (!dir.exists(path)) dir.create(path, recursive = TRUE)
        
        paths[[lab]] <- path
    
    }
    
    return(paths)
}
