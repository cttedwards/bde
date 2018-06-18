#'
#' @title Trim a data.frame so that remaining cofactor levels match the given values
#'
#' @param data input data.frame
#' @param label cofactor to trim
#' @param values levels of cofactor to keep
#'
#' @return Trimmed data.frame with \code{all(levels(data$label) %in% values)}
#'
#' @export
data_align <- function(data, label = character(), values = character()) {
    
    loc <- match(label, colnames(data))
    
    data <- data[data[,loc] %in% values,]
    
    data[,loc] <- factor(data[,loc])
    
    return(data)
}
