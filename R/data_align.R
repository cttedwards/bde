#'
#' @title Trim a data.frame so that remaining cofactor levels match the given values
#' 
#' @description When designing the analysis this function can be used to remove unwanted observer data that might not be needed for prediction, or conversely, to remove commercial effort data with no associated observer samples.
#'
#' @param data input data.frame
#' @param label cofactor to trim
#' @param values levels of cofactor to keep
#'
#' @return Trimmed data.frame with \code{all(levels(data$label) %in% values)}
#' 
#' @examples
#' # remove unnecessary observer data from dfr.sample
#' dfr.sample  <- expand.grid(year = c(1, 2, 3), area = c("N", "S"))
#' dfr.predict <- expand.grid(year = c(1, 2),    area = c("N", "S"))
#' 
#' data_align(dfr.sample, label = "year", values = unique(dfr.predict$year))
#' 
#' # remove predictions with no covariate data from dfr.predict
#' dfr.sample  <- expand.grid(year = c(1, 2), area = c("N", "S"))
#' dfr.predict <- expand.grid(year = c(1, 2), area = c("N", "S", "E", "W"))
#' 
#' data_align(dfr.predict, label = "area", values = unique(dfr.sample$area))
#'
#' @export
data_align <- function(data, label = character(), values = character()) {
    
    loc <- match(label, colnames(data))
    
    data <- data[data[,loc] %in% values,]
    
    data[,loc] <- factor(data[,loc])
    
    return(data)
}
