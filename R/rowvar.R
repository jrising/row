#' A class for variables
#'
#' Specify a named variable with units
#' @param units
#' @export
#' @examples
#' distance <- as.rowvar("cm")

as.rowvar <- function(units) {
    var <- list(units=units)
    class(var) <- "rowvar"
    return(var)
}

print.rowvar <- function(var) {
    cat("Units: ", var$units)
}
