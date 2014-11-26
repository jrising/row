#' Compile OpenWorld model
#'
#' Generate code for compilation
#' @param constants Constant parameters
#' @param stocks Stock variables
#' @param variables All other variables
#' @param equations Equations relating the variables
#' @keywords OpenWorld
#' @export
#' @examples
#' constants <- list(
#' rowcompile(constants, stocks, variables, equations)
 
rowcompile <- function(constants, stocks, variables, equations) {
    modelname <- paste("ROW", paste(floor(runif(10, 0, 10)), collapse=""), sep="")
    

    template <- system.file("ModelTemplate.h", package="row")
    text <- readLines(template, encoding="UTF-8")

    filename <- paste(modelname, "Model.h", sep="")
    writeLines(text, con=filename)
}

