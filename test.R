setwd("~/projects/water/library")

library(rapport) # for tocamel

rv <- function(value, units=NULL) {
  var <- list(value=value, units=units)
  class(var) <- "rowvar"
  return(var)
}

rowmodel <- function(constants, stocks) {
  model <- list(constants=constants, stocks=stocks, equations=c())
  class(model) <- "rowmodel"
  model
}

`+.rowmodel` <- function(model, equation) {
  model[["equations"]] <- c(model[["equations"]], equation)
  model
}

tocamel <- function(name) {
  gsub("\\.(\\w?)", "\\U\\1", name, perl=T)
}

modeleqn <- function(eqn) {
  text <- strsplit(deparse(eqn), "~")[[1]]
  deriv <- regexpr("diff(\\(.+\\))", text[1], perl=T)
  if (deriv) {
    whole <- regmatches(text[1], deriv)
    var <- substr(whole, 6, nchar(whole)-1)
    line <- paste(var, ".ddt(", text[2], ");\n", sep="")
  } else {
    var <- gsub("^\\s+|\\s+$", "", text[1])
    line <- paste("TemporalVariable& ", var, " = ", text[2], ";\n")
  }

  line
}

rowcompile <- function(model) {
    modelname <- paste("ROW", paste(floor(runif(10, 0, 10)), collapse=""), sep="")

    template <- "row/inst/extdata/ModelTemplate.h"
    #template <- system.file("ModelTemplate.h", package="row")
    text <- readLines(template, encoding="UTF-8")

    text <- gsub("\\$\\{MODEL\\}", modelname, text)

    constanttext <- ""
    for (var in names(model$constants))
      constanttext <- paste(constanttext, "Constant ", tocamel(var), ";\n", sep="")
    text <- gsub("\\$\\{CONSTANTS\\}", constanttext, text)
    
    stocktext <- ""
    for (var in names(model$stocks))
      stocktext <- paste(stocktext, "Stock ", tocamel(var), ";\n", sep="")
    text <- gsub("\\$\\{STOCKS\\}", stocktext, text)
    
    namedvartext <- ""
    for (var in names(model$namedvars))
      namedvartext <- paste(namedvartext, "NamedVariable ", tocamel(var), ";", sep="")
    text <- gsub("\\$\\{NAMEDVARS\\}", namedvartext, text)

    inits <- c()
    for (var in names(model$constants)) {
      if (is.numeric(model$constants[[var]]))
        inits <- c(inits, paste(tocamel(var), "(\"", var, "\", ", model$constants[[var]], ", Units::none, *this)", sep=""))
      else
        inits <- c(inits, paste(tocamel(var), "(\"", var, "\", ", model$constants[[var]]$value, ", Units::", model$constants[[var]]$units, ", *this)", sep=""))
    }
    for (var in names(model$stocks))
      inits <- c(inits, paste(tocamel(var), "(\"", var, "\", ", model$stocks[[var]]$value, ", Units::", model$stocks[[var]]$units, ", *this)", sep=""))
    for (var in names(model$namedvars)) {
      if (is.numeric(model$constants[[var]]))
        inits <- c(inits, paste(tocamel(var), "(\"", var, "\", ", model$namedvars[[var]], ", Units::none, *this)", sep=""))
      else
        inits <- c(inits, paste(tocamel(var), "(\"", var, "\", ", model$namedvars[[var]]$value, ", Units::", model$namedvars[[var]]$units, ", *this)", sep=""))
    }
    initstext <- paste(inits, collapse=", ")

    text <- gsub("\\$\\{VARINITS\\}", initstext, text)

    equations <- c()
    for (eqn in model$equations)
      equations <- c(equations, modeleqn(eqn))
    equationstext <- paste(equations, collapse="\n  ")

    text <- gsub("\\$\\{EQUATIONS\\}", initstext, text)
    
    filename <- paste(modelname, "Model.h", sep="\n")
    writeLines(text, con=filename)
}


stocks <- list(population=rv(100, "people"))
constants <- list(growth.rate=.1)

model <- rowmodel(constants, stocks) +
  (diff(population) ~ growth.rate * population)

rowcompile(model)



