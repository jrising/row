##setwd("~/projects/water/library")
setwd("~/projects/water")

library(rapport) # for tocamel

rv <- function(value, units=NULL) {
  var <- list(value=value, units=units)
  class(var) <- "rowvar"
  return(var)
}

ru.peryr <- "_PERYR_"

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

convertvariables <- function(expr) {
    gsubfn("[\\w\\.]+", tocamel, expr, perl=T)
}

trim <- function(expr) {
    gsub("^\\s+|\\s+$", "", expr)
}

modeleqn <- function(eqn) {
  text <- strsplit(deparse(eqn), "~")[[1]]
  deriv <- regexpr("diff(\\(.+\\))", text[1], perl=T)
  if (deriv) {
    whole <- regmatches(text[1], deriv)
    var <- substr(whole, 6, nchar(whole)-1)
    line <- paste(var, ".setddt(", trim(convertvariables(text[2])), ");\n", sep="")
  } else {
    line <- paste("TemporalVariable& ", trim(text[1]), " = ", text[2], ";\n")
  }

  line
}

rowcompile.modelname <- function() {
    paste("ROW", paste(floor(runif(10, 0, 10)), collapse=""), sep="")
}

rowcompile.model <- function(model, modelname) {
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
      else {
          myunits <- paste("Units::", model$constants[[var]]$units, sep="")
          if (model$constants[[var]]$units == ru.peryr)
              myunits <- "1 / Units::yr"
          inits <- c(inits, paste(tocamel(var), "(\"", var, "\", ", model$constants[[var]]$value, ", ", myunits, ", *this)", sep=""))
      }
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

    text <- gsub("\\$\\{EQUATIONS\\}", equationstext, text)
    
    filename <- paste(modelname, "Model.h", sep="")
    writeLines(text, con=filename)
}

rowcompile.main <- function(model, modelname) {
    template <- "row/inst/extdata/main.cpp"
    #template <- system.file("main.cpp", package="row")
    text <- readLines(template, encoding="UTF-8")

    text <- gsub("\\$\\{MODEL\\}", modelname, text)

    varlist <- c(names(model$stocks), names(model$namedvars))

    text <- gsub("\\$\\{VARLISTCSV\\}", paste(varlist, collapse=","), text)
    text <- gsub("\\$\\{VARLISTCOUT\\}", paste("vars[\"", paste(varlist, collapse="\"] << vars[\""), "\"]", sep=""), text)

    steps <- model$steps
    if (is.null(steps))
        steps <- 100
    
    text <- gsub("\\$\\{STEPS\\}", steps, text)

    substep <- model$substep
    if (is.null(substep))
        substep <- .01
    
    text <- gsub("\\$\\{SUBSTEP\\}", substep, text)

    filename <- paste("main", modelname, ".cpp", sep="")
    writeLines(text, con=filename)
}

rowcompile.makefile <- function(model, modelname) {
    template <- "row/inst/extdata/makefile"
    #template <- system.file("makefile", package="row")
    text <- readLines(template, encoding="UTF-8")

    text <- gsub("\\$\\{MODEL\\}", modelname, text)

    writeLines(text, con="makefile")
}

rowcompile <- function(model) {
    modelname <- rowcompile.modelname()
    rowcompile.model(model, modelname)
    rowcompile.main(model, modelname)
    rowcompile.makefile(model, modelname)
    system("make")
    system(paste("./main", modelname, " > output", modelname, ".csv", sep=""))
    read.csv(paste("output", modelname, ".csv", sep=""))
}    
    

stocks <- list(population=rv(100, "individuals"))
constants <- list(growth.rate=rv(.1, ru.peryr))

model <- rowmodel(constants, stocks) +
  (diff(population) ~ growth.rate * population)

results <- rowcompile(model)

plot(results$time, results$population)
