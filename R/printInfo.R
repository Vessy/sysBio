#' Print information about the model
#' 
#' This function prints out info about mode: model name, number of reactions and their types, number of species, and number of ODEs required to
#' represent the model. It also allows users to print detailed info about each of the reactions, species, reaction rates, parameters, rules, 
#' and model equations.
#' 
#' @param x a model name
#' @param allDetails a flag that specify whether the user wants short or long description of the model (flag set to FALSE).
#' 
#' @return Textual summary
#' 
#' @examples
#' exmp <- newModel("This is an example of a new model")
#' addMAreaction(exmp, react="A = null", "rf", "rb")
#' addMAreaction(exmp, react="A + B -> 2*AB", "k", name="Forward AB")
#' addMAreaction(exmp, react="AB -> null", "rAB")
#' 
#' addMAreactRate(exmp, "rf", "fixed", "1")
#' addMAreactRate(exmp, "rb", "fixed", "0.75")
#' addMAreactRate(exmp, "k", "fixed", "0.5")
#' addMAreactRate(exmp, "rAB", "assigned", "p1*A")
#' 
#' addParameters(exmp, "p1", 0.75)
#'  
#' addSpecies(exmp, "A", 10)
#' addSpecies(exmp, "B", 10)
#' addSpecies(exmp, "AB", 0)
#' 
#' addRule(exmp, "rule B", "ODEs", "B=-0.1*AB")
#' 
#' makeModel(exmp)
#'   
#' printInfo(exmp)
#' printInfo(exmp, allDetails=TRUE)
#' 
#' @export
#' 

#printInfo.function <- function(x, allDetails=FALSE){
printInfo <- function(x, allDetails=FALSE){
  
  if (!exists(deparse(substitute(x))))
    stop("Specified model does not exist!")
  
  print(paste("Model name: ", x$modelName, sep=""))
  print(paste("Number of reactions: ", length(x$reaction$rName), sep=""))
  print(paste("Reaction type(s): ", unique(x$reaction$type), sep="", collapse=", "))
  print(paste("Number of species: ", length(x$species$sName), sep=""))
  print(paste("Model can be represented in form of ", length(x$model), " differential equation(s)", sep=""))
  
  if (length(x$rules$rRule) > 0){
    print(paste("Number of rules: ", length(x$rules$rRule), sep=""))
  }
  
  
  if (allDetails){
    
    print("Reactions:")
    print(paste(x$reaction$rName, x$reaction$reaction, sep=": "))
    
    print("Model equations:")
    print(x$model)
    
    print("Model rates:")
    print(paste(x$rates$rrName, x$rates$rVal, sep="="))
    
    print("Initial values - species:")
    print(paste(x$species$sName, x$species$initVal, sep="="))
    
    print("Initial values - parameters:")
    print(paste(x$parameters$pName, x$parameters$initVa, sep="="))
    
    if (length(x$rules$rRule) > 0){
      print("Rules:")
      print(paste(x$rules$rName, x$rules$rRule))
    }
    
  } 
}

#printInfo <- cmpfun(printInfo.function)
#rm(printInfo.function)
