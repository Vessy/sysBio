#' Adding rules
#' 
#' This function allows user to define rules. Rules can be used to specify values for model species 
#' based on the values of other components (mainly species) in the model. Current version allows only 
#' for the ODEs type of rules.
#' 
#' @param x  model to which the rule is added (required)
#' @param rName - rule name (required)
#' @param type rule type; currently only type ODEs is supported (rules in the form of ODEs) (required)
#' @param rule rule definition (required)
#' @param overwrite a flag that allows changes to the existing rule (default value FALSE)
#'     
#' @return This function adds information about rules into the model (given as a first argument of the function). 
#'     Rules information is stored in the list format and contain the following elements:  
#'     \itemize{
#'     \item{rule$rName - rule name}
#'     \item{rule$rType - rule type (ODEs)}
#'     \item{rule$rRule - rule definition}
#'     }
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
#' # Show info about model reactions and rules
#' exmp$reaction
#' exmp$rule
#' 
#' @export
#' 

#addRule.function <- function(x, rName=NA, type=NA, rule=NA, overwrite=FALSE){
addRule <- function(x, rName=NA, type=NA, rule=NA, overwrite=FALSE){
  
  if (!exists(deparse(substitute(x))))
    stop("Specified model does not exist!")
  
  if (is.na(rName) | is.na(type) | is.na(rule))
    stop("Rule name and/or rule and/or type is/are not specified!")
  
  if (!(type %in% c("ODEs"))) 
    stop("Rule type is not in the correct format! Currently, the only supported format is ODEs")

  # TDB - expand supported formats
  # Algebraic format should suport numerical solution (may not give an exact solution, but approximation)
  #if (!(type %in% c("initial", "repeated", "algebraic", "ODEs"))) 
  #  stop("Rule type is not in the correct format! Allowed formats are: initial, repeated, algebraic, and ODEs")
  
  y <- x
  
  # Current number of rules
  curNumber <- length(y$rules)
  
  # Check if species with the same name exist
  if (curNumber > 0){
    if (rName %in% y$rules$rName){
      
      print("Rule with the same name already exist in the model!")
      
      if (overwrite){
        getIndex <- which(y$rules$rName == rName)
        
        if ((y$rules$rType[getIndex] != type) | (y$rules$rRule[getIndex] != rule)){
          y$rules$rType[getIndex] <- type
          y$rules$rRule[getIndex] <- rule
          print("Rule type and/or rule itself have been replaced")
        } else {
          print("The new rule and its type are the same as the old ones")
        }
        
      } else {
        stop("Specify a different rule or set overwrite flag to TRUE!")
      }
    } else {
      y$rules$rName <- c(y$rules$rName, rName) 
      y$rules$rType <- c(y$rules$rType, type) 
      y$rules$rRule <- c(y$rules$rRule, rule)  
    }
  } else {
    y$rules<- list(rName=rName, rType=type, rRule=rule)   
  }
  
  y$isChecked <- 0
  
  assign(deparse(substitute(x)), y, envir = .GlobalEnv)  
}

#addRule <- cmpfun(addRule.function)
#rm(addRule.function)

