#' Adding parameters
#' 
#' This function allows users to specify values of parameters used in the reaction (if parameters are used). The current 
#' sysBio version assumes that parameters, if defined, will remain constant during simulation. Thus, only numerical values 
#' can be assigned to parameter values.
#' 
#' @param x  model to which the parameter is added (required)
#' @param pName parameter name (required)
#' @param iVal parameter value; if the value is not specified, sysBio will set it to 1
#' @param overwrite a flag that allows changes to the existing parameter (default value FALSE)
#'     
#' @return This function adds information about parameters into the model (given as a first argument of the function). 
#'     Parameters information is stored in the list format and contain the following elements:
#'     \itemize{
#'     \item{parameters$pName - parameter name}
#'     \item{parameters$initVal - parameter value}
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
#' # Show info about model reactions and parameters
#' exmp$reaction
#' exmp$parameters
#' 
#' @export
#' 

#addParameters.function <- function(x, pName=NA, iVal="1", overwrite=FALSE){
addParameters <- function(x, pName=NA, iVal="1", overwrite=FALSE){
  
  if (!exists(deparse(substitute(x))))
    stop("Specified model does not exist!")
  
  if (is.na(pName))
    stop("Parameter name is not specified!")
  
  y <- x
  
  # Current number of parameters
  curNumber <- length(y$parameters)
  
  # Check if species with the same name exist
  if (curNumber > 0){
    if (pName %in% y$parameters$pName){
      
      print("Parameter with the same name already exist in the model!")
      
      if (overwrite){
        getIndex <- which(y$parameters$pName == pName)
        
        if (y$parameters$initVal[getIndex] != iVal){
          y$parameters$initVal[getIndex] <- iVal
          print("Initial value has been replaced")
        } else {
          print("The new initial value is the same as the old one")
        }
        
      } else {
        stop("Specify a different parameter or set overwrite flag to TRUE!")
      }
    } else {
      y$parameters$pName <- c(y$parameters$pName, pName)    
      y$parameters$initVal <- c(y$parameters$initVal, iVal) 
    }
  } else {
    y$parameters <- list(pName=pName, initVal=iVal)  
  } 
  
  y$isChecked <- 0
  
  assign(deparse(substitute(x)), y, envir = .GlobalEnv)  
}

#addParameters <- cmpfun(addParameters.function)
#rm(addParameters.function)
