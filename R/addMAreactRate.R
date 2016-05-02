#' Adding reaction rates
#' 
#' This function allows users to define reaction rate. Reaction rate can be 
#' defined as a fixed numerical value (e.g., r=m1, where m1 represents a 
#' constant parameter, m1=0.9, or just as r=0.9) or initial assignments 
#' (e.g., r=m1*A, where m1 represents a constant parameter and A represents 
#' a species).
#' 
#' @param x  model to which the reaction rate is added (required)
#' @param rr reaction rate name (required),
#' @param type reaction rate type; currently supported options are fixed and assigned; 
#'     if the value is not specified, sysBio will set type to fixed
#' @param val reaction rate value; if defined as fixed, the value should be specified as a number. 
#'     e.g., 0.1; if defined as assigned, the value can be defined in form of an assignment (function), 
#'     e.g., B or 10*A. If the value is not specified, sysBio will set the value to 1
#' @param overwrite a flag that allows changes to the existing reaction rate (default value FALSE)
#'     
#' @return This function adds information about reaction rates into the model (given as a first argument of the function). 
#'     Reaction rates information is stored in the list format and contain the following elements:
#'     \itemize{
#'     \item{rates$rrName - reaction rate name}
#'     \item{rates$rType - reaction rate type}
#'     \item{rates$rVal - reaction rate value}
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
#'  
#' # Show info about model reactions and rates
#' exmp$reaction
#' exmp$rates
#' 
#' @export
#' 

#addMAreactRate.function <- function(x, rr=NA, type="fixed", val="1", overwrite=FALSE){
addMAreactRate <- function(x, rr=NA, type="fixed", val="1", overwrite=FALSE){  
  if (!exists(deparse(substitute(x))))
    stop("Specified model does not exist!")
  
  if (is.na(rr))
    stop("Reaction rate is not specified!")
  
  
  if (!(type %in% c("fixed", "assigned"))) 
    stop("Reaction type is not in the correct format!")
  # Add option to use odes
  
  y <- x
  
  # Current number of species
  curNumber <- length(y$rates)
  
  # Check if a reaction rate with the same name exist
  if (curNumber > 0){
    if (rr %in% y$rates$rrName){
      
      print("Rate with the same name already exist in the model!")
      
      if (overwrite){
        getIndex <- which(y$rates$rrName == rr)
        
        if ((y$rates$rType[getIndex] != type) | (y$rates$rVal[getIndex] != val)){
          y$rates$rType[getIndex] <- type
          y$rates$rVal[getIndex] <- val
          print("Initial value and/or type of the reaction have been replaced")
        } else {
          print("The new initial value and/or type of the reaction are the same as the old one")
        }
        
      } else {
        stop("Specify a different reaction name or set overwrite flag to TRUE!")
      }
    } else {
      y$rates$rrName <- c(y$rates$rrName, rr)
      y$rates$rType <- c(y$rates$rType, type)
      y$rates$rVal <- c(y$rates$rVal, val)   
    }
  } else {
    y$rates <- list(rrName=rr, rType=type, rVal=val) 
  }
  
  y$isChecked <- 0
  
  assign(deparse(substitute(x)), y, envir = .GlobalEnv)  
}

#addMAreactRate <- cmpfun(addMAreactRate.function)
#rm(addMAreactRate.function)
