#' Adding species
#' 
#' This function allows users to specify initial values for species within the model. 
#' Model must contain at least one species. Initial values for all species, as well as all rates and 
#' parameters have to be defined. sysBio uses a term null to describe a null species (source or sink); 
#' null species do not need to be defined. 
#' 
#' @param x  model to which the species is added (required)
#' @param spName species name (required)
#' @param iVal initial value; if the value is not specified, sysBio will set initial value to 0
#' @param overwrite a flag that allows changes to the existing species (default value FALSE)
#'     
#' @return This function adds information about species into the model (given as a first argument of the function). 
#'     Species information is stored in the list format and contain the following elements:
#'     \itemize{
#'     \item{species$sName - species name}
#'     \item{species$initVal - initial value}
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
#' # Show info about model reactions and species
#' exmp$reaction
#' exmp$species
#' 
#' @export
#' 

#addSpecies.function <- function(x, spName=NA, iVal="0", overwrite=FALSE){
addSpecies <- function(x, spName=NA, iVal="0", overwrite=FALSE){
  
  if (!exists(deparse(substitute(x))))
    stop("Specified model does not exist!")
  
  if (is.na(spName))
    stop("Species name is not specified!")
  
  y <- x
  
  # Current number of species
  curNumber <- length(y$species)
  
  # Check if species with the same name exist
  if (curNumber > 0){
    if (spName %in% y$species$sName){
      
      print("Species with the same name already exist in the model!")
      
      if (overwrite){
        getIndex <- which(y$species$sName == spName)
        
        if (y$species$initVal[getIndex] != iVal){
          y$species$initVal[getIndex] <- iVal
          print("Initial value has been replaced")
        } else {
          print("The new initial value is the same as the old one")
        }
        
      } else {
        stop("Specify a different species or set overwrite flag to TRUE!")
      }
    } else {
      y$species$sName <- c(y$species$sName, spName)    
      y$species$initVal <- c(y$species$initVal, iVal)   
    }
  } else {
    y$species <- list(sName=spName, initVal=iVal)  
  }
  
  y$isChecked <- 0
  
  assign(deparse(substitute(x)), y, envir = .GlobalEnv)  
}

#addSpecies <- cmpfun(addSpecies.function)
#rm(addSpecies.function)
