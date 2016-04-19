# addSpecies function adds species into an existing model.
# It also provides option to specify the species initial
# value (if not specified, it assumes that the value is 0).
#
# This file is part of the R sysBio package. 
#
# sysBio package is free software and is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

addParameters.function <- function(x, pName=NA, iVal="0", overwrite=FALSE){
  
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

addParameters <- cmpfun(addParameters.function)
rm(addParameters.function)
