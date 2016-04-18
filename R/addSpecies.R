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

addSpecies.function <- function(x, spName=NA, iVal="0", overwrite=FALSE){
  
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

addSpecies <- cmpfun(addSpecies.function)
rm(addSpecies.function)
