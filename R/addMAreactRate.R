# addMAreactRate function adds a reaction rate into an existing model.
# Reaction rate can be specified as a fixed (constant value) or as an
# assigned (formula). 
# The plan is to add an option for an ordinary differential equation later.
#
# This file is part of the R sysBio package. 
#
# sysBio package is free software and is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

addMAreactRate.function <- function(x, rr=NA, type="fixed", val="1", overwrite=FALSE){
  
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

addMAreactRate <- cmpfun(addMAreactRate.function)
rm(addMAreactRate.function)
