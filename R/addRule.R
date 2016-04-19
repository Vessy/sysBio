# addRule function adds a rule into an existing model.
# A rule defines a change in a species values that depends on other species or parameters. 
#
# This file is part of the R sysBio package. 
#
# sysBio package is free software and is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

addRule.function <- function(x, rName=NA, type=NA, rule=NA, overwrite=FALSE){
  
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

addRule <- cmpfun(addRule.function)
rm(addRule.function)

