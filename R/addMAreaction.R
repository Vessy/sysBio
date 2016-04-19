# addMAreaction function adds a mass action reaction into an existing model.
# The reaction can be reversible (=) or non-reversible (->).
# If the reaction is reversible, two parameters (reaction rates) are required,
# otherwise only one parameter is required. 
# If reaction rates are not specified, they will be assigned value 1.
#
# This file is part of the R sysBio package. 
#
# sysBio package is free software and is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

addMAreaction.function <- function(x, react=NA, r1=NA, r2=NA, name=NA, overwrite=FALSE){
  
  if (!exists(deparse(substitute(x))))
    stop("Specified model does not exist!")
  
  if (is.na(react))
    stop("Reaction is missing!")
  
  t1 <- str_detect(react, "->")
  t2 <- str_detect(react, "=")
  
  if (!(t1 | t2))
    stop("Reaction is not in the correct format! Use -> sign to describe the first order reaction or = sign to describe reversible reaction.")
  
  y <- x
  
  if (is.na(name))
    print("You did not specify the name of the reaction. We will assign a generic name to this reaction (e.g., Reaction_number).
          This could lead to some problems (e.g., model could contain two reactions that are the same, but named differently).")
  
  # Current number of reactions
  curNumber <- length(y$reaction)
  
  # Check if reaction with the same name exist
  if (curNumber > 0){
    if (name %in% y$reaction$rName){
      
      print("Reaction with the same name already exist in the model!")
      
      if (overwrite){
        getIndex <- which(y$reaction$rName == name)
        
        y$reaction$reaction[getIndex] <- react
        y$reaction$r1[getIndex] <- r1
        y$reaction$r2[getIndex] <- r2
        
        print("Reaction rate(s) have been replaced")
      } else {
        stop("Specify a different reaction or set overwrite flag to TRUE!")
      }
    } else {
      y$reaction$rName <- c(y$reaction$rName, ifelse(is.na(name), paste("Reaction", length(y$reaction$rName)+1, sep="_"), name))
      y$reaction$reaction <- c(y$reaction$reaction, react)
      y$reaction$r1 <- c(y$reaction$r1, ifelse(is.na(r1), paste("r", length(y$reaction$r1)+length(y$reaction$r2)+1, sep="_"), r1))
      
      if (t2)
        y$reaction$r2 <- c(y$reaction$r2, ifelse(is.na(r2), paste("r", length(y$reaction$r1)+length(y$reaction$r2)+1, sep="_"), r2))
      
      y$reaction$type <- c(y$reaction$type, "Mass Action")  
    }
  } else {
    if (t2){
      y$reaction <- list(rName=ifelse(is.na(name), "Reaction_1", name), reaction=react, r1=ifelse(is.na(r1), "r_1", r1), r2=ifelse(is.na(r2), "r_2", r2), type="Mass Action")  
    } else {
      y$reaction <- list(rName=ifelse(is.na(name), "Reaction_1", name), reaction=react, r1=ifelse(is.na(r1), "r_1", r1), r2=c(), type="Mass Action")  
    }
  }
  
  y$isChecked <- 0
  
  assign(deparse(substitute(x)), y, envir = .GlobalEnv)  
}

addMAreaction <- cmpfun(addMAreaction.function)
rm(addMAreaction.function)
