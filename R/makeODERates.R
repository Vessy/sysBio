#' Creating representation for reaction rates
#' 
#' This function  creates assignment (in the format required for ODE solver) for each of the specified reaction rates 
#' based on their assigned/specified value.  This function is called from the "makeModel.R" function.
#' 
#' @param x  model name (required)
#'     
#' @return This function returns reaction rates of the model in the format appropriate for the ODEs solver
#'  

#makeODERates.function <- function(x){
makeODERates <- function(x){
  
  hlp <- c()
  
  # Get the number of values for rates
  rates.length <- length(x$rates$rVal)
  
  # Sort species names and get the number of species
  sList.sorted <- x$species$sName[order(nchar(x$species$sName), x$species$sName)]
  sList.length <- length(sList.sorted)
  
  # If there are any rates, create string(s) that represent the rate assignments/values
  if (rates.length > 0)
    for (i in 1:rates.length){
      
     # For fixed type use whatever value was provided
      if (toupper(x$rates$rType[i]) == "FIXED")
        hlp <- c(hlp, paste(as.character(x$rates$rrName[i]), " <- ", x$rates$rVal[i]))
      
    # For assigned type use derivatives that correspond for assigned species (e.g., dA for A)  
    
      if (toupper(x$rates$rType[i]) == "ASSIGNED"){
        
        hlp2 <- x$rates$rVal[i]
        
        for (j in sList.length:1)
          hlp2 <- stringr::str_replace_all(hlp2, sList.sorted[j], paste("y[", which(x$species$sName == sList.sorted[j]), "]", sep=""))
        
        hlp <- c(hlp, paste(as.character(x$rates$rrName[i]), " <- ", hlp2 ))
      }
    }
  
  
  hlp
}

#makeODERates <- cmpfun(makeODERates.function)
#rm(makeODERates.function)
