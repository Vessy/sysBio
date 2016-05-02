#' Creating representation for reaction parameters
#' 
#' This function creates assignment (in the format required for ODE solver) for each each of the specified 
#' paramterers based on their assigned/specified value. This function is called from the "makeModel.R" function.
#' 
#' @param x  model name (required)
#'     
#' @return This function returns model parameters in the format appropriate for the ODEs solver
#'     


#makeODEParameters.function <- function(x){
makeODEParameters <- function(x){
  
  hlp <- c()
  
  # Get the number of parameters (with initial values)
  par.iv.length <- length(x$parameters$initVal)
  
  # Create an assignment for each parameter (assign it its initial value)
  if (par.iv.length > 0)
    for (i in 1:par.iv.length)
      hlp <- c(hlp, paste(as.character(x$parameters$pName[i]), " <- ", x$parameters$initVal[i]))
  
  hlp
}

#makeODEParameters <- cmpfun(makeODEParameters.function)
#rm(makeODEParameters.function)
