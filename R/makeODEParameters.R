# makeODEParameters function creates assignment (in the format required for ODE solver)
# for each each of the specidied paramterer based on its assigned/specified value.
# This function is called from the "makeModel.R" function.
#
# This file is part of the R sysBio package. 
#
# sysBio package is free software and is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

makeODEParameters.function <- function(x){
  
  hlp <- c()
  
  # Get the number of parameters (with initial values)
  par.iv.length <- length(x$parameters$initVal)
  
  # Create an assignment for each parameter (assign it its initial value)
  if (par.iv.length > 0)
    for (i in 1:par.iv.length)
      hlp <- c(hlp, paste(as.character(x$parameters$pName[i]), " <- ", x$parameters$initVal[i]))
  
  hlp
}

makeODEParameters <- cmpfun(makeODEParameters.function)
rm(makeODEParameters.function)
