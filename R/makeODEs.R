# makeODEs function transforms ODEs describing the giving model into the R 
# format required by ODE solver.
# in a given time interval.
# This function is called from the "simulateModel.R" function.
#
# This file is part of the R sysBio package. 
#
# sysBio package is free software and is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#


makeODEs.function <- function(x){
  
  if (!exists(deparse(substitute(x))))
    stop("Specified model does not exist!")
  
  hlpFun <- NULL
  
  isValidated <- 0
  
  if (x$isChecked == 0){
    print ("Model not validated... Validating model...")
    isValidated <- validateModel(x)
  }
  
  if ((x$isChecked == 1) | (isValidated == 1)){
  #if (x$isChecked == 1){
    parameters.length <- length(x$odes$parameters)
    rates.length <- length(x$odes$rates)
    
    odes.length <- length(x$odes$equations) + length(x$odes$rules)
    
    if ((parameters.length > 0) & (rates.length > 0)){
      hlpFun <- function(t, y, dy, params){
        
        r <- rep(0, length(dy))
        
        eval(parse(text=x$odes$parameters))
        
        eval(parse(text=paste(as.character(x$rates$rrName), " <- " , x$odes$rates, sep="")))
        
        eval(parse(text=c(x$odes$equations, x$odes$rules)))
        
        list(r)
      }
    }
    
    if ((parameters.length == 0) & (rates.length > 0)){
      hlpFun <- function(t, y, dy, params){
        
        r <- rep(0, length(dy))
        
        eval(parse(text=paste(as.character(x$rates$rrName), " <- " , x$odes$rates, sep="")))
        
        eval(parse(text=c(x$odes$equations, x$odes$rules)))
        
        list(r)
      }
    }
  }
  else {
    print("You need to make a model first (see makeModel function)")
  }
  
  hlpFun  
}

makeODEs <- cmpfun(makeODEs.function)
rm(makeODEs.function)
