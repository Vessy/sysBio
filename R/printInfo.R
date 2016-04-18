# printInfo function prints out info about model, e.g.,  
# name, number of reactions and their types, numper of species,
# number of ODEs, etc.
#
# This file is part of the R sysBio package. 
#
# sysBio package is free software and is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

printInfo.function <- function(x, allDetails=FALSE){
  
  if (!exists(deparse(substitute(x))))
    stop("Specified model does not exist!")
  
  print(paste("Model name: ", x$modelName, sep=""))
  print(paste("Number of reactions: ", length(x$reaction$rName), sep=""))
  print(paste("Reaction type(s): ", unique(x$reaction$type), sep="", collapse=", "))
  print(paste("Number of species: ", length(x$species$sName), sep=""))
  print(paste("Model can be represented in form of ", length(x$model), " differential equation(s)", sep=""))
  
  if (length(x$rules$rRule) > 0){
    print(paste("Number of rules: ", length(x$rules$rRule), sep=""))
  }
  
  
  if (allDetails){
    
    print("Reactions:")
    print(paste(x$reaction$rName, x$reaction$reaction, sep=": "))
    
    print("Model equations:")
    print(x$model)
    
    print("Model rates:")
    print(paste(x$rates$rrName, x$rates$rVal, sep="="))
    
    print("Initial values - species:")
    print(paste(x$species$sName, x$species$initVal, sep="="))
    
    print("Initial values - parameters:")
    print(paste(x$parameters$pName, x$parameters$initVa, sep="="))
    
    if (length(x$rules$rRule) > 0){
      print("Rules:")
      print(paste(x$rules$rName, x$rules$rRule))
    }
    
  } 
}

printInfo <- cmpfun(printInfo.function)
rm(printInfo.function)
