# simulateModel function runs a set of ODEs that describe the given model
# in a given time interval.
#
# This file is part of the R sysBio package. 
#
# sysBio package is free software and is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

simulateModel.function <- function(x,times=seq(0, 10, by = 0.1)){
  
  if (!exists(deparse(substitute(x))))
    stop("Specified model does not exist!")
  
  if (validateModel(x) == 1){
  
    # Check if vector dimensions match
    
    dyini <-rep(0, length(x$odes$equations))
    yini <- eval(parse(text=paste("c(", paste(x$species$sName, "=", x$species$initVal, sep="", collapse=", "), ")", sep="")))
    
    ODEresult <- daspk(y=yini, dy=dyini, times=times, res=makeODEs(x))
    
    as.data.frame(ODEresult)
  }
}

simulateModel<- cmpfun(simulateModel.function)
rm(simulateModel.function)
