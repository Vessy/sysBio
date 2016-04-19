# newModel function creates a data structure that stores a new model
#
# This file is part of the R sysBio package. 
#
# sysBio package is free software and is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
 
newModel.function <- function(x){
  
  list(modelName=x, reaction=c(), species=c(), rates=c(), parameters=c(), events=c(), rules=c(), model=c(), odes=c(), stochMatrix=c(), stochModel=c(), isChecked=0)
  
}

newModel <- cmpfun(newModel.function)
rm(newModel.function)
