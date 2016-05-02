#' Model simulation (ordinary differential equation solver)
#' 
#' This function allows users to simulate the model (i.e., solve ODEs) using a differential equation solver 
#' based on a combination of backward differentiation formula and a direct linear system solution method 
#' (using the "daspk" function from the deSolve package).
#' 
#' @param x  model name (required)
#' @param times time points in which model will be simulated 
#'     
#' @return Data frame where each row corresponds to a time point in which model was simulated. First column corresponds to 
#'     time, while the rest of columns correspond to model species, i.e., values assigned to those species at a given time point.
#'     
#' @examples
#' exmp <- newModel("This is an example of a new model")
#' addMAreaction(exmp, react="A = null", "rf", "rb")
#' addMAreaction(exmp, react="A + B -> 2*AB", "k", name="Forward AB")
#' addMAreaction(exmp, react="AB -> null", "rAB")
#' 
#' addMAreactRate(exmp, "rf", "fixed", "1")
#' addMAreactRate(exmp, "rb", "fixed", "0.75")
#' addMAreactRate(exmp, "k", "fixed", "0.5")
#' addMAreactRate(exmp, "rAB", "assigned", "p1*A")
#' 
#' addParameters(exmp, "p1", 0.75)
#'  
#' addSpecies(exmp, "A", 10)
#' addSpecies(exmp, "B", 10)
#' addSpecies(exmp, "AB", 0)
#' 
#' addRule(exmp, "rule B", "ODEs", "B=-0.1*AB")
#' 
#' makeModel(exmp)
#'   
#' simResults <-simulateModel(exmp)
#' plotResults(simResults, title="Simulation results")
#' 
#' @export
#' 

#simulateModel.function <- function(x,times=seq(0, 10, by = 0.1)){
simulateModel <- function(x,times=seq(0, 10, by = 0.1)){
  
  if (!exists(deparse(substitute(x))))
    stop("Specified model does not exist!")
  
  if (validateModel(x) == 1){
  
    # Check if vector dimensions match
    
    dyini <-rep(0, length(x$odes$equations))
    yini <- eval(parse(text=paste("c(", paste(x$species$sName, "=", x$species$initVal, sep="", collapse=", "), ")", sep="")))
    
    ODEresult <- deSolve::daspk(y=yini, dy=dyini, times=times, res=makeODEs(x))
    
    as.data.frame(ODEresult)
  }
}

#simulateModel<- cmpfun(simulateModel.function)
#rm(simulateModel.function)
