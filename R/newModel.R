#' Creating a new model
#' 
#' Creating a new model object. This object will contain all 
#' components of the model and information required to run 
#' simulation.
#' 
#' @param x  name of the model (required)
#'    
#' @return This function adds information about rules into the model (given as a first argument of the function). 
#'     Rules information is stored in the list format and contain the following elements:  
#'     \itemize{
#'     \item{modelName - name of the model}
#'     \item{reaction - list of reactions (empty)}
#'     \item{species - list of species (empty)}
#'     \item{rates - list of reaction rates (empty)}
#'     \item{parameters - list of reaction parameters (empty)}
#'     \item{events - list of events (empty)}
#'     \item{rules - list of rules (empty)}
#'     \item{model - mathematical model of the system (empty)}
#'     \item{odes - mathematical model of the system in ODEs format (empty)}
#'     \item{stochMatrix - stochastic matrix (empty)}
#'     \item{stochModel - stochastic model (propensity function) (empty)}
#'     \item{isChecked - a flag that marks that changes have been done to the model, e.g., new reaction or species added (set to value 0)}
#'     }
#'     
#' @examples
#' exmp <- newModel("This is an example of a new model")
#' 
#' # Show info about model
#' exmp
#' 
#' @export
#' 



#newModel.function <- function(x){
newModel <- function(x){
  list(modelName=x, reaction=c(), species=c(), rates=c(), parameters=c(), events=c(), rules=c(), model=c(), odes=c(), stochMatrix=c(), stochModel=c(), isChecked=0)
  
}

#newModel <- cmpfun(newModel.function)
#rm(newModel.function)
