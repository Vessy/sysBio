#' Adding molecular reactions.
#' 
#' Molecular reactions can be described by differential equation that define the rate of change in molecular concentrations. addMAreaction function
#' transforms reactions into ODEs using the law of mass action. Given a reaction: k1A + k2B -(r)-> k3*AB, where A and B represent reactants and AB 
#' represents a product (in general, we can call A, B, and AB species), r represents the reaction rate, and k1, k2, and k3 represent stoichometric 
#' coefficients, one can use the law of mass action to determine the change in the amount of each species as: dA/dt = dB/dt = - dAB/dt = -rA^(k1)B^(k2).
#' Or in general, given a reaction sum(i=1:m)[k{i}S{i}] -(r)-> sum(j=1:n)[l{j}S{j}], changes in the amount of each species can be represented as 
#' dS/dt = r(l{i}-k{i})prod(i=1:m)S^(k{i}). 
#' addMAreaction function adds a mass action reaction into an existing model. sysBio allows users to write reactions in two formats: 1. as forward 
#' (non-reversible) reactions, e.g., A -> B, with rate r, 2. as reversible reactions, e.g., A = B, with forward rate r1 and reverse rate r2. 
#' sysBio transforms and  interprets the reversible reactions as two forward reactions, e.g., A -> B, with rate r1, and B -> A, with rate r2.
#' 
#' @param x  model to which the reaction is added (required)
#' @param react reaction to be added (required). sysBio uses the "->" symbol 
#'     for forward reaction and the "=" symbol for the reversible reaction; 
#'     values that represent stoichometric coefficients should be explicitly 
#'     included in the reaction definition (e.g., 2A + B -> 1.5AB); sysBio 
#'     uses a term null to describe a null species (source or sink).
#' @param r1 reaction rate (forward); if the name is not specified, sysBio 
#'     will assign a generic name to the reaction rate, in the format 
#'     r_{reaction_rate_number}
#' @param r2 reaction rate (reversible); if the name is not specified, 
#'     sysBio will assign a generic name to the reaction rate, in the format 
#'     r_{reaction_rate_number}
#' @param name name of the reaction; if the name is not specified, sysBio 
#'     will assign a generic name to the reaction, in the format 
#'     Reaction_{reaction_number}
#' @param overwrite a flag that allows changes to the existing reaction 
#'     (default value FALSE)
#'     
#' @return This function adds information about the reaction into the model (given as a first argument of the function). 
#'     Reaction information is stored in the list format and contain the following elements:
#'     \itemize{
#'     \item{reaction$rName - name of the reaction}
#'     \item{reaction$reaction - reaction}
#'     \item{reaction$r1 - forward rate of the reaction}
#'     \item{reaction$r2 - reverse rate of the reaction (if available, NULL otherwise)}
#'     \item{reaction$type - asigned type "Mass Action"}
#'     }
#'     
#' @examples
#' exmp <- newModel("This is an example of a new model")
#' addMAreaction(exmp, react="A = null", "rf", "rb")
#' addMAreaction(exmp, react="A + B -> 2*AB", "k", name="Forward AB")
#' addMAreaction(exmp, react="AB -> null", "rAB")
#' 
#' # Show info about model reactions
#' exmp$reaction
#' 
#' @export

#addMAreaction.function <- function(x, react=NA, r1=NA, r2=NA, name=NA, overwrite=FALSE){
addMAreaction <- function(x, react=NA, r1=NA, r2=NA, name=NA, overwrite=FALSE){
  
  if (!exists(deparse(substitute(x))))
    stop("Specified model does not exist!")
  
  if (is.na(react))
    stop("Reaction is missing!")
  
  t1 <- stringr::str_detect(react, "->")
  t2 <- stringr::str_detect(react, "=")
  
  if (!(t1 | t2))
    stop("Reaction is not in the correct format! Use -> sign to describe the first order reaction or = sign to describe reversible reaction.")
  
  y <- x
  
  if (is.na(name))
    print("You did not specify the name of the reaction. We will assign a generic name to this reaction (e.g., Reaction_reactionNmber). This could lead to some problems (e.g., model could contain two reactions that are the same, but named differently).")
  
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

#addMAreaction <- compiler::cmpfun(addMAreaction.function)
#rm(addMAreaction.function)
