#' Creating a model
#' 
#' This function creates mathematical model of the system, a set 
#' of differential equations that correspond to that mathematical 
#' model, stochastic matrix, and propensity function. It also 
#' transforms all defined rates, parameters, and rules into a 
#' format required by the ODEs solver function.
#' 
#' @param x  model name (required)
#'     
#' @return This function adds updates model (given as an argument 
#'     of the function) with information about mathematical model 
#'     of the system, ODEs, stochastic matrix, and propensity 
#'     function. This function also updates model's isChecked 
#'     flag, as model is created based on the most recent data. 
#'     It updates the following elements:
#'     \itemize{
#'     \item{model - mathematical model of the system}
#'     \item{odes$equations - a set of ODEs that correspond to the mathematical model of the system (in format for ODEs solver)}
#'     \item{odes$rates - reaction rates assignments (in format for ODEs solver)}
#'     \item{odes$parameters - parameters assignments (in format for ODEs solver)}
#'     \item{odes$rules - rules assignments (in format for ODEs solver)}
#'     \item{stochMatrix - stochastic matrix (rows correspond to species, columns to reactions)}
#'     \item{stochModel - propensity function (one equation for each reaction)}
#'     \item{isChecked - flag set to 1}
#'     }
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
#' # Show info about the model
#' exmp
#' 
#' makeModel(exmp)
#'   
#' # Show info about the updated model
#' exmp
#' 
#' @export
#' 

#makeModel.function <- function(x){
makeModel <- function(x){
  
  if (!exists(deparse(substitute(x))))
    stop("Specified model does not exist!")
  
  # We'll use y to make all modifications
  y <- x
  
  # Check if there's any reaction in the model
  numR <- length(y$reaction$rName)
  
  if (numR > 0)
  {
    # Create temporary data frame
    tmpDF1 <- data.frame(stoch=c(), species=c(), side=c(), reaction=c(), product=c(), odeSpecies=c(), odeProduct=c()) 
    
    # Temp variable (counter) for reversible reaction case
    cnt.r2 <- 1
    
    for (i in 1:numR){
      
      # Remove spaces from the reaction 
      react.cln <- gsub(" ","", y$reaction$reaction[i] , fixed=TRUE)      
      
      # Check if it is reversible reaction
      whichOne <- length(strsplit(react.cln, "\\->")[[1]])
      
      if (whichOne == 1) #Reversible reaction
      {
        # Get both sides of reaction
        x.react <- strsplit(react.cln, "\\=")[[1]]
        
        if (length(x.react) > 2)
          print("Error! Cascades are currently not supported")
        
        if (length(x.react) == 1)
          print("Error! Reaction is not in a correct format! Use -> or = symbols!")
        
        # Parse both directions of reaction
        tmpDF1 <- rbind(tmpDF1, parseReaction(x, paste(x.react[1], x.react[2], sep="->"), y$reaction$r1[i], paste(i, "1", sep="_")))
        tmpDF1 <- rbind(tmpDF1, parseReaction(x, paste(x.react[2], x.react[1], sep="->"), y$reaction$r2[cnt.r2], paste(i, "2", sep="_")))
        cnt.r2 <- cnt.r2 + 1
        
      } else 
        if (whichOne == 2)
          {
            # Parse reaction (only one direction)
            tmpDF1 <- rbind(tmpDF1, parseReaction(x, react.cln, y$reaction$r1[i],as.character(i)))        
        } else {
        print("Error! Cascades are currently not supported")
      }
    }
    
    # Remove any species that is called "null" (sources or sinks)
    tmpDF2 <- tmpDF1[as.character(tmpDF1$species) != "null", ]
    
    y$stochMatrix <- makeStMat(x, tmpDF2)
    y$stochModel <- makeStModel(x, tmpDF2)
    
    # Summarize per species quantity (per reaction and product)
    tmpDF1 <- plyr::ddply(tmpDF2, .variable=c("species", "reaction", "product", "odeSpecies", "odeProduct"), function(x) data.frame(tot=sum(as.numeric(x$stoch)*as.numeric(x$side))))     
    
    # Remove those with total equal to zero (that canceled out)
    tmpDF2 <- tmpDF1[as.numeric(tmpDF1$tot) != 0, ]
    
    # Summarize per species and reactions (in the form reaction times product)
    tmpDF1 <- plyr::ddply(tmpDF2, .variable=c("species", "reaction", "odeSpecies"), function(x) data.frame(t1=ifelse(x$tot == 1, paste(as.character(x$reaction), x$product, sep="*"), ifelse(x$tot == -1, paste("-", paste(as.character(x$reaction),x$product, sep="*"), sep=""), paste(as.character(x$tot), as.character(x$reaction), x$product, sep="*"))), t2=ifelse(x$tot == 1, paste(as.character(x$reaction), x$odeProduct, sep="*"), ifelse(x$tot == -1, paste("-", paste(as.character(x$reaction),x$odeProduct, sep="*"), sep=""), paste(as.character(x$tot), as.character(x$reaction), x$odeProduct, sep="*")))))
    
    # Now summarize everything per species
    tmpDF2 <- plyr::ddply(tmpDF1, .variable=c("species", "odeSpecies"), function(x) data.frame(res1=paste(x$t1, sep="", collapse=" + "), res2=paste(x$t2, sep="", collapse=" + ")))
    
    # Final tuning - replace + - signs with -, etc.
    tmpDF1 <- plyr::ddply(tmpDF2, .variable=c("species", "odeSpecies"), function(x) data.frame(model=stringr::str_replace_all(as.character(x$res1), "\\+ \\-", "- "), equation=stringr::str_replace_all(as.character(x$res2), "\\+ \\-", "- ")))
    
    # Add model equations to the model
    y$model <- paste("d_", as.character(tmpDF1$species), " <- ", as.character(tmpDF1$model), sep="")    
    
    # if rules include any species, include them into the model too  - TBD
    
    # Check if all species are used - TDB
    
    # Create ODEs
    y$odes <- list(equations=paste(paste("r[", 1:nrow(tmpDF1), "]", sep=""), " <- ", as.character(tmpDF1$equation), " - d", as.character(tmpDF1$odeSpecies), sep=""), rates=makeODERates(x), parameters=makeODEParameters(x), rules=makeODERules(x,tmpDF1))
    
    # Mark the model as OK
    y$isChecked <- 1
    
    # Return model to the original object
    assign(deparse(substitute(x)), y, envir = .GlobalEnv)  
    
  } else {
    print("No reactions found!")
  }      
}

#makeModel <- cmpfun(makeModel.function)
#rm(makeModel.function)
