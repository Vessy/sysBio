#' Creating representation for the rules
#' 
#' This function  creates assignment (in the format required for ODE solver) for each of the specified rules
#' based on their defined values.  This function is called from the "makeModel.R" function.
#' 
#' @param x  model name (required)
#' @param tmpDF  a data frame that contains information about species and their mathematical representations
#'     (temp data frame used within the "makeModel.R" function) (required)
#'     
#' @return This function returns rules defined for the model in the format appropriate for the ODEs solver
#'  

#makeODERules.function <- function(x, howMany){
makeODERules <- function(x, tmpDF){
  
  hlp <- c()
  hlpDF <- data.frame(lside = c(), rside = c())
  howMany <- nrow(tmpDF)
  
  # Sort species names and get the number of species
  sList.sorted <- x$species$sName[order(nchar(x$species$sName), x$species$sName)]
  sList.length <- length(sList.sorted)
  
  # Get ODEs rules numbers
  rules.odes <- which(toupper(x$rules$rType) == "ODES")
  
  # Check how many rules are there
  numRules.odes <- length(rules.odes)
  
  # If there are any rules
  if (numRules.odes > 0){
    for (i in 1:numRules.odes){
      
      # Remove spaces from the rule
      hlp0 <- gsub(" ","", x$rules$rRule[rules.odes[i]], fixed=TRUE)
      
      # Check if the rule is defined as an equation
      hlp1 <- strsplit(hlp0, "\\=")[[1]]
      if (length(hlp1) != 2)
        stop("Problem with rule definition. Rule needs to be defined in the form of equation, e.g., the rule dA/dt = k*A should be defined as A=k*A")
      
      # Also check if the left side contains only one species
      # Split the string at the operators
      numbers.in <- unique(unlist(strsplit( hlp1[1] , split="[-+*/=)( ]")))
      # Remove numbers from the results (cases like 5*A would be split as 5 and A)
      # This step may return a warning:
      # "Warning message: NAs introduced by coercion" 
      lside <- suppressWarnings(numbers.in[is.na(as.numeric(numbers.in))])
      
      # Get the species involved in the ODEs
      
      numbers.in <- unique(unlist(strsplit(hlp0 , split="[-+*/=)( ]")))
      hlpR <- suppressWarnings(numbers.in[is.na(as.numeric(numbers.in))])
      whatsIn <- hlpR[(hlpR != "")]
      
      # Check if all species, parameters, or rates are defined
      
      everythingDefined <- c(x$species$sName, x$reaction$r1, x$reaction$r2, x$rates$rrName, x$parameters$pName)
      
      if (length(intersect(whatsIn, everythingDefined)) != length(whatsIn))
      {
        print("Not everything has been defined! You need to define the following objects: ")
        print(setdiff(whatsIn, everythingDefined))
        stop("Please define the missing object(s), then try again!")
      }
        
     # TBD  - check if there is any exception about what can be used here and what cannot be used.
     # E.G., if the same species can be defined in the reaction and in the rule
      
     
      hlp0 <- data.frame(species = as.character(hlp1[1]), lside = as.character(hlp1[1]), rside = as.character(hlp1[2]))
      
     for (j in sList.length:1){
        hlp0$lside <- stringr::str_replace_all(hlp0$lside, sList.sorted[j], paste("y[", which(x$species$sName == sList.sorted[j]), "]", sep=""))
        hlp0$rside <- stringr::str_replace_all(hlp0$rside, sList.sorted[j], paste("y[", which(x$species$sName == sList.sorted[j]), "]", sep=""))
     }
      
     if (i == 1){
       hlpDF <- hlp0
     } else {
       hlpDF <- rbind(hlpDF, hlp0)
     }
    }
  }  
  
  toAddRules <- nrow(hlpDF)
 
  if (toAddRules > 0){
    # Transform data frame into a set of equations that need to be solved
    
    # Create a vector that will have reaction/rule number for each species
    spDerivative <- c()
    
    for (i in 1:toAddRules){
      
      # Check if the species for which rule is defined has defined ODEs from reactions
      isAlreadyIn <- which(as.character(tmpDF$species)== as.character(hlpDF$species[i]))
        
      if (length(isAlreadyIn) == 0){
        # Species in the rule have not been defined before (in the reaction)
        
        # Check if the species for which rule is defined have been defined in some other rule
        isAlreadyInRule <- which(as.character(hlpDF$species[1:(i-1)])== as.character(hlpDF$species[i]))
        
        if ((length(isAlreadyInRule) == 0) | (i == 1)){
          # No
          spDerivative <- c(spDerivative, paste("r[", howMany+1, "]", sep=""))
          howMany <- howMany + 1
        } else {
          # Species in the rule has been defined in the rule before
          spDerivative <- c(spDerivative, paste("r[", spDerivative[isAlreadyInRule], "]", sep=""))
        }
      } else {
        # Species in the rule has been defined before 
        spDerivative <- c(spDerivative, paste("r[", isAlreadyIn, "]", sep=""))
      }
    }
    
    # Add column with the ODE reaction number
    hlpDF <- cbind(hlpDF, rNum=spDerivative)
    
    hlp <- paste(as.character(hlpDF$rNum), " <- ", as.character(hlpDF$rside), " - d", as.character(hlpDF$lside), sep="")
    
  }
  
  hlp
}

#makeODERules <- cmpfun(makeODERules.function)
#rm(makeODERules.function)
