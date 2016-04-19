# solveStoch function transforms reactions and their corresponded items into a
# format required for stochastic simulation (using Gillespie method).
#
# This file is part of the R sysBio package. 
#
# sysBio package is free software and is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

solveStoch.function <- function(x,times=10,
                                method = "D", simName = "", tau = 0.3, f = 10, epsilon = 0.03, 
                                nc = 10, hor = NaN, dtf = 10, nd = 100, ignoreNegativeState = TRUE, 
                                consoleInterval = 0, censusInterval = 0, verbose = FALSE, 
                                maxWallTime = Inf){
  
  if (!exists(deparse(substitute(x))))
    stop("Specified model does not exist!")
  
  isValidated <- 0
  
  if (x$isChecked == 0){
    print ("Model not validated... Validating model...")
    isValidated <- validateModel(x)
  }
  
  if ((x$isChecked == 1) | (isValidated == 1)){
   
    parameters.length <- length(x$parameters$pName)
    rates.length <- length(x$rates$rrName)
    reactions.length <- length(x$reaction$rName)
    rules.length <- length(x$rules$rName)
    stochModel.length <- length(x$stochModel)
    
    total.reactions <- reactions.length + rules.length
    
    # Vector of model parameters can be specified or NULL (defualt value)
    params <- NULL
    params.data <- NULL
    
    # Check if we have any parameters. If we do, create a vector with parameters
    if (parameters.length > 0) 
      params.data <- c(params.data, paste(x$parameters$pName, "=", x$parameters$initVal, sep=""))
    
    # Check if there are rates marked as "fixed"
    if ("FIXED" %in% toupper(x$rates$rType)){
      get.index.fixed <- which(toupper(x$rates$rType) == "FIXED")
      
      params.data <- c(params.data, paste(x$rates$rrName[get.index.fixed], "=", x$rates$rVal[get.index.fixed], sep=""))
    }
     
    if (!is.null(params.data))
      params <- eval(parse(text=paste("c(", paste(params.data , sep="", collapse=", "), ")", sep="")))
    
    stochModel.tmp <- x$stochModel
    stochMatrix.tmp <- x$stochMatrix 
    

    # Check if there are any rules defined
    if (rules.length > 0){
      
      # If there are rules, we need to:
      # 1. add it to propensity function list and 
      # 2. update/expand stochastic matrix
      
      # Get ODEs rules numbers
      rules.odes <- which(toupper(x$rules$rType) == "ODES")
      
      # Check how many rules are there
      numRules.odes <- length(rules.odes)
      
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
          lside <- numbers.in[is.na(as.numeric(numbers.in))]
          
          if (length(lside) != 1)
            stop("Problem with rule definition. Rule needs to be defined in the form of equation, e.g., the rule dA/dt = k*A should be defined as A=k*A. Left side should contain a single species")
          
          # Get the species involved in the ODEs
          
          numbers.in <- unique(unlist(strsplit(hlp0 , split="[-+*/=)( ]")))
          hlpR <- numbers.in[is.na(as.numeric(numbers.in))]
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
          
          # Add the rule ODE reaction in the list of reactions for stochastic simulation (propensity function list)
          stochModel.tmp <-c(stochModel.tmp, as.character(hlp1[2]))
          
          # Update/expand stochastic matrix
          
          # Get species involved in the rule
          speciesIn <- intersect(whatsIn, x$species$sName)
          
          # Get species alreadi in the matrix
          speciesMatrix <- row.names(x$stochMatrix)
          
          # Check if there are any new species
          areInRule.new <-setdiff(speciesIn, speciesMatrix)
          toAdd.rows <- length(areInRule.new)
          
          # Create a new matrix column (for the new reaction)
          stMatrix.newColumn <- c(rep(0, nrow(x$stochMatrix)))
          # Check which species appear in the rule, add -1 for them
          stMatrix.newColumn[which(speciesMatrix %in% speciesIn)] <- -1
          # Check if anything is in left side, replace that value with 1
          stMatrix.newColumn[which(speciesMatrix %in% lside)] <- 1
          
          # Add a new column to the matrix
          stochMatrix.tmp <- cbind(stochMatrix.tmp, stMatrix.newColumn)
          # Rename that column
          colnames(stochMatrix.tmp)[ncol(stochMatrix.tmp)] <- paste("Rule_", i, sep="")
          
          colnames(stochMatrix.tmp) <- 
          
          # Check if there are any new species (in the rules), if yes, create a new row(s) too
          if (toAdd.rows > 0){
            for (j in 1:toAdd.rows){
              stMatrix.newRow <- c(rep(0, ncol(x$stochMatrix)))
              
              if (areInRule.new[j] %in% lside){
                stMatrix.newRow <- c(stMatrix.newRow, 1)
              } else {
                stMatrix.newRow <- c(stMatrix.newRow, -1)
              }
              
              stochMatrix.tmp <- rbind(stochMatrix.tmp, stMatrix.newRow)
              
              # Rename that row
              rownames(stochMatrix.tmp)[nrow(stochMatrix.tmp)] <- areInRule.new[j]
            }  
          }
          
        }
      }  
    }

    
    # Create a vector of propensity functions where state variables correspond to the names of the elements in x0
    # This requires that SPECIES_NAMES are replaced with {SPECIES_NAMES}
    matchStringFront <- sprintf("$(%s)\\*", paste(x$species$sName, collapse="|"))
    replaceStringFront <- "{\\1}*"
    matchStringMid <- sprintf("\\*(%s)\\*", paste(x$species$sName, collapse="|"))
    replaceStringMid <- "*{\\1}*"
    matchStringEnd <- sprintf("\\*(%s)$", paste(x$species$sName, collapse="|"))
    replaceStringEnd <- "*{\\1}"
    
    for (i in 1:stochModel.length) {
      stochModel.tmp[i] <- gsub(matchStringFront, replaceStringFront, stochModel.tmp[i])
      stochModel.tmp[i] <- gsub(matchStringMid, replaceStringMid, stochModel.tmp[i])
      stochModel.tmp[i] <- gsub(matchStringEnd, replaceStringEnd, stochModel.tmp[i])
    }
   
    
    a <- stochModel.tmp
    
    # Create a vector with initial values
    x0 <- eval(parse(text=paste("c(", paste(x$species$sName, "=", x$species$initVal, sep="", collapse=", "), ")", sep="")))
    
    
    # Get stochastic matrix
    nu <- stochMatrix.tmp
    
    # Solve model stochastically
    storchResult <- ssa(x0,a,nu, params, times,  
                        method, simName, tau, f, epsilon, nc, hor, dtf, nd, ignoreNegativeState, 
                        consoleInterval, censusInterval, verbose,maxWallTime)
    
    # Return simulated values
    as.data.frame(storchResult$data)
    
  } else {
    print("You need to make a model first (see makeModel function)")
  }
}

solveStoch<- cmpfun(solveStoch.function)
rm(solveStoch.function)



