# makeODERules function creates assignment (in the format required for ODE solver)
# for each each of the specidied paramterer based on its assigned/specified value.
# This function is called from the "makeModel.R" function.
#
# This file is part of the R sysBio package. 
#
# sysBio package is free software and is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

makeODERules.function <- function(x, howMany){
  
  hlp <- c()
  hlpDF <- data.frame(lside = c(), rside = c())
  
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
      lside <- numbers.in[is.na(as.numeric(numbers.in))]
      
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
      
     
      hlp0 <- data.frame(lside = as.character(hlp1[1]), rside = as.character(hlp1[2]))
      
     for (j in sList.length:1){
        hlp0$lside <- str_replace_all(hlp0$lside, sList.sorted[j], paste("y[", which(x$species$sName == sList.sorted[j]), "]", sep=""))
        hlp0$rside <- str_replace_all(hlp0$rside, sList.sorted[j], paste("y[", which(x$species$sName == sList.sorted[j]), "]", sep=""))
     }
      
     if (i == 1){
       hlpDF <- hlp0
     } else {
       hlpDF <- rbind(hlpDF, hlp0)
     }
      
    }
  }  
  
  if (nrow(hlpDF) > 0)
    # Transform data frame into a set of equations that need to be solved
    hlp <- paste(paste("r[", (howMany+1):(howMany + nrow(hlpDF)), "]", sep=""), " <- ", as.character(hlpDF$rside), " - d", as.character(hlpDF$lside), sep="")
      
  hlp
}

makeODERules <- cmpfun(makeODERules.function)
rm(makeODERules.function)
