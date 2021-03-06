#' Print information about the model
#' 
#' This function validates model, i.e., checks if all reactions, parameters, rates, rules, etc., are 
#' incoorporated into a model, as well as whether all variables have assigned values (either constant or in a form of a function).
#' 
#' @param x a model name
#' 
#' @return Numeric value - 1 if model has been validated, 0 if validation failed
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
#' validateModel(exmp)
#' 
#' @export
#' 

#validateModel.function <- function(x){
validateModel <- function(x){
  
  if (!exists(deparse(substitute(x))))
    stop("Specified model does not exist!")
  
  print("Validating model... ")
  
  isValidated <- 1
  
  #######
  # There has to be a better way to do this (so it should be TDB)
  # but right now I am going to do it this way, just to be able to validate model
  # and test some other features
  #
  # Current way - if species/rates/parameteres are not found as a part of reactions, check rules
  # For rules - check if all components (species, rates, parameters, etc) are found somewhere
  # Right now, we don't differentiate between different types of components
  #######
  # If there are rules, get everything from the rules (species, rates, parameters, etc)
  
  numR  <- length(x$rules$rRule)
  
  if (numR  > 0){
    
    # Remove spaces from the rule
    hlp0 <- sapply(1:numR , function(y) {gsub(" ","", x$rules$rRule[y] , fixed=TRUE) }) 
    hlp1 <- unique(unlist(sapply(1:numR , function(y) {strsplit(hlp0, split="[-+*/=)( ]|[^x][0-9]+|^[0-9]+")})))
    rule.all <- hlp1[(hlp1 != "")]
    
    everythingDefined <- c(x$species$sName, x$reaction$r1, x$reaction$r2, x$species$sName, x$rates$rrName, x$parameters$pName)
    
    if (length(setdiff(rule.all, everythingDefined)) != 0){
      print("Problem with rules - not all species/rates/parameters used in the rules are defined... ")
      print("You need to define: ")
      print(setdiff(rule.all, everythingDefined))
      isValidated <- 0
    }
  }
  
  ##########
  
  
  # Check species definitions
  if ((length(x$reaction) > 0) & (length(x$species) > 0)){
    
    numR <- length(x$reaction$reaction)
  
    hlp0 <- sapply(1:numR, function(y) {gsub(" ","", x$reaction$reaction[y] , fixed=TRUE) }) 
    hlp1 <- unlist(sapply(1:numR, function(y) {strsplit(hlp0[y], "\\->")}))
    hlp2 <- unlist(sapply(1:length(hlp1), function(y) {strsplit(hlp1[y], "\\=")}))
    hlp3 <- unique(unlist(sapply(1:length(hlp2), function(y) {strsplit(hlp2[y], "[*+)( ]")})))
    
    # Remove numbers from the results (cases like 5*A would be split as 5 and A)
    # This step may return a warning:
    # "Warning message: NAs introduced by coercion" 
    hlp2 <- suppressWarnings(hlp3[is.na(as.numeric(hlp3))])
    
    numSp <- setdiff(hlp2, "null") # remove null from the reactions
    
    if (length(setdiff(numSp, x$species$sName)) != 0){
      print("Problem with species definition - not all species used in the reactions are defined... ")
      print("You need to define: ")
      print(setdiff(numSp, x$species$sName))
      isValidated <- 0
    }
    
    if (length(setdiff(x$species$sName, c(numSp, "null"))) != 0){
      
      # If not 0, check if the difference is due to species used in the rules
      
      whatIsMissing <- setdiff(x$species$sName, c(numSp, "null"))
      
      if (length(setdiff(whatIsMissing, rule.all)) != 0){
            print("Problem with species definition - all defined species are not used... ")
            print("Unused species are: ")
            print(setdiff(whatIsMissing, rates.all))
            isValidated <- 0
      }
    }
  } else {
    print("Model or species definitions are missing... ")
    isValidated <- 0
  }
     
  # Check rates definitions
  if (length(x$rates) > 0){
      
    toCheckRates.assigned <- which(x$rates$rType == "assigned")
      
    numR <- length(toCheckRates.assigned)
      
    if (numR > 0){
      hlp0 <- x$rates$rVal[toCheckRates.assigned]
        
      hlp1 <- sapply(1:numR, function(y) {gsub(" ","", hlp0[y] , fixed=TRUE) }) 
      hlp2.1 <- unlist(sapply(1:numR, function(y) {strsplit(hlp1[y], "\\+")}))
      hlp2.2 <- unlist(sapply(1:length(hlp2.1), function(y) {strsplit(hlp2.1[y], "\\-")}))
      hlp2.3 <- unlist(sapply(1:length(hlp2.2), function(y) {strsplit(hlp2.2[y], "\\*")}))
      hlp2.4 <- unlist(sapply(1:length(hlp2.3), function(y) {strsplit(hlp2.3[y], "\\/")}))
      hlp2.5 <- unlist(sapply(1:length(hlp2.4), function(y) {strsplit(hlp2.4[y], "\\%")}))
      hlp2.6 <- unlist(sapply(1:length(hlp2.5), function(y) {strsplit(hlp2.5[y], "\\^")}))
      hlp2.7 <- unique(unlist(sapply(1:length(hlp2.6), function(y) {strsplit(hlp2.6[y], "\\log")})))
          
      remove.nums <- hlp2.7[!sapply(hlp2.7, is.numeric)]
        
      numSp.assigned <- setdiff(remove.nums, x$species$sName)
      # This step may return a warning:
      # "Warning message: NAs introduced by coercion" 
      hlpR <- suppressWarnings(numSp.assigned[is.na(as.numeric(numSp.assigned))])
      whatsIn <- hlpR[(hlpR != "")]
        
      if (length(setdiff(whatsIn, x$parameters$pName)) != 0){
        print("Problem with rates - not all paramaters assigned to rates are defined... ")
        isValidated <- 0
      }
    }
  } else {
    print("Definitions of reaction rates are missing... ")
    isValidated <- 0
  }

  
  # Check events
    
  if (length(x$events) > 0){
    
    # Check conditions
  
    numE <- length(x$events$condition)
      
    hlp1 <- sapply(1:numE, function(y) {gsub(" ","", x$events$condition[y] , fixed=TRUE) }) 
    hlp2.1 <- unlist(sapply(1:numE, function(y) {strsplit(hlp1[y], "\\>")}))
    hlp2.2 <- unlist(sapply(1:length(hlp2.1), function(y) {strsplit(hlp2.1[y], "\\<")}))
    hlp2.3 <- unlist(sapply(1:length(hlp2.2), function(y) {strsplit(hlp2.2[y], "\\=")}))
    hlp2.4 <- unlist(sapply(1:length(hlp2.3), function(y) {strsplit(hlp2.3[y], "\\&")}))
    hlp2.5 <- unique(unlist(sapply(1:length(hlp2.4), function(y) {strsplit(hlp2.4[y], "\\|")})))
    
    remove.nums <- suppressWarnings(hlp2.5[!sapply(hlp2.5, is.numeric)])
    
    if (length(setdiff(remove.nums, c(x$species$sName, "time"))) != 0){
      print(" Problem with events - not all paramaters within condition are defined... ")
      isValidated <- 0
    }
    
    # Check rules
    
    numE <- length(x$events$rule)
    
    hlp2.1 <- unlist(sapply(1:numR, function(y) {strsplit(hlp1[y], "\\+")}))
    hlp2.2 <- unlist(sapply(1:length(hlp2.1), function(y) {strsplit(hlp2.1[y], "\\-")}))
    hlp2.3 <- unlist(sapply(1:length(hlp2.2), function(y) {strsplit(hlp2.2[y], "\\*")}))
    hlp2.4 <- unlist(sapply(1:length(hlp2.3), function(y) {strsplit(hlp2.3[y], "\\/")}))
    hlp2.5 <- unlist(sapply(1:length(hlp2.4), function(y) {strsplit(hlp2.4[y], "\\%")}))
    hlp2.6 <- unlist(sapply(1:length(hlp2.5), function(y) {strsplit(hlp2.5[y], "\\^")}))
    hlp2.7 <- unique(unlist(sapply(1:length(hlp2.6), function(y) {strsplit(hlp2.6[y], "\\log")})))
    
    remove.nums <- suppressWarnings(hlp2.7[!sapply(hlp2.7, is.numeric)])
      
    if (length(setdiff(remove.nums, c(x$parameters$pName, x$rates$rrName, x$species$sName))) != 0){
      print(" Problem with events - not all paramaters within condition are defined... ")
      isValidated <- 0
    }
  }  
  
  
  #######################################################
  
  if (isValidated == 1)
    print("Model has been validated!")
  
  isValidated
  
}

#validateModel<- cmpfun(validateModel.function)
#rm(validateModel.function)
