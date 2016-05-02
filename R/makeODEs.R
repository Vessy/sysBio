#' Creating a residual function for the ODEs solver
#' 
#' This function  creates a residual function that will be used within the ODEs solver. 
#' This function is called from the "simulateModel.R" function
#' 
#' @param x  model name (required)
#'     
#' @return A residual function that will be used within the ODEs solver.
#'  

#makeODEs.function <- function(x){
makeODEs <- function(x){
  
  if (!exists(deparse(substitute(x))))
    stop("Specified model does not exist!")
  
  hlpFun <- NULL
  
  isValidated <- 0
  
  if (x$isChecked == 0){
    print ("Model not validated... Validating model...")
    isValidated <- validateModel(x)
  }
  
  if ((x$isChecked == 1) | (isValidated == 1)){
  #if (x$isChecked == 1){
    parameters.length <- length(x$odes$parameters)
    rates.length <- length(x$odes$rates)
    
    # Merge equations for rules and rates (if there are species that have equations in both)
    
    r_reactions <- strsplit(x$odes$equations, " <- ")
    r_reactions.df <- do.call(rbind.data.frame, r_reactions)
    colnames(r_reactions.df) <- c("react", "product")
   
    
    if (length(x$odes$rules) > 0){
      
      r_rules <- strsplit(x$odes$rules, " <- ")
      r_rules.df <- do.call(rbind.data.frame, r_rules)
      colnames(r_rules.df) <- c("react", "product")
      
      odes.all <- plyr::ddply(rbind(r_reactions.df, r_rules.df), .variable=c("react"), function(x) data.frame(productAll = paste(paste("(", x$product, ")", sep=""), sep="", collapse=" + ")))
   
    } else {
      odes.all <-r_reactions.df
      colnames(odes.all) <- c("react", "productAll")
    }
    
    odes.length <- length(odes.all)
    
    if ((parameters.length > 0) & (rates.length > 0)){
      hlpFun <- function(t, y, dy, params){
        
        r <- rep(0, length(dy))
        
        eval(parse(text=x$odes$parameters))
        
        eval(parse(text=paste(as.character(x$rates$rrName), " <- " , x$odes$rates, sep="")))
        
        #eval(parse(text=c(x$odes$equations, x$odes$rules)))
        eval(parse(text=paste(as.character(odes.all$react), " <- " , as.character(odes.all$productAll), sep="")))
        
        list(r)
      }
    }
    
    if ((parameters.length == 0) & (rates.length > 0)){
      hlpFun <- function(t, y, dy, params){
        
        r <- rep(0, length(dy))
        
        eval(parse(text=paste(as.character(x$rates$rrName), " <- " , x$odes$rates, sep="")))
        
        #eval(parse(text=c(x$odes$equations, x$odes$rules)))
        eval(parse(text=paste(as.character(odes.all$react), " <- " , as.character(odes.all$productAll), sep="")))
        
        
        list(r)
      }
    }
  }
  else {
    print("You need to make a model first (see makeModel function)")
  }
  
  hlpFun  
}

#makeODEs <- cmpfun(makeODEs.function)
#rm(makeODEs.function)
