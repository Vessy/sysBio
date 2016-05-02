#' Parsing reaction components
#' 
#' This function parses components of a reaction. This function is called from the "parseReaction.R" function.
#' 
#' @param model  model name (required)
#' @param components a vector that contain left and right side of the reaction (required)
#' @param rate reaction rate (required)
#'     
#' @return This function returns a data frame that contains parsed components of a reaction.
#'   


#parseComponents.function <- function(model, components, rate){
parseComponents <- function(model, components, rate){
  
  # Get components (species) from each side of the reaction
  hlp1 <- sapply(1:length(components), function(y) {strsplit(components[y], "\\+")})
  
  # Check if null
  if (length(hlp1) > 0)
  {
    # Check how many components (species) are on each side of the reaction
    hlp2 <- unlist(hlp1)
    hlp3 <- plyr::laply(hlp1, length)
    
    # Get the quantity, sign, and rate for the species in the reaction (based on the side of species in the reaction)
    hlp4 <- plyr::ldply(1:length(hlp2), function(y) {speciesDetails(hlp2[y])})
    hlp4 <- cbind(hlp4, side=c(rep(-1, times = hlp3[1]), rep(1, times = hlp3[2]))) 
    hlp4 <- cbind(hlp4, rRate=c(rep(rate, times = hlp3[1]+ hlp3[2])))
    
    
    # zero order reaction, i.e., a reaction where the reaction rate does not 
    # depend on the concentration of reactants, e.g., null -> something
    zOrder <- 0
    
    # Check if right side contains only null, e.g., null -> something
    if ((hlp3[1] == 1) & (hlp2[1] == "null"))
    {
      zOrder <- 1
     
      # If it is zero order equation, product will be equal to the reaction rate for all reactants
      hlp4 <- cbind(hlp4, prod=c(rep("1", times=hlp3[1]+ hlp3[2])))
        
    } else{
      
      # Otherwise use product (which will be either just a reactant or a product of reactants, depending on the number of reactants on the left side)
      hlp4 <- cbind(hlp4, prod=c(rep(makeProduct(hlp4, 1), times=hlp3[1]+ hlp3[2])))
    }
    
    hlp5 <- c()
    
    # Assign derivatives for each product/reactant
    for(i in 1:nrow(hlp4))     
      hlp5 <- c(hlp5, paste("y[", which(model$species$sName == as.character(hlp4$V2[i])), "]", sep=""))
    
    hlp4 <- cbind(hlp4, odeSpecies=hlp5)
    
    # Add a column for the product of odes (or just 1s for zero rate reactions) 
    if (zOrder == 1){
      hlp4 <- cbind(hlp4, odeProd=c(rep("1", times=hlp3[1]+ hlp3[2])))
    } else {
      hlp4 <- cbind(hlp4, odeProd=c(rep(makeProduct(hlp4, 2), times=hlp3[1]+ hlp3[2])))
    }
    
    hlp4
  }
  else {
    hlp1
  }
  
}

#parseComponents <- cmpfun(parseComponents.function)
#rm(parseComponents.function)
