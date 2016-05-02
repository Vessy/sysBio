#' Creating a product for reaction species
#' 
#' This function creates the product form for all of the species. It is called from the "parseComponents.R" function.
#' 
#' @param dfTmp  a data frame that contains
#' @param whichOne  a flag that determines whether to use the reactant/species (e.g., A) (flag set to 1) or 
#'     a derivative of the reactant (e.g., dA) in the equation (flag != 1)
#'     
#' @return This function returns a product form for a reactant/species from the reactions
#'  

#makeProduct.function <- function(dfTmp, whichOne){
makeProduct <- function(dfTmp, whichOne){  
  
# Find rows that contain species found on the left side of the reaction
  mp.1 <- dfTmp[dfTmp$side == -1,]
  
  # whichOne determines whether to use the reactant/species (A) or derivative of derivative (dA) in the equation
  if (whichOne == 1){
    # First one
    hlp <- ifelse(mp.1$V1[1] == 1, mp.1$V2[1], paste(mp.1$V2[1], mp.1$V1[1], sep="^"))
    
    # If there are more than one ractants on the left side, create a product
    if (nrow(mp.1) > 1)
      for (i in 2:nrow(mp.1))
        hlp <- paste(hlp, ifelse(mp.1$V1[i] == 1, mp.1$V2[i], paste(mp.1$V2[i], mp.1$V1[i], sep="^")), sep="*")
  } else {
    # First one
    hlp <- ifelse(mp.1$V1[1] == 1, as.character(mp.1$odeSpecies[1]), paste(as.character(mp.1$odeSpecies[1]), mp.1$V1[1], sep="^"))
    
    if (nrow(mp.1) > 1)
      for (i in 2:nrow(mp.1))
        hlp <- paste(hlp, ifelse(mp.1$V1[i] == 1, as.character(mp.1$odeSpecies[i]), paste(as.character(mp.1$odeSpecies[i]), mp.1$V1[i], sep="^")), sep="*")
  }
  
  hlp
}

#makeProduct <- cmpfun(makeProduct.function)
#rm(makeProduct.function)
