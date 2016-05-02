#' Parsing reactions
#' 
#' This function parses eaction and transforms the canonical form of a mass action reaction 
#' into a format that can be used to describe them as ODEs. This function is called from the
#' "parseReaction.R" function.
#' 
#' @param model  model name (required)
#' @param reaction reaction (forward reaction only) (required)
#' @param rate reaction rate (required)
#' @param rNumber number of reaction in the model; if reaction is reversible, this number will be defined in the following format:
#'     reactionNumber_1 and reactionNumber_2 (required)
#'     
#' @return This function returns a data frame that contains parsed reaction.
#' 

#parseReaction.function <- function(model, reaction, rate, rNumber){
parseReaction <- function(model, reaction, rate, rNumber){
  
  hlp1 <- c()
  
  # Get both sides (components) of the reaction
  x.react <- strsplit(reaction, "\\->")[[1]]
  
  if (length(x.react) > 2)
    stop("Error! Cascades are currently not supported")
  
  # Parse each of the components of the reaction
  hlp1 <- parseComponents(model, x.react, rate) 
  hlp1 <- cbind(hlp1, rNumber = rNumber)
  
  
  # Rename columns and return data frame
  colnames(hlp1) <- c("stoch", "species", "side", "reaction", "product", "odeSpecies", "odeProduct", "reactionNumber") 
  hlp1  
}

#parseReaction <- cmpfun(parseReaction.function)
#rm(parseReaction.function)
