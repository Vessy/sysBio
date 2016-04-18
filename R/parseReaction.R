# parseReaction function parses reaction and transforms the canonical form of
# a mass action reaction into a format that can be used to describe them as ODEs.
#
# This file is part of the R sysBio package. 
#
# sysBio package is free software and is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

parseReaction.function <- function(model, reaction, rate, rNumber){
  
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

parseReaction <- cmpfun(parseReaction.function)
rm(parseReaction.function)
