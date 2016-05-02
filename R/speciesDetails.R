#' Get info abour species included in the reaction
#' 
#' This function returns info about species included in the reaction (species name and the corresponding stoichometric coefficient). 
#' It is called within the "parseComponents.R" function
#' 
#' @param x  a model name
#'
#' @return A vector that contains species name and the corresponding stoichometric coefficient
#'   

#speciesDetails.function <- function(x){  
speciesDetails <- function(x){
  
  # Assumes format k*species
  hlp <- unique(unlist(strsplit(x , split="[*)( ]")))
  
  if (length(hlp) == 1){  
    species.value <- 1
    species.name <- hlp[1]
  } else 
     if (length(hlp) == 2){
    species.value <- hlp[1] 
    species.name <- hlp[2]
     } else {
    stop("Problem with reaction definition. Supports only a single multiplication format: k*species")
  }
  
  c(species.value, species.name)
}

#speciesDetails <- cmpfun(speciesDetails.function)
#rm(speciesDetails.function)

