# speciesDetails function provides info abour species included in the model,
# e.g., species names and their initial values.
#
# This file is part of the R sysBio package. 
#
# sysBio package is free software and is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

speciesDetails.function <- function(x){  
  
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

speciesDetails <- cmpfun(speciesDetails.function)
rm(speciesDetails.function)

