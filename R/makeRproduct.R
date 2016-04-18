# makeRproduct function parses the canonical form of a mass action reaction
# and transforms it into a (product) format based on the reaction rates and species.
# This function is called from the "parseComponents" function.
#
# This file is part of the R sysBio package. 
#
# sysBio package is free software and is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

makeRproduct.function <- function(x){
  
  mp.1 <- x[x$side == -1,]
  
  # First one
  hlp <- ifelse(mp.1$V1[1] == 1, mp.1$V2[1], paste(mp.1$V2[1], mp.1$V1[1], sep="^"))
  
  if (nrow(mp.1) > 1)
    for (i in 2:nrow(mp.1))
      hlp <- paste(hlp, ifelse(mp.1$V1[i] == 1, mp.1$V2[i], paste(mp.1$V2[i], mp.1$V1[i], sep="^")), sep="*")
  
  hlp
}

makeRproduct <- cmpfun(makeRproduct.function)
rm(makeRproduct.function)
