# splitOperations function separates expressions based on various operations
#
# This file is part of the R sysBio package. 
#
# sysBio package is free software and is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

splitOperations.function <- function(x=NA){
  
  if (is.na(x))
    stop("No data specified!")
  
  hlp0 <- gsub(" ","", x , fixed=TRUE)
        
  hlp1 <- unlist(strsplit(hlp0, "\\="))
  hlp0 <- unlist(strsplit(hlp1, "\\+"))
  hlp1 <- unlist(strsplit(hlp0, "\\-"))
  hlp0 <- unlist(strsplit(hlp1, "\\*"))
  hlp1 <- unlist(strsplit(hlp0, "\\/"))
  hlp0 <- unlist(strsplit(hlp1, "\\%"))
  hlp1 <- unlist(strsplit(hlp0, "\\^"))
  hlp0 <- unlist(strsplit(hlp1, "\\log"))
  hlp1 <- unlist(strsplit(hlp0, "\\abs"))
  hlp0 <- unlist(strsplit(hlp1, "sqrt"))
  hlp1 <- unlist(strsplit(hlp0, "\\ceiling"))
  hlp0 <- unlist(strsplit(hlp1, "floor"))
  hlp1 <- unlist(strsplit(hlp0, "\\trunc"))
  hlp0 <- unlist(strsplit(hlp1, "cos"))
  hlp1 <- unlist(strsplit(hlp0, "\\sin"))
  hlp0 <- unlist(strsplit(hlp1, "tan"))
  hlp1 <- unlist(strsplit(hlp0, "\\log"))
  hlp0 <- unlist(strsplit(hlp1, "log10"))
  hlp1 <- unique(unlist(strsplit(hlp0, "\\exp")))
  
  hlp1

}

splitOperations <- cmpfun(splitOperations.function)
rm(splitOperations.function)
