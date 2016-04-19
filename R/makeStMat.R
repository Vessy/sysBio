# makeStMat function parses the components of reactions and transforms them into a
# matrix format required for the stochastic simulation
#
# This file is part of the R sysBio package. 
#
# sysBio package is free software and is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

makeStMat.function <- function(x, tmpDF){
  
  if (!exists(deparse(substitute(x))))
    stop("Specified model does not exist!")
  
 # Calculate "amounts" of each species per reaction
 tmpDF.hlp <- ddply(tmpDF, .variable=c("reactionNumber", "species"), function(x) data.frame(tot=sum(as.numeric(x$stoch)*as.numeric(x$side))))     
 tmpDF.hlp2 <- tmpDF.hlp[as.numeric(tmpDF.hlp$tot) != 0, ]
 
 # Transform the numbers into a matrix
 matrix.hlp <- acast(tmpDF.hlp2, species~reactionNumber, value.var="tot", fill=0)
 
 # Check the size of the matrix. 
 # It is possible for daply to return an array instead of matrix, in case there is only a single species (e.g., zero order reaction)
 #if (!is.matrix(matrix.hlp)) {
#   matrix.hlp2 <- as.matrix(daply(tmpDF.hlp2, .(species, reactionNumber), function(x) x$tot))
# } else {
   # Order matrix rows and columns
   # and return the matrix
  # We used intersect in case there are more species defined than used in reactions (in case they are defined for rules, etc)
   matrix.hlp[intersect(rownames(matrix.hlp),x$species$sName),sort(colnames(matrix.hlp)), drop=FALSE]
 #}

}

makeStMat <- cmpfun(makeStMat.function)
rm(makeStMat.function)
