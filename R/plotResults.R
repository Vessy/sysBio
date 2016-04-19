# plotResults function plots the simulation results
#
# This file is part of the R sysBio package. 
#
# sysBio package is free software and is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

plotResults.function <- function(x=NULL, smooth = FALSE, title=NULL){
  
  if (is.null(x))
    stop("No data provided")
  
  if (ncol(x) > 2){
    plotHlp1 <- stack(x[,2:ncol(x)])
    plotHlp2 <- cbind(time = rep(x[,1], times=(ncol(x)-1)), plotHlp1)
    colnames(plotHlp2) <- c("Time", "SpeciesAmounts", "Species")
  } else {
    plotHlp2 <- cbind(x, Species=rep(colnames(x)[2], times=nrow(x)))
    colnames(plotHlp2) <- c("Time", "SpeciesAmounts", "Species")
  }
  
  if (smooth)
    qplot(Time, SpeciesAmounts, data=plotHlp2, geom="smooth", se=FALSE, colour=Species, position="identity", xlab="Time (seconds)", ylab="Species amounts", main=title) + theme_bw(18)      
  else
    qplot(Time, SpeciesAmounts, data=plotHlp2, geom="line", colour=Species, position="identity", xlab="Time (seconds)", ylab="Species amounts", main=title) + theme_bw(18)
}

plotResults <- cmpfun(plotResults.function)
rm(plotResults.function)

