#' Results visualization
#' 
#' This function provides a simple visualization of simulation results, where time points are plotted on the x-axis 
#' and the corresponding species amounts for each species are plotted on the y-axis.
#' 
#' @param x a data frame that contains results of the simulation (required)
#' @param smooth a flag that specify type of geom(s) to draw - smooth (flag set to TRUE) or line (flag set to FALSE).
#' @param title figure title
#' 
#' @return Figure
#' 
#' @examples
#' exmp <- newModel("This is an example of a new model")
#' addMAreaction(exmp, react="A = null", "rf", "rb")
#' addMAreaction(exmp, react="A + B -> 2*AB", "k", name="Forward AB")
#' addMAreaction(exmp, react="AB -> null", "rAB")
#' 
#' addMAreactRate(exmp, "rf", "fixed", "1")
#' addMAreactRate(exmp, "rb", "fixed", "0.75")
#' addMAreactRate(exmp, "k", "fixed", "0.5")
#' addMAreactRate(exmp, "rAB", "assigned", "p1*A")
#' 
#' addParameters(exmp, "p1", 0.75)
#'  
#' addSpecies(exmp, "A", 10)
#' addSpecies(exmp, "B", 10)
#' addSpecies(exmp, "AB", 0)
#' 
#' addRule(exmp, "rule B", "ODEs", "B=-0.1*AB")
#' 
#' makeModel(exmp)
#'   
#' simResults <-simulateModel(exmp)
#' plotResults(simResults, title="Simulation results")
#' 
#' @export
#' 

#plotResults.function <- function(x=NULL, smooth = FALSE, title=NULL){
plotResults<- function(x=NULL, smooth = FALSE, title=NULL){
  
  if (is.null(x))
    stop("No data provided!")
  
  if (!is.data.frame(x))
    stop("Data has to be in the data frame format!")
  
  if (ncol(x) > 2){
    plotHlp1 <- stack(x[,2:ncol(x)])
    plotHlp2 <- cbind(time = rep(x[,1], times=(ncol(x)-1)), plotHlp1)
    colnames(plotHlp2) <- c("Time", "SpeciesAmounts", "Species")
  } else {
    plotHlp2 <- cbind(x, Species=rep(colnames(x)[2], times=nrow(x)))
    colnames(plotHlp2) <- c("Time", "SpeciesAmounts", "Species")
  }
  
  if (smooth){
    #ggplot2::qplot(Time, SpeciesAmounts, data=plotHlp2, geom="smooth", se=FALSE, colour=Species, position="identity", xlab="Time (seconds)", ylab="Species amounts", main=title) + ggplot2::theme_bw(18)
    ggplot2::qplot(Time, SpeciesAmounts, data=plotHlp2, geom="smooth", se=FALSE, colour=Species, xlab="Time (seconds)", ylab="Species amounts", main=title) + ggplot2::theme_bw(18)      
  } else {
    #ggplot2::qplot(Time, SpeciesAmounts, data=plotHlp2, geom="line", colour=Species, position="identity", xlab="Time (seconds)", ylab="Species amounts", main=title) + ggplot2::theme_bw(18)
    ggplot2::qplot(Time, SpeciesAmounts, data=plotHlp2, geom="line", colour=Species, xlab="Time (seconds)", ylab="Species amounts", main=title) + ggplot2::theme_bw(18)
  }
}

#plotResults <- cmpfun(plotResults.function)
#rm(plotResults.function)

