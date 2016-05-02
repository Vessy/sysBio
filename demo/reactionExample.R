###########################################
library(sysBio)
###########################################
#
# A set of examples that illustrate basic
# functionalities of sysBio package
#
# 04/20/2016
###########################################
#
###########################################
# Synthesis 
# Zero order reaction
# rm(list = ls())

exmp1 <- newModel("Synthesis")

addMAreaction(exmp1, "null  -> P", "k")

addSpecies(exmp1, "P", 0)

addMAreactRate(exmp1, "k", "fixed", 1)

makeModel(exmp1)
#printInfo(exmp1)

simResults <-simulateModel(exmp1)
plotResults(simResults, title="Synthesis")

# Does not work for zero order rate - needs to be check!
#simResults.stoch <- solveStoch(exmp1)
#plotResults(simResults.stoch, title="Synthesis (stochastic simulation)")

################################################
# Degradation
# rm(list = ls())

exmp2 <- newModel("Degradation")

addMAreaction(exmp2, "P -> null", "r")
addMAreactRate(exmp2, "r", type="assigned", val="k*P")

addSpecies(exmp2, "P", 10)

addParameters(exmp2, "k", 0.1)

makeModel(exmp2)
#printInfo(exmp2)

simResults <-simulateModel(exmp2)
plotResults(simResults, title="Degradation")

simResults.stoch <- solveStoch(exmp2)
plotResults(simResults.stoch, title="Degradation (stochastic simulation)")

################################################
# Gene regulation
# rm(list = ls())

exmp3 <- newModel("Gene Regulation")

addMAreaction(exmp3, "DNA -> DNA + mRNA", r1="v1", name="Transcription")
addMAreaction(exmp3, "mRNA -> mRNA + protein", r1="v2", name="Translation")
addMAreaction(exmp3, "DNA + protein -> DNA_protein", r1="v3", name="Binding")
addMAreaction(exmp3, "DNA_protein -> DNA + protein", r1="v4", name="Unbinding")
addMAreaction(exmp3, "mRNA -> null", r1="v5", name="Degradation: mRNA")
addMAreaction(exmp3, "protein -> null", r1="v6", name="Degradation:protein")

addMAreactRate(exmp3, "v1", "assigned", "k1*DNA")
addMAreactRate(exmp3, "v2", "assigned", "k2*mRNA")
addMAreactRate(exmp3, "v3", "assigned", "k3*DNA*protein")
addMAreactRate(exmp3, "v4", "assigned", "k3r*DNA_protein")
addMAreactRate(exmp3, "v5", "assigned", "k4*mRNA")
addMAreactRate(exmp3, "v6", "assigned", "k5*protein")

addSpecies(exmp3, "DNA", 50)
addSpecies(exmp3, "mRNA", 0)
addSpecies(exmp3, "protein", 0)
addSpecies(exmp3, "DNA_protein", 0)

addParameters(exmp3, "k1", 0.2)
addParameters(exmp3, "k2", 20)
addParameters(exmp3, "k3", 0.2)
addParameters(exmp3, "k3r", 1)
addParameters(exmp3, "k4", 1.5)
addParameters(exmp3, "k5", 1)

#validateModel(exmp3)
makeModel(exmp3)
#printInfo(exmp3)

simResults <-simulateModel(exmp3)
plotResults(simResults, title="Gene regulation")

simResults.stoch <- solveStoch(exmp3,10)
plotResults(simResults.stoch, title="Gene regulation (stochastic simulation)")

################################################
# Yeast heterotrimeric G protein cycle
rm(list = ls())

exmp4 <- newModel("Yeast heterotrimeric G protein cycle")

addMAreaction(exmp4, "L + R = RL", r1="kRL", r2="kRLm", name="Receptor-ligand interaction")
addMAreaction(exmp4, "Gd + Gbg -> G", r1="kG1", name="Heterotrimeric G-protein formation")
addMAreaction(exmp4, "RL + G -> Ga + Gbg + RL", r1="kGa", name="G protein activation")
addMAreaction(exmp4, "R = null", r1="kRdo", r2="kRs", name="Receptor synthesis and degradation")
addMAreaction(exmp4, "RL -> null", r1="kRD1", name="Receprot-ligand degradation")
addMAreaction(exmp4, "Ga -> Gd", r1="kGd", name="G proten inactivation")

addSpecies(exmp4, "L", 6.022E+017)
addSpecies(exmp4, "R", 10000.0)
addSpecies(exmp4, "RL", 0.0)
addSpecies(exmp4, "Gd", 3000)
addSpecies(exmp4, "Gbg", 3000)
addSpecies(exmp4, "G", 7000)
addSpecies(exmp4, "Ga", 0.0)

addMAreactRate(exmp4, "kRL", "fixed", 3.32E-18)
addMAreactRate(exmp4, "kRLm", "fixed", 0.01)
addMAreactRate(exmp4, "kG1", "fixed", 1.0)
addMAreactRate(exmp4, "kGa", "fixed", 1.0E-5)
addMAreactRate(exmp4, "kRdo", "fixed", 4.0E-4)
addMAreactRate(exmp4, "kRs", "fixed", 4.0)
addMAreactRate(exmp4, "kRD1", "fixed", 0.0040)
addMAreactRate(exmp4, "kGd", "fixed", 0.11)

#validateModel(exmp4)

makeModel(exmp4)
#printInfo(exmp4)

simResults <-simulateModel(exmp4, times=seq(0, 50, by = 0.5))

# Plot results, but skip L, because value is too hight
plotResults(simResults[,c(1, 3:8)], title="Yeast heterotrimeric G protein cycle")

simResults.stoch <- solveStoch(exmp4, 50)
plotResults(simResults.stoch[,c(1:5, 7:8)], title="Yeast heterotrimeric G protein cycle (stochastic simulation)")

################################################
# HIV infection model
# rm(list = ls())

exmp5 <- newModel("HIV infection model")

addMAreaction(exmp5, "null  -> T", "l")
addMAreaction(exmp5, "T -> null", "r")
addMAreaction(exmp5, "V -> null", "c")
addMAreaction(exmp5, "T + V-> I", "b")
addMAreaction(exmp5, "I -> 900*V", "d")

addSpecies(exmp5, "T", 100)
addSpecies(exmp5, "V", 50000)
addSpecies(exmp5, "I", 150)

addMAreactRate(exmp5, "l", "fixed", 80)
addMAreactRate(exmp5, "b", "fixed", 0.00002)
addMAreactRate(exmp5, "r", "fixed", 0.15)
addMAreactRate(exmp5, "d", "fixed", 0.55)
addMAreactRate(exmp5, "c", "fixed", 5.5)

makeModel(exmp5)
#printInfo(exmp5)

simResults <- simulateModel(exmp5)
simResults <- cbind(simResults, log10V=log10(simResults$V))
plotResults(simResults[,c("time", "T", "I", "log10V")], title="HIV infection model")
plot(simResults)

simResults.stoch <- solveStoch(exmp5, 10)
simResults.stoch <-  cbind(simResults.stoch, log10V=log10(simResults.stoch$V))
plotResults(simResults.stoch[,c("V1", "T", "I", "log10V")], title="HIV infection model (stochastic simulation)")
plot(simResults.stoch)

################################################
# Rules - test #1
# rm(list = ls())

exmp6 <- newModel("Rules - test #1")

addMAreaction(exmp6, "A -> B", r1="m")

addSpecies(exmp6, "A", 10)
addSpecies(exmp6, "B", 0)
addSpecies(exmp6, "C", 5)

addMAreactRate(exmp6, "m", "assigned", "k1*A")
addParameters(exmp6, "k1", 1)
addParameters(exmp6, "k2", 1)

addRule(exmp6, "rule1", "ODEs", "C=k2*A")

#validateModel(exmp6)

makeModel(exmp6)
#printInfo(exmp6)

simResults <-simulateModel(exmp6) 
plotResults(simResults, title="Rules - test #1")

simResults.stoch <- solveStoch(exmp6)
plotResults(simResults.stoch, title="Rules - test #1")

################################################
# Rules - test #2
# rm(list = ls())

exmp7 <- newModel("Rules - test #2")

addMAreaction(exmp7, "null  -> P", "k")

addSpecies(exmp7, "P", 0)

addMAreactRate(exmp7, "k", "fixed", 1)

addRule(exmp7, "rule1", "ODEs", "C=k2*P")
addParameters(exmp7, "k2", 1)
addSpecies(exmp7, "C", 1)

makeModel(exmp7)
#printInfo(exmp7)

simResults <-simulateModel(exmp7)
plotResults(simResults, title="Rules - test #2")

simResults.stoch <- solveStoch(exmp7)
plotResults(simResults.stoch, title="Rules - test #2")

################################################
# Rules - test #3
# rm(list = ls())

exmp8 <- newModel("Rules - test #3")

addMAreaction(exmp8, "null  -> P", "k")
addMAreaction(exmp8, "Q = null", "rf", "rb")
addMAreaction(exmp8, "2*M + 0.9*N -> 0.5*P + Q + 10*R", "l")
addMAreaction(exmp8, "M = N", "lf", "lr")

addSpecies(exmp8, "P", 0)
addSpecies(exmp8, "Q", 10)
addSpecies(exmp8, "M", 70)
addSpecies(exmp8, "N", 110)
addSpecies(exmp8, "R", 0.2)

addMAreactRate(exmp8, "k", "fixed", 1)
addMAreactRate(exmp8, "rf", "fixed", 0.5)
addMAreactRate(exmp8, "rb", "fixed", 1)
addMAreactRate(exmp8, "lf", "fixed", 1)
addMAreactRate(exmp8, "lr", "fixed", 0.1)
addMAreactRate(exmp8, "l", "assigned", "2*M*N")

makeModel(exmp8)

addRule(exmp8, "rule1", "ODEs", "R=kr*P")
addRule(exmp8, "rule2", "ODEs", "Q=kq*R")
addRule(exmp8, "rule2", "ODEs", "M=5*R")
addParameters(exmp8, "kr", 0.01)
addParameters(exmp8, "kq", 0.25)

makeModel(exmp8)
#printInfo(exmp8)

simResults <-simulateModel(exmp8, times=seq(0, 100, by = 0.1))
plotResults(simResults, title="Rules - test #2")

simResults.stoch <- solveStoch(exmp8, 100)
plotResults(simResults.stoch, title="Rules - test #2")

################################################
# Rules - test #3
# rm(list = ls())

exmp9 <- newModel("Rules - test #3")

addMAreaction(exmp9, "null  -> P", "k")
addMAreaction(exmp9, "Q = null", "rf", "rb")
addMAreaction(exmp9, "0.5*P + Q + 10*R -> 2*M + 0.9*N", "l")

addSpecies(exmp9, "P", 0)
addSpecies(exmp9, "Q", 10)
addSpecies(exmp9, "M", 1)
addSpecies(exmp9, "N", 1)
addSpecies(exmp9, "R", 0.2)

addMAreactRate(exmp9, "k", "fixed", 1)
addMAreactRate(exmp9, "rf", "fixed", 0.5)
addMAreactRate(exmp9, "rb", "fixed", 1)
addMAreactRate(exmp9, "l", "assigned", "2*M*N")

makeModel(exmp9)

addRule(exmp9, "rule1", "ODEs", "R=kr*P")
addRule(exmp9, "rule2", "ODEs", "Q=kq*R")
addParameters(exmp9, "kr", 0.01)
addParameters(exmp9, "kq", 0.25)

makeModel(exmp9)
#printInfo(exmp9)

simResults <-simulateModel(exmp9,times=seq(0, 100, by = 0.1))
plotResults(simResults, title="Rules - test #2")

simResults.stoch <- solveStoch(exmp9, 100)
plotResults(simResults.stoch, title="Rules - test #2")


################################################
# Example from the manual
# rm(list = ls())
exmp <- newModel("This is an example of a new model")

addMAreaction(exmp, react="A = null", "rf", "rb")
addMAreaction(exmp, react="A + B -> 2*AB", "k", name="Forward AB")
addMAreaction(exmp, react="AB -> null", "rAB")

addMAreactRate(exmp, "rf", "fixed", "1")
addMAreactRate(exmp, "rb", "fixed", "0.75")
addMAreactRate(exmp, "k", "fixed", "0.5")
addMAreactRate(exmp, "rAB", "assigned", "p1*A")

addParameters(exmp, "p1", 0.75)

addSpecies(exmp, "A", 10)
addSpecies(exmp, "B", 10)
addSpecies(exmp, "AB", 0)

addRule(exmp, "rule B", "ODEs", "B=-0.1*AB")

makeModel(exmp)

simResults <-simulateModel(exmp, times=seq(0, 100, by = 0.1))
plotResults(simResults, title="Simulation results")

simResults.stoch <- solveStoch(exmp, 100)
plotResults(simResults.stoch, title="Simulation results (stochastic)")


