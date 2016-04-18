# Load libraries
library(sysBio)

# rm(list = ls())

test <- newModel("cell")

addMAreaction(test, "DNA -> DNA + mRNA", r1="v1", name="Transcription")
addMAreaction(test, "mRNA -> mRNA + protein", r1="v2", name="Translation")
addMAreaction(test, "DNA + protein -> DNA_protein", r1="v3", name="Binding")
addMAreaction(test, "DNA_protein -> DNA + protein", r1="v4", name="Unbinding")
addMAreaction(test, "mRNA -> null", r1="v5", name="Degradation: mRNA")
addMAreaction(test, "protein -> null", r1="v6", name="Degradation:protein")

#validateModel(test)

addMAreactRate(test, "v1", "assigned", "k1*DNA")
addMAreactRate(test, "v2", "assigned", "k2*mRNA")
addMAreactRate(test, "v3", "assigned", "k3*DNA*protein")
addMAreactRate(test, "v4", "assigned", "k3r*DNA_protein")
addMAreactRate(test, "v5", "assigned", "k4*mRNA")
addMAreactRate(test, "v6", "assigned", "k5*protein")

#validateModel(test)

addSpecies(test, "DNA", 50)
addSpecies(test, "mRNA", 0)
addSpecies(test, "protein", 0)
addSpecies(test, "DNA_protein", 0)

#validateModel(test)

addParameters(test, "k1", 0.2)
addParameters(test, "k2", 20)
addParameters(test, "k3", 0.2)
addParameters(test, "k3r", 1)
addParameters(test, "k4", 1.5)
addParameters(test, "k5", 1)

validateModel(test)

makeModel(test)

printInfo(test)

simResults <-simulateModel(test)
plotResults(simResults, title="Test")

simResults.stoch <- solveStoch(test)
plotResults(simResults.stoch, title="Test (stochastic simulation)")


################################################

#rm(list = ls())

t2 <- newModel("Zero order rates")

addMAreaction(t2, "null  -> P", "k")

addSpecies(t2, "P", 0)

addMAreactRate(t2, "k", "fixed", 1)

makeModel(t2)
printInfo(t2)

simResults <-simulateModel(t2)
plotResults(simResults, title="Zero order reaction")

# Does not work for zero order rate - needs to be check!
#simResults.stoch <- solveStoch(t2)
#plotResults(simResults.stoch, title="Zero order reaction (stochastic simulation)")

################################################
rm(list = ls())

t3 <- newModel("Yeast Heterotrimeric G Protein Cycle")
#ftp://ftp.sogr.idv.tw/Backup%20Files/1/1/V0/D/Program%20Files/MATLAB/R2007b/toolbox/simbio/simbiodemos/html/gprotein.html

addMAreaction(t3, "L + R = RL", r1="kRL", r2="kRLm", name="Receptor-ligand interaction")
addMAreaction(t3, "Gd + Gbg -> G", r1="kG1", name="Heterotrimeric G-protein formation")
addMAreaction(t3, "RL + G -> Ga + Gbg + RL", r1="kGa", name="G protein activation")
addMAreaction(t3, "R = null", r1="kRdo", r2="kRs", name="Receptor synthesis and degradation")
addMAreaction(t3, "RL -> null", r1="kRD1", name="Receprot-ligand degradation")
addMAreaction(t3, "Ga -> Gd", r1="kGd", name="G proten inactivation")

addSpecies(t3, "L", 6.022E+017)
addSpecies(t3, "R", 10000.0)
addSpecies(t3, "RL", 0.0)
addSpecies(t3, "Gd", 3000)
addSpecies(t3, "Gbg", 3000)
addSpecies(t3, "G", 7000)
addSpecies(t3, "Ga", 0.0)

addMAreactRate(t3, "kRL", "fixed", 3.32E-18)
addMAreactRate(t3, "kRLm", "fixed", 0.01)
addMAreactRate(t3, "kG1", "fixed", 1.0)
addMAreactRate(t3, "kGa", "fixed", 1.0E-5)
addMAreactRate(t3, "kRdo", "fixed", 4.0E-4)
addMAreactRate(t3, "kRs", "fixed", 4.0)
addMAreactRate(t3, "kRD1", "fixed", 0.0040)
addMAreactRate(t3, "kGd", "fixed", 0.11)

validateModel(t3)

makeModel(t3)
printInfo(t3)

simResults <-simulateModel(t3, times=seq(0, 50, by = 0.5))

# Plot results, but skip L, because value is too hight
plotResults(simResults[,c(1, 3:8)], title="Yeast Heterotrimeric G Protein Cycle")

simResults.stoch <- solveStoch(t3, 50)
plotResults(simResults.stoch[,c(1, 3:8)], title="Yeast Heterotrimeric G Protein Cycle)")


################################################
#rm(list = ls())

rt1 <- newModel("Rule test #1")

addMAreaction(rt1, "A -> B", r1="m")

addSpecies(rt1, "A", 10)
addSpecies(rt1, "B", 0)
addSpecies(rt1, "C", 5)

addMAreactRate(rt1, "m", "assigned", "k1*A")
addParameters(rt1, "k1", 1)
addParameters(rt1, "k2", 1)

addRule(rt1, "rule1", "ODEs", "C=k2*A")

validateModel(rt1)

makeModel(rt1)
printInfo(rt1)

simResults <-simulateModel(rt1) 
plotResults(simResults, title="Model - rule - test")

simResults.stoch <- solveStoch(rt1)
plotResults(simResults.stoch, title="Model - rule - test")

################################################

#rm(list = ls())

tr2 <- newModel("Zero order rates with rules (for test only)")

addMAreaction(tr2, "null  -> P", "k")

addSpecies(tr2, "P", 0)

addMAreactRate(tr2, "k", "fixed", 1)

addRule(tr2, "rule1", "ODEs", "C=k2*P")
addParameters(tr2, "k2", 1)
addSpecies(tr2, "C", 1)

makeModel(tr2)
printInfo(tr2)

simResults <-simulateModel(tr2)
plotResults(simResults, title="Zero order rates with rules (for test only)")

# Does not work for zero order rate - needs to be check!
simResults.stoch <- solveStoch(tr2)
plotResults(simResults.stoch, title="Zero order reaction (stochastic simulation)")

################################################

rm(list = ls())

ts <- newModel("HIV infection model")

addMAreaction(ts, "null  -> T", "l")
addMAreaction(ts, "T -> null", "r")
addMAreaction(ts, "V -> null", "c")
addMAreaction(ts, "T + V-> I", "b")
addMAreaction(ts, "I -> 900*V", "d")

addSpecies(ts, "T", 100)
addSpecies(ts, "V", 50000)
addSpecies(ts, "I", 150)

addMAreactRate(ts, "l", "fixed", 80)
addMAreactRate(ts, "b", "fixed", 0.00002)
addMAreactRate(ts, "r", "fixed", 0.15)
addMAreactRate(ts, "d", "fixed", 0.55)
addMAreactRate(ts, "c", "fixed", 5.5)

makeModel(ts)
#printInfo(ts)

simResults <- simulateModel(ts)
simResults <- cbind(simResults, log10V=log10(simResults$V))
plotResults(simResults[,c("time", "T", "I", "log10V")], title="HIV infection model")
plot(simResults)


# Does not work for zero order rate - needs to be check!
simResults.stoch <- solveStoch(ts, 100)
plotResults(simResults.stoch, title="Zero order reaction (stochastic simulation)")

