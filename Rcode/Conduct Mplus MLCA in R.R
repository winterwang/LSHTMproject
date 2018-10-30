 # Date: 2018-10-29 created
 # Date updated: "Mon Oct 29 13:39:14 2018"
 # Objective: Conduct MLCA modelling in R using Mplus

library(MplusAutomation)
setwd("~/ドキュメント/githubprojects/LSHTMproject/Rcode/MplusCode")
# Run LCA without multilevel structure, only day level --------------------

# Class = 1
runModels("LCA01.inp", showOutput = TRUE, Mplus_command = "/opt/mplus/8.1/mplus")

# Class = 2
runModels("LCA02.inp", showOutput = TRUE, Mplus_command = "/opt/mplus/8.1/mplus")

# -> Rstudio crashes so back to work in terminals