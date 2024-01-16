.libPaths(c(.libPaths(),"L:/afdelingen/Actuariaat_Business/Centraal Archief/06 Informatievoorziening en infrastructuur/R packages"))

library(DBI)
library(odbc)
library(dplyr)
library(SuperLearner)
library(glmnet)
library(randomForest)
library(MASS)
library(gam)

ScriptLocation <- "L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Scripts/R/"
ModelLocation <- "//KA0089.loods2.org/Actuariaat/SAS Data/DennisS/PhD/A4/Models"
DataLocation <- "//KA0089.loods2.org/Actuariaat/SAS Data/DennisS/PhD/A4"

source(paste0(ScriptLocation,"runICE - Combined.r"))
source(paste0(ScriptLocation,"runICE - CVE.r"))
source(paste0(ScriptLocation,"runICE - Death.r"))
source(paste0(ScriptLocation,"runICE - Combined - Statin.r"))
source(paste0(ScriptLocation,"runICE - CVE - Statin.r"))
source(paste0(ScriptLocation,"runICE - Death - Statin.r"))

source(paste0(ScriptLocation,"p_values.r"))
