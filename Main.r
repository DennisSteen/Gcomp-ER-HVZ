library(DBI)
library(odbc)
library(dplyr)
library(SuperLearner)
con <- dbConnect(odbc::odbc(), .connection_string = "", timeout = 10)
ScriptLocation <- ""

CompleteDataset <- tbl(con, "DS_PhD_CompleteSetA4_90")

TimePoints <- as.data.frame(CompleteDataset %>% 
  filter(dag90 < 3000) %>%
  distinct(dag90) %>%
  arrange(dag90))$dag90

Medications <- c("C03AA","C03DA","C07A","C08","C09A","C09C","C10AA","C10AZ")

source(paste0(ScriptLocation,"ExtraSuperLearners.r"))
SLvector <- c("SL.glmnet","SL.glmnet.ridge","SL.randomForest","SL.stepAIC","SL.gam2")

source(paste0(ScriptLocation,"SwitchModels.r"))
source(paste0(ScriptLocation,"CostGroupModels.r"))
source(paste0(ScriptLocation,"PolisModels.r"))
source(paste0(ScriptLocation,"ComorbiditieModels.r"))
source(paste0(ScriptLocation,"MedicationUserModels.r"))
source(paste0(ScriptLocation,"MedicationPickUpModels.r"))
source(paste0(ScriptLocation,"MedicationPayingModels.r"))
source(paste0(ScriptLocation,"MedicationAdherenceModels.r"))
source(paste0(ScriptLocation,"CVEModels.r"))
source(paste0(ScriptLocation,"DeathModels.r"))
source(paste0(ScriptLocation,"CensorModels.r"))
