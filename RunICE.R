.libPaths(c(.libPaths(),"L:/afdelingen/Actuariaat_Business/Centraal Archief/06 Informatievoorziening en infrastructuur/R packages"))

library(DBI)
library(odbc)
library(dplyr)
library(SuperLearner)
library(glmnet)
library(randomForest)
library(MASS)
library(gam)
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};Server=ka0327.loods2.org;Trusted_Connection=True;Database=SBX210", timeout = 10)
ScriptLocation <- "L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Scripts/R/"

CompleteDataset <- tbl(con, "DS_PhD_CompleteSetA4_90")
source(paste0(ScriptLocation,"ExtraSuperLearners.r"))


runICE <- function(dataSet, SLvector, ReplaceColumnNames = NA, ReplaceColumnValues = NA, BootStraps = 1){
  
  TimePoints <- as.data.frame(dataSet %>% 
                                filter(dag90 < 3000) %>%
                                distinct(dag90) %>%
                                arrange(dag90))$dag90
  
  Medications <- c("C03AA","C03DA","C07A","C08","C09A","C09C","C10AA","C10AZ")
  Preds <- rep(NA,BootStraps)
  
  pb <- txtProgressBar(min = 0, max = 1, style = 3)
  
  
  
  fullDataSet <- as.data.frame(dataSet %>%
                                 dplyr::select(dag90,leeftijd_index,geslacht,BN_INKOMEN_OMS,BN_OPLEIDING_OMS,BN_SOCIALE_KLASSE_OMS
                                               ,FKG,DKG,HKG
                                               ,AV_INDICATOR,TV_INDICATOR,ER_INDICATOR,HERVERZEKERING_ER,ER_REGELING
                                               ,Diabetes,COPD,Reuma,Depressie
                                               ,Paying
                                               ,paste("user",Medications,sep="_")
                                               ,paste("paying",Medications,"lag",sep="_")
                                               ,paste("paying",Medications,sep="_")
                                               ,paste("adherence",Medications,"lag",sep="_")
                                               ,paste("pickup",Medications,"lag",sep="_")
                                               ,paste("PickUp",Medications,sep="_")
                                               ,Switch, Censor, CVE, Death)) %>%
    mutate(leeftijd2 = leeftijd_index * leeftijd_index
           ,lft_M = ifelse(geslacht == 'M', leeftijd_index,0)) %>%
    mutate_if(is.character, as.factor)
  
  dimFDS <- dim(fullDataSet)
  
  for(B in 1:BootStraps){
    NewDataSet <- fullDataSet
    if(length(ReplaceColumnNames) == 1){
      if(!is.na(ReplaceColumnNames)){
        if(length(ReplaceColumnNames) != length(ReplaceColumnValues)){stop("Replace column values and names do not have the same length")}
        NewDataSet[,ReplaceColumnNames] <- ifelse(!is.na(NewDataSet[,ReplaceColumnNames]),matrix(rep(ReplaceColumnValues,dimFDS[1]),byrow = T,ncol=length(ReplaceColumnValues)),NA)
      }
    } else{
      if(length(ReplaceColumnNames) != length(ReplaceColumnValues)){stop("Replace column values and names do not have the same length")}
      NewDataSet[,ReplaceColumnNames] <- ifelse(!is.na(NewDataSet[,ReplaceColumnNames]),matrix(rep(ReplaceColumnValues,dimFDS[1]),byrow = T,ncol=length(ReplaceColumnValues)),NA)
    }
      
    
    ##Following Appendix 1 of arXiv:2306.10976
    for(t in rev(TimePoints[-1])){ #Have to skip the last one
      SS <- NewDataSet$dag90 == t - 90
      
      ## Set modeldata
      ModelData <- fullDataSet %>%
        filter(dag90 == t & Switch == 0)
      
      ## Select predictors
      ModelDataX <- ModelData %>% 
        dplyr::select(leeftijd_index,geslacht,BN_INKOMEN_OMS,BN_OPLEIDING_OMS,BN_SOCIALE_KLASSE_OMS
                      ,FKG,DKG,HKG
                      ,AV_INDICATOR,TV_INDICATOR,ER_INDICATOR,HERVERZEKERING_ER,ER_REGELING
                      ,Diabetes,COPD,Reuma,Depressie
                      ,Paying
                      ,paste("user",Medications,sep="_")
                      ,paste("pickup",Medications,"lag",sep="_")
                      ,paste("paying",Medications,"lag",sep="_")
                      ,paste("PickUp",Medications,sep="_")
                      ,paste("paying",Medications,sep="_")
        ) %>%
        mutate(leeftijd2 = leeftijd_index * leeftijd_index
               ,lft_M = ifelse(geslacht == 'M', leeftijd_index,0)) %>%
        mutate_if(is.character, as.factor)
      
      ##Set Outcome
        Y <- c(NewDataSet %>%
                 filter(dag90 == t & Switch == 0) %>%
                 dplyr::select(CVE))[[1]]
      
      ##Create model
        assign(paste("model_CVE_",t,sep=""),SuperLearner(Y,ModelDataX,family=binomial(),SL.library = SLvector)) ##Step 1
       saveRDS(get(paste("model_CVE_",t,sep="")),file=paste("L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Models/model_CVE_",t,".rda",sep=""))
      
      ## Predict values for t - 1 
      NewDataSet$CVE[SS] <- pmax(predict(get(paste("model_CVE_",t,sep="")),newdata = NewDataSet[SS,],type="response")$pred,NewDataSet[SS,"CVE"]) ##Step 2
      
      ##Delete models and data from workspace
      rm(list = setdiff(ls(pattern = "^model_"),lsf.str()))
      rm(list = c("ModelData","ModelDataX","Y"))
      
      setTxtProgressBar(pb, (B - (t-90)/max(TimePoints)) / BootStraps )
    }
    
    Preds[B] <- (NewDataSet %>% filter(dag90 == 90) %>% dplyr::select(CVE) %>% summarise(mean(CVE)))[[1]]
  }
  
  out <- c("Mean"=mean(Preds),quantile(Preds,probs=0.025),quantile(Preds,probs=0.975))
  
  close(pb)
  return(out)
}
#runICE(CompleteDataset,c("SL.glmnet2"),BootStraps=1)
                         
start.time <- Sys.time()                         
runICE(CompleteDataset,c("SL.glmnet2","SL.glmnet.ridge","SL.gam2","SL.glm"),BootStraps=1)
runICE(CompleteDataset,c("SL.glmnet2","SL.glmnet.ridge","SL.gam2","SL.glm"),BootStraps=1
       ,c(paste("paying",Medications,"lag",sep="_"),paste("paying",Medications,sep="_"))
       ,c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
end.time <- Sys.time()
end.time - start.time
