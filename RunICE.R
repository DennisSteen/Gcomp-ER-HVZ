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
ModelLocation <- "//KA0089.loods2.org/Actuariaat/SAS Data/DennisS/PhD/A4/Models"
DataLocation <- "//KA0089.loods2.org/Actuariaat/SAS Data/DennisS/PhD/A4"

CompleteDataset <- tbl(con, "DS_PhD_CompleteSetA4_90")
source(paste0(ScriptLocation,"ExtraSuperLearners.r"))

StatinUsers <- as.character(as.data.frame(CompleteDataset %>% filter(dag90 == 0, user_C10AA == 1) %>% dplyr::select(ZCL_REL_NR))[[1]])

runICE <- function(dataSet, SLvector, ReplaceColumnNames = NA, ReplaceColumnValues = NA, BootStraps = 1, Estimate = TRUE, SAVE = FALSE, name = "NaturalCourse"){
  
  TimePoints <- as.data.frame(dataSet %>% 
                                filter(dag90 < 3000) %>%
                                distinct(dag90) %>%
                                arrange(dag90))$dag90
  
  Medications <- c("C03AA","C03DA","C07A","C08","C09A","C09C","C10AA","C10AZ")
  Preds <- matrix(rep(NA,BootStraps * length(TimePoints)),ncol = BootStraps )
  
  Models <- vector(mode = "list", length = length(TimePoints))
  
  Preds[TimePoints == max(TimePoints)] <- mean((as.data.frame(CompleteDataset) %>% filter(dag90 == max(TimePoints)) %>% dplyr::select(CVE))$CVE)
  
  pb <- txtProgressBar(min = 0, max = 1, style = 3)
  

    fullDataSetBase <- as.data.frame(dataSet %>%
                                   dplyr::select(ZCL_REL_NR, dag90,leeftijd_index,geslacht,BN_INKOMEN_OMS,BN_OPLEIDING_OMS,BN_SOCIALE_KLASSE_OMS
                                                 ,FKG,DKG,HKG
                                                 ,AV_INDICATOR,TV_INDICATOR
                                                 ,ER_INDICATOR,HERVERZEKERING_ER,ER_REGELING
                                                 ,Diabetes,COPD,Reuma,Depressie
                                                 ,Paying
                                                 ,YearPassed
                                                 ,paste(Medications,"lag",sep="_")
                                                 ,paste("user",Medications,sep="_")
                                                 ,paste("paying",Medications,"lag",sep="_")
                                                 ,paste("paying",Medications,sep="_")
                                                 ,paste("adherence",Medications,"lag",sep="_")
                                                 ,paste("pickup",Medications,"lag",sep="_")
                                                 ,paste("PickUp",Medications,sep="_")
                                                 ,Switch, Censor, CVE, Death)) %>%
      mutate("Class_C03AA" = as.factor(ifelse(user_C03AA == 0,"1. No-User",ifelse(PickUp_C03AA == 0,"2. User, No pick up",ifelse(paying_C03AA == 0, "3. Pick-up, No payment","4. Pick-up, payment"))))
             ,"Class_C03DA" = as.factor(ifelse(user_C03DA == 0,"1. No-User",ifelse(PickUp_C03DA == 0,"2. User, No pick up",ifelse(paying_C03DA == 0, "3. Pick-up, No payment","4. Pick-up, payment"))))
             ,"Class_C07A" = as.factor(ifelse(user_C07A == 0,"1. No-User",ifelse(PickUp_C07A == 0,"2. User, No pick up",ifelse(paying_C07A == 0, "3. Pick-up, No payment","4. Pick-up, payment"))))
             ,"Class_C08" = as.factor(ifelse(user_C08 == 0,"1. No-User",ifelse(PickUp_C08 == 0,"2. User, No pick up",ifelse(paying_C08 == 0, "3. Pick-up, No payment","4. Pick-up, payment"))))
             ,"Class_C09A" = as.factor(ifelse(user_C09A == 0,"1. No-User",ifelse(PickUp_C09A == 0,"2. User, No pick up",ifelse(paying_C09A == 0, "3. Pick-up, No payment","4. Pick-up, payment"))))
             ,"Class_C09C" = as.factor(ifelse(user_C09C == 0,"1. No-User",ifelse(PickUp_C09C == 0,"2. User, No pick up",ifelse(paying_C09C == 0, "3. Pick-up, No payment","4. Pick-up, payment"))))
             ,"Class_C10AA" = as.factor(ifelse(user_C10AA == 0,"1. No-User",ifelse(PickUp_C10AA == 0,"2. User, No pick up",ifelse(paying_C10AA == 0, "3. Pick-up, No payment","4. Pick-up, payment"))))
             ,"Class_C10AZ" = as.factor(ifelse(user_C10AZ == 0,"1. No-User",ifelse(PickUp_C10AZ == 0,"2. User, No pick up",ifelse(paying_C10AZ == 0, "3. Pick-up, No payment","4. Pick-up, payment"))))) %>%
      mutate(leeftijd2 = leeftijd_index * leeftijd_index
             ,lft_M = ifelse(geslacht == 'M', leeftijd_index,0)) %>%
      mutate_if(is.character, as.factor) 
    
  for(B in 1:BootStraps){   

      unique_ZCL <- unique(fullDataSetBase$ZCL_REL_NR)
      sample_ZCL <- sample(unique_ZCL,length(unique_ZCL),replace = (B>1))
      fullDataSet <- fullDataSetBase %>%
        filter(ZCL_REL_NR %in% sample_ZCL)
      weights <- data.frame(table(sample_ZCL))
      weights[,1] <- as.numeric(as.character(weights[,1]))


  
    dimFDS <- dim(fullDataSet[fullDataSet$dag90 != 0,])
  

    NewDataSet <- fullDataSet
    if(length(ReplaceColumnNames) == 1){
      if(!is.na(ReplaceColumnNames)){
        if(length(ReplaceColumnNames) != length(ReplaceColumnValues)){stop("Replace column values and names do not have the same length")}
        NewDataSet[NewDataSet$dag90 != 0,ReplaceColumnNames] <- ifelse(!is.na(NewDataSet[NewDataSet$dag90 != 0,ReplaceColumnNames]),matrix(rep(ReplaceColumnValues,dimFDS[1]),byrow = T,ncol=length(ReplaceColumnValues)),NA)
      }
    } else{
      if(length(ReplaceColumnNames) != length(ReplaceColumnValues)){stop("Replace column values and names do not have the same length")}
      NewDataSet[NewDataSet$dag90 != 0,ReplaceColumnNames] <- ifelse(!is.na(NewDataSet[NewDataSet$dag90 != 0,ReplaceColumnNames]),matrix(rep(ReplaceColumnValues,dimFDS[1]),byrow = T,ncol=length(ReplaceColumnValues)),NA)
    }
    
    NewDataSet <- NewDataSet %>%
                    mutate("Class_C03AA" = as.factor(ifelse(user_C03AA == 0,"1. No-User",ifelse(PickUp_C03AA == 0,"2. User, No pick up",ifelse(paying_C03AA == 0, "3. Pick-up, No payment","4. Pick-up, payment"))))
                            ,"Class_C03DA" = as.factor(ifelse(user_C03DA == 0,"1. No-User",ifelse(PickUp_C03DA == 0,"2. User, No pick up",ifelse(paying_C03DA == 0, "3. Pick-up, No payment","4. Pick-up, payment"))))
                            ,"Class_C07A" = as.factor(ifelse(user_C07A == 0,"1. No-User",ifelse(PickUp_C07A == 0,"2. User, No pick up",ifelse(paying_C07A == 0, "3. Pick-up, No payment","4. Pick-up, payment"))))
                            ,"Class_C08" = as.factor(ifelse(user_C08 == 0,"1. No-User",ifelse(PickUp_C08 == 0,"2. User, No pick up",ifelse(paying_C08 == 0, "3. Pick-up, No payment","4. Pick-up, payment"))))
                            ,"Class_C09A" = as.factor(ifelse(user_C09A == 0,"1. No-User",ifelse(PickUp_C09A == 0,"2. User, No pick up",ifelse(paying_C09A == 0, "3. Pick-up, No payment","4. Pick-up, payment"))))
                            ,"Class_C09C" = as.factor(ifelse(user_C09C == 0,"1. No-User",ifelse(PickUp_C09C == 0,"2. User, No pick up",ifelse(paying_C09C == 0, "3. Pick-up, No payment","4. Pick-up, payment"))))
                            ,"Class_C10AA" = as.factor(ifelse(user_C10AA == 0,"1. No-User",ifelse(PickUp_C10AA == 0,"2. User, No pick up",ifelse(paying_C10AA == 0, "3. Pick-up, No payment","4. Pick-up, payment"))))
                            ,"Class_C10AZ" = as.factor(ifelse(user_C10AZ == 0,"1. No-User",ifelse(PickUp_C10AZ == 0,"2. User, No pick up",ifelse(paying_C10AZ == 0, "3. Pick-up, No payment","4. Pick-up, payment"))))) %>%
                    mutate(Paying_YP = Paying * YearPassed)
      
    
    ##Following Appendix 1 of arXiv:2306.10976
    for(t in rev(TimePoints[-1])){ #Have to skip the last one
      SS <- NewDataSet$dag90 == t - 90 & NewDataSet$Switch == 0 & NewDataSet$Death == 0 & NewDataSet$Censor == 0
      
      ## Set modeldata
      ModelData <- NewDataSet %>%
                    filter(dag90 == t & Switch == 0 & Death == 0 & Censor == 0) %>%
                    dplyr::select(ZCL_REL_NR, CVE) %>%
                    left_join((fullDataSet %>% filter(dag90 == t-90) %>% distinct()),by = join_by(ZCL_REL_NR)) %>%
                    left_join(weights, by = join_by(ZCL_REL_NR == sample_ZCL))


      ## Select predictors
      ModelDataX <- ModelData %>%
        dplyr::select(leeftijd_index,geslacht
                      ,BN_INKOMEN_OMS,BN_OPLEIDING_OMS,BN_SOCIALE_KLASSE_OMS
                      ,FKG,DKG,HKG
                      ,AV_INDICATOR,TV_INDICATOR
                      ,ER_INDICATOR,HERVERZEKERING_ER,ER_REGELING
                      ,Diabetes,COPD,Reuma,Depressie
                      ,Paying, YearPassed
                      ,paste("Class",Medications,sep="_")
        ) %>%
        mutate(leeftijd2 = leeftijd_index * leeftijd_index
               ,lft_M = ifelse(geslacht == 'M', leeftijd_index,0)
               ,Paying_YP = Paying * YearPassed) %>%
        mutate_if(is.character, as.factor)

      ModelDataX <- as.data.frame(model.matrix(~ leeftijd_index + geslacht 
                                 + BN_INKOMEN_OMS + BN_OPLEIDING_OMS + BN_SOCIALE_KLASSE_OMS
                                 + FKG + DKG + HKG 
                                 + AV_INDICATOR + TV_INDICATOR
                                 + ER_INDICATOR + HERVERZEKERING_ER + ER_REGELING 
                                 + Diabetes + COPD + Reuma + Depressie 
                                 + Paying_YP
                                 + Class_C03AA:YearPassed + Class_C03DA:YearPassed + Class_C07A:YearPassed + Class_C08:YearPassed + Class_C09A:YearPassed + Class_C09C:YearPassed + Class_C10AA:YearPassed + Class_C10AZ:YearPassed - 1 , ModelDataX))

      ##Set Outcome
        Y <- ModelData$CVE.x

      ##Create model
      if(Estimate == TRUE){
        assign(paste("model",name,t,sep="_"),SuperLearner(Y,ModelDataX,family=binomial(),SL.library = SLvector)) ##Step 1
        if(SAVE == TRUE){
          saveRDS(get(paste("model",name,t,sep="_")),file=paste(ModelLocation,"/model_",name,"_",t,".rda",sep=""))
        }
      } else {
        assign(paste("model",name,t,sep="_"),readRDS(paste(ModelLocation,"/model_",name,"_",t,".rda",sep="")))
      }
      
      #Models[[length(TimePoints) - sum(TimePoints > t)]] <- get(paste("model",name,t,sep="_"))
      #names(Models)[length(TimePoints) - sum(TimePoints > t)] <- paste("M",t,sep="_")
      
      ## Predict values for t - 1 
      NewDataSet$CVE[SS] <- pmax(predict(get(paste("model",name,t,sep="_")),newdata = as.data.frame(model.matrix(~ leeftijd_index + geslacht 
                                                                                                                 + BN_INKOMEN_OMS + BN_OPLEIDING_OMS + BN_SOCIALE_KLASSE_OMS
                                                                                                                 + FKG + DKG + HKG 
                                                                                                                 + AV_INDICATOR + TV_INDICATOR
                                                                                                                 + ER_INDICATOR + HERVERZEKERING_ER + ER_REGELING 
                                                                                                                 + Diabetes + COPD + Reuma + Depressie 
                                                                                                                 + Paying_YP
                                                                                                                 + Class_C03AA:YearPassed + Class_C03DA:YearPassed + Class_C07A:YearPassed + Class_C08:YearPassed + Class_C09A:YearPassed + Class_C09C:YearPassed + Class_C10AA:YearPassed + Class_C10AZ:YearPassed - 1 , NewDataSet[SS,])),type="response")$pred,NewDataSet[SS,"CVE"]) ##Step 2
      
      ##Delete models and data from workspace
      rm(list = setdiff(ls(pattern = "^model_"),lsf.str()))
      rm(list = c("ModelData","ModelDataX","Y"))
      

      Preds[TimePoints == t-90,B] <- mean(NewDataSet[SS,"CVE"])
      
      setTxtProgressBar(pb, (B - (t-90)/max(TimePoints)) / BootStraps )
    }
  }
  
  rownames(Preds) <- TimePoints
  out <- list("Information"=c("Mean"=mean(Preds[2,1]),quantile(Preds[2,],probs=0.025,na.rm = T),quantile(Preds[2,],probs=0.975,na.rm=T)),"Hazards" = Preds)#, "Models" = Models)
  
  close(pb)
  return(out)
}
#runICE(CompleteDataset,c("SL.mean"),BootStraps=1)
#NC <- runICE(CompleteDataset,c("SL.glm"),BootStraps=1)
#Adjusted <- runICE(CompleteDataset,c("SL.glm"),BootStraps=1
#       ,ReplaceColumnNames = c(paste0("paying_",Medications))
#       ,ReplaceColumnValues = rep(0,8))

                         
(start.time <- Sys.time())
NaturalCourse <- runICE(CompleteDataset,c("SL.glm"),BootStraps=75, Estimate = TRUE, SAVE = FALSE, name = "NaturalCourse")
end.time <- Sys.time()
end.time - start.time
saveRDS(NaturalCourse,file=paste0(DataLocation,"/NaturalCourse_glm_Bootstrap.rda"))

gc()

(start.time <- Sys.time())
Medications <- c("C03AA","C03DA","C07A","C08","C09A","C09C","C10AA","C10AZ")
NoPayments <- runICE(CompleteDataset,c("SL.glm")
       ,ReplaceColumnNames = c(paste0("paying_",Medications))
       ,ReplaceColumnValues = rep(0,8)
       ,BootStraps=75, Estimate = TRUE, SAVE = FALSE, name = "NoPayments")
end.time <- Sys.time()
saveRDS(NoPayments,file=paste0(DataLocation,"/NoPayments_glm_Bootstrap.rda"))

end.time - start.time

cbind(NaturalCourse$Hazards,NoPayments$Hazards)
