for(t in TimePoints){
  ModelData <- as.data.frame(CompleteDataset %>%
                               filter(dag90 == t))
  
  if(t == 0){
    ModelDataX <- as.data.frame(ModelData %>% 
                                  dplyr::select(leeftijd_index,geslacht,BN_INKOMEN_OMS,BN_OPLEIDING_OMS,BN_SOCIALE_KLASSE_OMS
                                                ,FKG,DKG,HKG
                                                ,AV_INDICATOR,TV_INDICATOR,ER_INDICATOR,HERVERZEKERING_ER,ER_REGELING) %>%
                                  mutate(leeftijd2 = leeftijd_index * leeftijd_index
                                         ,lft_M = ifelse(geslacht == 'M', leeftijd_index,0)) %>%
                                  mutate_if(is.character, as.factor))
  } else {
    ModelDataX <- as.data.frame(ModelData %>% 
                                  dplyr::select(leeftijd_index,geslacht,BN_INKOMEN_OMS,BN_OPLEIDING_OMS,BN_SOCIALE_KLASSE_OMS
                                                ,FKG,DKG,HKG
                                                ,AV_INDICATOR,TV_INDICATOR,ER_INDICATOR,HERVERZEKERING_ER,ER_REGELING
                                                ,Paying_lag
                                                ,Diabetes_lag,COPD_lag,Reuma_lag,Depressie_lag
                                                ,paste(Medications,"lag",sep="_")) %>%
                                  mutate(leeftijd2 = leeftijd_index * leeftijd_index
                                         ,lft_M = ifelse(geslacht == 'M', leeftijd_index,0)) %>%
                                  mutate_if(is.character, as.factor))
  }
  
  ##Diabetes Model
  tryCatch({
    Y<- ModelData$Diabetes[ModelData$Diabetes_lag == 0]
    assign(paste("model_Diabetes_",t,sep=""),SuperLearner(Y,ModelDataX[ModelData$Diabetes_lag == 0,],family=binomial(),SL.library = SLvector))
    saveRDS(get(paste("model_Diabetes_",t,sep="")),file=paste("L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Models/model_Diabetes_",t,".rda",sep=""))
  }, error = function(e){cat("Iteration ",t,"ERROR: ",conditionMessage(e),"\n")})
  
  ##COPD Model
  tryCatch({
    Y<- ModelData$COPD[ModelData$COPD_lag == 0]
    assign(paste("model_COPD_",t,sep=""),SuperLearner(Y,ModelDataX[ModelData$COPD_lag == 0,],family=binomial(),SL.library = SLvector))
    saveRDS(get(paste("model_COPD_",t,sep="")),file=paste("L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Models/model_COPD_",t,".rda",sep=""))
  }, error = function(e){cat("Iteration ",t,"ERROR: ",conditionMessage(e),"\n")})
  
  ##Reuma model
  tryCatch({
    Y<- ModelData$Reuma[ModelData$Reuma_lag == 0]
    assign(paste("model_Reuma_",t,sep=""),SuperLearner(Y,ModelDataX[ModelData$Reuma_lag == 0,],family=binomial(),SL.library = SLvector))
    saveRDS(get(paste("model_Reuma_",t,sep="")),file=paste("L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Models/model_Reuma_",t,".rda",sep=""))
  }, error = function(e){cat("Iteration ",t,"ERROR: ",conditionMessage(e),"\n")})
  
  ##Depression model
  tryCatch({
    Y<- ModelData$Depressie[ModelData$Depressie_lag == 0]
    assign(paste("model_Depressie_",t,sep=""),SuperLearner(Y,ModelDataX[ModelData$Depressie_lag == 0,],family=binomial(),SL.library = SLvector))
    saveRDS(get(paste("model_Depressie_",t,sep="")),file=paste("L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Models/model_Depressie_",t,".rda",sep=""))
  }, error = function(e){cat("Iteration ",t,"ERROR: ",conditionMessage(e),"\n")})

  ##Delete models from workspace
  rm(list = setdiff(ls(pattern = "^model_"),lsf.str()))
  rm(list = c("ModelData","ModelDataX","Y"))
}