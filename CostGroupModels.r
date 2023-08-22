for(t in TimePoints){
  if(t == 0){
    ModelData <- as.data.frame(CompleteDataset %>%
                                 filter(dag90 == t))
    ModelDataX <- as.data.frame(ModelData %>% 
                                  dplyr::select(leeftijd_index,geslacht,BN_INKOMEN_OMS,BN_OPLEIDING_OMS,BN_SOCIALE_KLASSE_OMS) %>%
                                  mutate(leeftijd2 = leeftijd_index * leeftijd_index
                                         ,lft_M = ifelse(geslacht == 'M', leeftijd_index,0)) %>%
                                  mutate_if(is.character, as.factor))
  } else {
    ModelData <- as.data.frame(CompleteDataset %>%
                                 filter(dag90 == t & YearPassed < 90/ 365))
    ModelDataX <- as.data.frame(ModelData %>% 
                                  dplyr::select(leeftijd_index,geslacht,BN_INKOMEN_OMS,BN_OPLEIDING_OMS,BN_SOCIALE_KLASSE_OMS
                                                  ,AV_lag,TV_lag,VER_lag,HER_lag,GBER_lag,Paying_lag
                                                  ,Diabetes_lag,COPD_lag,Reuma_lag,Depressie_lag,FKG_lag,DKG_lag,HKG_lag
                                                  ,paste(Medications,"lag",sep="_")) %>%
                                  mutate(leeftijd2 = leeftijd_index * leeftijd_index
                                         ,lft_M = ifelse(geslacht == 'M', leeftijd_index,0)) %>%
                                  mutate_if(is.character, as.factor))
  }
  
  ##FKG Model
  Y <- ModelData$FKG
  assign(paste("model_FKG_",t,sep=""),SuperLearner(Y,ModelDataX,family=binomial(),SL.library = SLvector))
  saveRDS(get(paste("model_FKG_",t,sep="")),file=paste("L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Models/model_FKG_",t,".rda",sep=""))
  
  ##DKG Model
  Y <- ModelData$DKG
  assign(paste("model_DKG_",t,sep=""),SuperLearner(Y,ModelDataX,family=binomial(),SL.library = SLvector))
  saveRDS(get(paste("model_DKG_",t,sep="")),file=paste("L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Models/model_DKG_",t,".rda",sep=""))
  
  ##HKG Model
  Y <- ModelData$HKG
  assign(paste("model_HKG_",t,sep=""),SuperLearner(Y,ModelDataX,family=binomial(),SL.library = SLvector))
  saveRDS(get(paste("model_HKG_",t,sep="")),file=paste("L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Models/model_HKG_",t,".rda",sep=""))
  
  ##Delete models from workspace
  rm(list = setdiff(ls(pattern = "^model_"),lsf.str()))
  rm(list = c("ModelData","ModelDataX","Y"))
}