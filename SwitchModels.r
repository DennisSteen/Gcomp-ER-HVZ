for(t in TimePoints[-c(1:5)]){ #Can skip the first 6 since there won't be a switch in those timepoints
  ModelData <- as.data.frame(CompleteDataset %>%
                               filter(dag90 == t & (YearPassed < 90/365 | Switch == 1)))
  
  if(t == 0){
    ModelDataX <- as.data.frame(ModelData %>% 
                                  dplyr::select(leeftijd_index,geslacht,BN_INKOMEN_OMS,BN_OPLEIDING_OMS,BN_SOCIALE_KLASSE_OMS) %>%
                                  mutate(leeftijd2 = leeftijd_index * leeftijd_index
                                         ,lft_M = ifelse(geslacht == 'M', leeftijd_index,0)) %>%
                                  mutate_if(is.character, as.factor))
  } else {
    ModelDataX <- as.data.frame(ModelData %>% 
                                  dplyr::select(leeftijd_index,geslacht,BN_INKOMEN_OMS,BN_OPLEIDING_OMS,BN_SOCIALE_KLASSE_OMS
                                                ,AV_lag,TV_lag,VER_lag,HER_lag,GBER_lag,Paying_lag
                                                ,Diabetes_lag,COPD_lag,Reuma_lag,Depressie_lag,FKG_lag,DKG_lag,HKG_lag
                                                ,paste(Medications,"lag",sep="_")) %>%
                                  mutate(leeftijd2 = leeftijd_index * leeftijd_index
                                         ,lft_M = ifelse(geslacht == 'M', leeftijd_index,0)) %>%
                                  mutate_if(is.character, as.factor))
  }
  
  ##Switch Model
  tryCatch({
    Y <- ModelData$Switch
    assign(paste("model_Switch_",t,sep=""),SuperLearner(Y,ModelDataX,family=binomial(),SL.library = SLvector))
    saveRDS(get(paste("model_Switch_",t,sep="")),file=paste("L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Models/model_Switch_",t,".rda",sep=""))
  }, error = function(e){cat("Iteration ",t,", ERROR: ",conditionMessage(e),"\n",sep = "")})
  
  ##Delete models from workspace
  rm(list = setdiff(ls(pattern = "^model_"),lsf.str()))
  rm(list = c("ModelData","ModelDataX","Y"))
}