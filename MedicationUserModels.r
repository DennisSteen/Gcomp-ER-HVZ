for(t in TimePoints[-1]){
  ModelData <- as.data.frame(CompleteDataset %>%
                               filter(dag90 == t))
  
  if(t == 0){
    ModelDataX <- as.data.frame(ModelData %>% 
                                  dplyr::select(leeftijd_index,geslacht,BN_INKOMEN_OMS,BN_OPLEIDING_OMS,BN_SOCIALE_KLASSE_OMS
                                                ,FKG,DKG,HKG
                                                ,AV_INDICATOR,TV_INDICATOR,ER_INDICATOR,HERVERZEKERING_ER,ER_REGELING
                                                ,Diabetes,COPD,Reuma,Depressie) %>%
                                  mutate(leeftijd2 = leeftijd_index * leeftijd_index
                                         ,lft_M = ifelse(geslacht == 'M', leeftijd_index,0)) %>%
                                  mutate_if(is.character, as.factor))
  } else {
    ModelDataX <- as.data.frame(ModelData %>% 
                                  dplyr::select(leeftijd_index,geslacht,BN_INKOMEN_OMS,BN_OPLEIDING_OMS,BN_SOCIALE_KLASSE_OMS
                                                ,FKG,DKG,HKG
                                                ,AV_INDICATOR,TV_INDICATOR,ER_INDICATOR,HERVERZEKERING_ER,ER_REGELING
                                                ,Diabetes,COPD,Reuma,Depressie
                                                ,Paying_lag
                                                ,paste(Medications,"lag",sep="_")) %>%
                                  mutate(leeftijd2 = leeftijd_index * leeftijd_index
                                         ,lft_M = ifelse(geslacht == 'M', leeftijd_index,0)) %>%
                                  mutate_if(is.character, as.factor))
  }
  
  for(m in Medications){
    tryCatch({
      Y <- ModelData[ModelData[,paste(m,"_lag",sep="")] == 0,paste("user_",m,sep="")]
      assign(paste("model_user_",m,"_",t,sep=""),SuperLearner(Y,ModelDataX[ModelData[,paste(m,"_lag",sep="")] == 0,],family=binomial(),SL.library = SLvector))
      saveRDS(get(paste("model_user_",m,"_",t,sep="")),file=paste("L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Models/model_user_",m,"_",t,".rda",sep=""))
    }, error = function(e){cat("Iteration ",t,"ERROR: ",conditionMessage(e),"\n")})
  }
  
  ##Delete models from workspace
  rm(list = setdiff(ls(pattern = "^model_"),lsf.str()))
  rm(list = c("ModelData","ModelDataX","Y"))
}