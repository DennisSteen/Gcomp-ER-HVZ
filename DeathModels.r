for(t in TimePoints[-c(1:5)]){ #Can skip the first 6 since there won't be a switch in those timepoints
  ModelData <- as.data.frame(CompleteDataset %>%
                               filter(dag90 == t & Switch == 0 & CVE == 0))
  
  
  ModelDataX <- as.data.frame(ModelData %>% 
                                dplyr::select(leeftijd_index,geslacht,BN_INKOMEN_OMS,BN_OPLEIDING_OMS,BN_SOCIALE_KLASSE_OMS
                                              ,FKG,DKG,HKG
                                              ,AV_INDICATOR,TV_INDICATOR,ER_INDICATOR,HERVERZEKERING_ER,ER_REGELING
                                              ,Diabetes,COPD,Reuma,Depressie
                                              ,Paying
                                              ,paste("user",Medications,sep="_")) %>%
                                mutate(leeftijd2 = leeftijd_index * leeftijd_index
                                       ,lft_M = ifelse(geslacht == 'M', leeftijd_index,0)) %>%
                                mutate_if(is.character, as.factor))
  
  ##Switch Model
  tryCatch({
    Y <- ModelData$Death
    assign(paste("model_Death_",t,sep=""),SuperLearner(Y,ModelDataX,family=binomial(),SL.library = SLvector))
    saveRDS(get(paste("model_Death_",t,sep="")),file=paste("L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Models/model_Death_",t,".rda",sep=""))
  }, error = function(e){cat("Iteration ",t,", ERROR: ",conditionMessage(e),"\n",sep = "")})
  
  ##Delete models from workspace
  rm(list = setdiff(ls(pattern = "^model_"),lsf.str()))
  rm(list = c("ModelData","ModelDataX","Y"))
}