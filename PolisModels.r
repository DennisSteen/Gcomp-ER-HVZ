for(t in TimePoints){
  if(t == 0){
    ModelData <- as.data.frame(CompleteDataset %>%
                                 filter(dag90 == t))
    ModelDataX <- as.data.frame(ModelData %>% 
                                  dplyr::select(leeftijd_index,geslacht,BN_INKOMEN_OMS,BN_OPLEIDING_OMS,BN_SOCIALE_KLASSE_OMS
                                                ,FKG,DKG,HKG) %>%
                                  mutate(leeftijd2 = leeftijd_index * leeftijd_index
                                         ,lft_M = ifelse(geslacht == 'M', leeftijd_index,0)) %>%
                                  mutate_if(is.character, as.factor))
  } else {
    ModelData <- as.data.frame(CompleteDataset %>%
                                 filter(dag90 == t & YearPassed < 90/ 365))
    ModelDataX <- as.data.frame(ModelData %>% 
                                  dplyr::select(leeftijd_index,geslacht,BN_INKOMEN_OMS,BN_OPLEIDING_OMS,BN_SOCIALE_KLASSE_OMS
                                                ,FKG,DKG,HKG
                                                ,AV_lag,TV_lag,VER_lag,HER_lag,GBER_lag,Paying_lag
                                                ,Diabetes_lag,COPD_lag,Reuma_lag,Depressie_lag
                                                ,paste(Medications,"lag",sep="_")) %>%
                                  mutate(leeftijd2 = leeftijd_index * leeftijd_index
                                         ,lft_M = ifelse(geslacht == 'M', leeftijd_index,0)) %>%
                                  mutate_if(is.character, as.factor))
  }
  
  ##AV Model
  tryCatch({
    Y<- ModelData$AV_INDICATOR
    assign(paste("model_AV_",t,sep=""),SuperLearner(Y,ModelDataX,family=binomial(),SL.library = SLvector))
    saveRDS(get(paste("model_AV_",t,sep="")),file=paste("L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Models/model_AV_",t,".rda",sep=""))
  }, error = function(e){cat("Iteration ",t,"ERROR: ",conditionMessage(e),"\n")})
  
  ##TV Model
  tryCatch({
    Y<- ModelData$TV_INDICATOR
    assign(paste("model_TV_",t,sep=""),SuperLearner(Y,ModelDataX,family=binomial(),SL.library = SLvector))
    saveRDS(get(paste("model_TV_",t,sep="")),file=paste("L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Models/model_TV_",t,".rda",sep=""))
  }, error = function(e){cat("Iteration ",t,"ERROR: ",conditionMessage(e),"\n")})
  
  ##VER model
  tryCatch({
    Y<- ModelData$ER_INDICATOR
    assign(paste("model_VER_",t,sep=""),SuperLearner(Y,ModelDataX,family=binomial(),SL.library = SLvector))
    saveRDS(get(paste("model_VER_",t,sep="")),file=paste("L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Models/model_VER_",t,".rda",sep=""))
  }, error = function(e){cat("Iteration ",t,"ERROR: ",conditionMessage(e),"\n")})
  
  ##HER model
  tryCatch({
    Y<- ModelData$HERVERZEKERING_ER
    assign(paste("model_HER_",t,sep=""),SuperLearner(Y,ModelDataX,family=binomial(),SL.library = SLvector))
    saveRDS(get(paste("model_HER_",t,sep="")),file=paste("L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Models/model_HER_",t,".rda",sep=""))
  }, error = function(e){cat("Iteration ",t,"ERROR: ",conditionMessage(e),"\n")})
  
  ##GBER model
  tryCatch({
    Y<- ModelData$ER_REGELING
    assign(paste("model_GBER_",t,sep=""),SuperLearner(Y,ModelDataX,family=binomial(),SL.library = SLvector))
    saveRDS(get(paste("model_GBER_",t,sep="")),file=paste("L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Models/model_GBER_",t,".rda",sep=""))
  }, error = function(e){cat("Iteration ",t,"ERROR: ",conditionMessage(e),"\n")})
  
  ##Delete models from workspace
  rm(list = setdiff(ls(pattern = "^model_"),lsf.str()))
  rm(list = c("ModelData","ModelDataX","Y"))
}