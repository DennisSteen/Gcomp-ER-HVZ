.libPaths(c(.libPaths(),"L:/afdelingen/Actuariaat_Business/Centraal Archief/06 Informatievoorziening en infrastructuur/R packages"))

library(DBI)
library(odbc)
library(dplyr)
library(survival)
library(ggplot2)
library(ggsurvfit)
library(pammtools)

con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};Server=ka0327.loods2.org;Trusted_Connection=True;Database=SBX210", timeout = 10)
ScriptLocation <- "L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Scripts/R/"
PlotLocation <- "L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Plots/"
ModelLocation <- "//KA0089.loods2.org/Actuariaat/SAS Data/DennisS/PhD/A4/Models"
DataLocation <- "//KA0089.loods2.org/Actuariaat/SAS Data/DennisS/PhD/A4"

CompleteDataset <- tbl(con, "DS_PhD_CompleteSetA4_90")

KaplanMeier <- as.data.frame(CompleteDataset %>% 
  filter(dag90 < 3000) %>%
  group_by(ZCL_REL_NR) %>%
  summarise_at(vars(CVE,Death,dag90), max) %>%
  mutate(EndPoint = ifelse(CVE == 1,"CVE",ifelse(Death == 1,"Death","Censor")))
)

colors <- c("CVE" =  "red", "Death" = "blue", "Combined" = "green")
fit1 <- survfit2(Surv(dag90, as.factor(EndPoint)) ~ 1, data = KaplanMeier) %>%
  tidy_survfit()
fit2 <- survfit2(Surv(dag90,as.factor(pmax(CVE,Death))) ~ 1, data = KaplanMeier) %>%
  tidy_survfit()

p <- ggplot() +
  theme_bw() +
  scale_x_continuous(name = "Years after indexdate", breaks = seq(0,8*365.25,365.25) , labels = seq(0,8)) +
  scale_y_continuous(name = "Cumulative Incidence", breaks = seq(0,0.3,0.1), labels = paste0(seq(0,30,10),"%") ,limits = c(0,0.3)) +
  theme(legend.position = "bottom"
        ,axis.title = element_text(size = 8)
        ,axis.text  = element_text(size = 5)
        ,legend.text = element_text(size = 8)
        ,legend.margin = margin(0,0,0,0)
        ,legend.box.margin = margin(-10,-10,0,-10)) +
  labs(color = "") +
  geom_step(aes(x = time, y = estimate, color = outcome), data = fit1, linewidth = 0.25) +
  geom_stepribbon(aes(x = time, ymin = conf.low, ymax = conf.high, fill = outcome), data = fit1, alpha = 0.5, show.legend = F) +
  geom_step(aes(x = time, y = estimate, color = "Combined"), data = fit2, linewidth = 0.25) + 
  geom_stepribbon(aes(x = time, ymin = conf.low, ymax = conf.high, fill = "Combined"), data = fit2, alpha = 0.5, show.legend = F) +
  scale_color_manual(values = colors)
print(p)
ggsave(paste0(PlotLocation,"KaplanMeier",".tiff"), plot = p, units = "cm", width = 9, height = 9, dpi = 500) 


plotData <- function(name){
  dataNC <- readRDS(paste0(DataLocation,"/NaturalCourse_",name,".rda"))
  bounds <- apply(dataNC$Hazards,MARGIN=1,FUN = quantile,probs=c(0.025,0.975),na.rm = T)
  data_incl_bounds_NC <- as.data.frame(cbind("days"=as.numeric(rownames(dataNC$Hazards)),"pred"=cumsum(pmax(dataNC$Hazards[,1] - c(dataNC$Hazards[2:dim(dataNC$Hazards)[1],1],0),0)),apply(pmax(t(bounds)- c(dataNC$Hazards[2:dim(dataNC$Hazards)[1],1],0),0),MARGIN=2,FUN=cumsum) ))
  
  dataNP <- readRDS(paste0(DataLocation,"/NoPayments_",name,".rda"))
  bounds <- apply(dataNP$Hazards,MARGIN=1,FUN = quantile,probs=c(0.025,0.975),na.rm = T)
  data_incl_bounds_NP <- as.data.frame(cbind("days"=as.numeric(rownames(dataNP$Hazards)),"pred"=cumsum(pmax(dataNP$Hazards[,1] - c(dataNP$Hazards[2:dim(dataNP$Hazards)[1],1],0),0)),apply(pmax(t(bounds)- c(dataNP$Hazards[2:dim(dataNP$Hazards)[1],1],0),0),MARGIN=2,FUN=cumsum) ))
  
  colors <- c("Natural Course" =  "red", "No Payments" = "blue")
  
  p <- ggplot() +
    theme_bw() +
    geom_step(data = data_incl_bounds_NC, aes(x = days, y=pred, color = "Natural Course"), linewidth = 0.25) +
    geom_stepribbon(data = data_incl_bounds_NC, aes(x = days, ymin = `2.5%`, ymax = `97.5%`), fill= "red", alpha = 0.25) +
    geom_step(data = data_incl_bounds_NP, aes(x = days, y=pred, color = "No Payments"), linewidth = 0.25) +
    geom_stepribbon(data = data_incl_bounds_NP, aes(x = days, ymin = `2.5%`, ymax = `97.5%`), fill= "blue", alpha = 0.1) +
    scale_x_continuous(name = "Years after indexdate", breaks = seq(0,8*365.25,365.25) , labels = seq(0,8)) +
    scale_y_continuous(name = "Cumulative Incidence of CVE", breaks = seq(0,0.3,0.1), labels = paste0(seq(0,30,10),"%")) +
    theme(legend.position = "bottom"
          ,axis.title = element_text(size = 8)
          ,axis.text  = element_text(size = 5)
          ,legend.text = element_text(size = 8)
          ,legend.margin = margin(0,0,0,0)
          ,legend.box.margin = margin(-10,-10,0,-10)) +
    labs(color = "") + 
    scale_color_manual(values = colors) +
    coord_cartesian(ylim = c(0,0.3))

 ggsave(paste0(PlotLocation,name,".tiff"), plot = p, units = "cm", width = 9, height = 9, dpi = 500)
  print(p)
}

plotData("glm_Bootstrap")