ScriptLocation <- "L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Scripts/R/"
PlotLocation <- "L:/afdelingen/Actuariaat_Business/DennisS/PhD/A4/Plots/"
ModelLocation <- "//KA0089.loods2.org/Actuariaat/SAS Data/DennisS/PhD/A4/Models"
DataLocation <- "//KA0089.loods2.org/Actuariaat/SAS Data/DennisS/PhD/A4"

#Calculate p-value of Naturalcourse and Nopayment for CVE
name <- "glm_Bootstrap"
dataNC <- readRDS(paste0(DataLocation,"/NaturalCourse_",name,".rda"))
dataNP <- readRDS(paste0(DataLocation,"/NoPayments_",name,".rda"))

sNC <- sum((dataNC$Hazards[1,] -mean(dataNC$Hazards[1,]))^2) / (length(dataNC$Hazards[1,]) - 1)
sNP <- sum((dataNP$Hazards[1,] -mean(dataNP$Hazards[1,]))^2) / (length(dataNP$Hazards[1,]) - 1)
nNC <- length(dataNC$Hazards[1,])
nNP <- length(dataNP$Hazards[1,])
sDelta <- sqrt(sNC / nNC + sNP / nNP)
df <- (sNC / nNC + sNP / nNP)^2 / ((sNC / nNC)^2 / (nNC - 1) + (sNP / nNP)^2/(nNP - 1))
t <- (dataNC$Information[1] - dataNP$Information[1]) / sDelta
(p_value <- 2 * pt(t, df = df,lower.tail = F))