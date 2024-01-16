CalculatePValues <- function(name, title){
dataNC <- readRDS(paste0(DataLocation,"/NaturalCourse_",name,".rda"))
dataNP <- readRDS(paste0(DataLocation,"/NoPayments_",name,".rda"))

sNC <- sum((dataNC$Hazards[1,] -mean(dataNC$Hazards[1,]))^2) / (length(dataNC$Hazards[1,]) - 1)
sNP <- sum((dataNP$Hazards[1,] -mean(dataNP$Hazards[1,]))^2) / (length(dataNP$Hazards[1,]) - 1)
nNC <- length(dataNC$Hazards[1,])
nNP <- length(dataNP$Hazards[1,])
sDelta <- sqrt(sNC / nNC + sNP / nNP)
df <- (sNC / nNC + sNP / nNP)^2 / ((sNC / nNC)^2 / (nNC - 1) + (sNP / nNP)^2/(nNP - 1))
t <- (dataNC$Information[1] - dataNP$Information[1]) / sDelta
cat(paste0("Estimation for ", title, ":\n"))
cat(paste0(c("Estimate Natural Course: ","2.5%-lowerbound: ","97.5%-upperbound: "),round(dataNC$Information * 100,1),"% \n"))
cat(paste0(c("Estimate No Payments: ","2.5%-lowerbound: ","97.5%-upperbound: "),round(dataNP$Information * 100,1),"% \n"))
cat(paste0("p-value difference: ",p_value <- 2 * pt(t, df = df,lower.tail = (t < 0)),"\n\n"))
}

CalculatePValues("glm_Bootstrap_combined","full dataset and combined outcome") #Calculate p-value of Naturalcourse and No payment for Combined outcome
CalculatePValues("glm_Bootstrap","full dataset and CVE outcome") #Calculate p-value of Naturalcourse and No payment for CVE
CalculatePValues("glm_Bootstrap_death","full dataset and death") #Calculate p-value of Naturalcourse and No payment for death

##Sensitivity Analysis on statin dataset
CalculatePValues("glm_Bootstrap_Combined_StatinOnly","statin dataset and combined outcome") #Calculate p-value of Naturalcourse and No payment for Combined outcome
CalculatePValues("glm_Bootstrap_CVE_StatinOnly","statin dataset and CVE outcome") #Calculate p-value of Naturalcourse and No payment for CVE outcome
CalculatePValues("glm_Bootstrap_Death_StatinOnly","statin dataset and death outcome") #Calculate p-value of Naturalcourse and No payment for Death outcome