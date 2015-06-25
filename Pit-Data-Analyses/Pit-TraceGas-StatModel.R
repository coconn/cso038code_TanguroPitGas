# Pit-TraceGas-StatModel.R
# taking trace gas vial data along with TDR and temp data and making a simple model
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# requires files created in:
# PitCalcs-TraceGas-Rcode.R
# PitCalcs-Thermocouple-Rcode.R


########################################################################
# BRING IN DATA / PREP

library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(data.table)
library(lubridate)
library(reshape2)
library(tidyr)
library(magrittr)

# where to save outputs
pathsavetab = "~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Analyses/PitTables/"
pathsavefigs = "~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Analyses/PitGasFigures/"

# regr_table
source("~/Documents/GITHUB/RPersonalFunctionsChristine/regr_table.r")

# bring in stuff
pitTDRsummary <- read.csv("~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Rprocessed/pitTDRsummarytable.csv", stringsAsFactors=FALSE)

pitgassummary <- read.csv("~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Rprocessed/pitgassummary.csv", stringsAsFactors=FALSE)

# get rid of rows with NA in sample depth
pitTDRsummary <- subset(pitTDRsummary, !is.na(pitTDRsummary$sampledepth))


########################################################################
# WIDEN pitTDRsummary SO IT MATCHES pitgassummary

# mean
keep1 <- c("PitID", "YearMonth", "DataType", "measurement", "sampledepth")
tmp <- subset(pitTDRsummary, select = keep1)
tmp2 <- tmp %>% spread(DataType, measurement)
colnames(tmp2)[4] <- "meandegC"; colnames(tmp2)[5] <- "meanVW"

# sd
keep1 <- c("PitID", "YearMonth", "DataType", "sd", "sampledepth")
tmp3 <- subset(pitTDRsummary, select = keep1)
tmp4 <- tmp3 %>% spread(DataType, sd)
colnames(tmp4)[4] <- "sddegC"; colnames(tmp4)[5] <- "sdVW"

# se
keep1 <- c("PitID", "YearMonth", "DataType", "se", "sampledepth")
tmp5 <- subset(pitTDRsummary, select = keep1)
tmp6 <- tmp5 %>% spread(DataType, se)
colnames(tmp6)[4] <- "sedegC"; colnames(tmp6)[5] <- "seVW"

# join
pitTDRsummary2 <- join(x = tmp2, y = tmp4, by = c("PitID", "YearMonth", "sampledepth"))
pitTDRsummary2 <- join(x = pitTDRsummary2, y = tmp6, by = c("PitID", "YearMonth", "sampledepth"))

# name compliance
pitTDRsummary2$PitID[grepl("Mutum", pitTDRsummary2$PitID)] <- "MU"


########################################################################
# GET READY TO MERGE, CREATE MODEL DATAFRAME

# get cols ready in pitgassummary

# year-month combo variable
pitgassummary <- transform(pitgassummary, YearMonth = paste(year(pitgassummary$SampleDate),month(pitgassummary$SampleDate),sep="-"))

# need: PitID YearMonth sampledepth
keep <- c(which(colnames(pitgassummary)=="pitID"),which(colnames(pitgassummary)=="YearMonth"),which(colnames(pitgassummary)=="sampledepth"))
var1 <- which(colnames(pitgassummary)=="meanN2Oppm")
varend <- which(colnames(pitgassummary)=="ciCH4ppm")
pitgassummary <- pitgassummary[,c(keep,var1:varend)]

# names
colnames(pitgassummary)[1] <- "PitID"

# merge
pitmodeldf <- merge(x = pitTDRsummary2, y = pitgassummary, by = c("PitID", "YearMonth", "sampledepth"), all=T)

# get rid of rows with NA in N2O mean emissions
pitmodeldf <- subset(pitmodeldf, !is.na(pitmodeldf$meanN2Oppm))

# add back Year and Month as vars
pitmodeldf$Year <- substr(pitmodeldf$YearMonth, 1, 4)
pitmodeldf$Month <- month(as.numeric(substr(pitmodeldf$YearMonth, 6, nchar(pitmodeldf$YearMonth))), label = TRUE)

# add forest label
pitmodeldf$LUType <- "Forest"
pitmodeldf$LUType[grepl("MU", pitmodeldf$PitID)] <- "Agriculture"



########################################################################
# TEMPERATURE MODEL

# temp subset
tempsubset <- subset(pitTDRsummary, pitTDRsummary$DataType=="degC")

# model
fit <- lm(measurement ~ PitID + Month + sampledepth, data=tempsubset)
summary(fit) # show results

# regr_table
fname = paste(pathsavetab, "regr_table_temp" ,sep="")
regr_table(fit, fname)



########################################################################
# VWC MODEL

# VW subset
vwsubset <- subset(pitTDRsummary, pitTDRsummary$DataType=="VW")

# model
fit <- lm(measurement ~ PitID + Month + sampledepth, data=vwsubset)
summary(fit) # show results

# regr_table
fname = paste(pathsavetab, "regr_table_VWC" ,sep="")
regr_table(fit, fname)




########################################################################
# GAS MODELS

# these need lots of work - pretty arbitrary right now

# LU as factor
pitmodeldf$LUType <- factor(pitmodeldf$LUType)

# N2O
fit <- lm(meanN2Oppm ~ meandegC + meanVW + Month + sampledepth, data=pitmodeldf)
summary(fit) # show results
fname = paste(pathsavetab, "regr_table_N2O" ,sep="")
regr_table(fit, fname)

# CO2
fit <- lm(meanCO2ppm ~ meandegC + meanVW + Month, data=pitmodeldf)
summary(fit) # show results
fname = paste(pathsavetab, "regr_table_CO2" ,sep="")
regr_table(fit, fname)

# CH4
fit <- lm(log(meanCH4ppm) ~ meandegC + meanVW + Month + sampledepth, data=pitmodeldf)
summary(fit) # show results
fname = paste(pathsavetab, "regr_table_CH4" ,sep="")
regr_table(fit, fname)



########################################################################
# ANOVAS WITH LUTYPE
# diagnostic plots courtesy http://www.statmethods.net/stats/anova.html

## Two Way Factorial Design 

# N2O
fit <- aov(log(meanN2Oppm) ~ LUType*sampledepth, data=pitmodeldf)
summary(fit) # show results
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots

# CO2
fit <- aov(log(meanCO2ppm) ~ LUType*sampledepth, data=pitmodeldf)
summary(fit) # show results
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots

# CH4
fit <- aov(log(meanCH4ppm) ~ LUType*sampledepth, data=pitmodeldf)
summary(fit) # show results
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots


# One Way Anova

# N2O
fit <- aov(log(meanN2Oppm) ~ LUType, data=pitmodeldf)
summary(fit) # show results
# diagnostics
png(file = paste(pathsavetab, "ANOVAdiagnostics/ANOVAdiagnostics-N2O.png", sep=""),width=6,height=6,units="in",res=400)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots
title("N2O ANOVA Diagnostics", line = -2, outer = TRUE)
dev.off()
# tukey hst
TukeyHSD(fit)
print(model.tables(fit,"means"),digits=3)       #report the means and the number of subjects/cell

# CO2
fit <- aov(log(meanCO2ppm) ~ LUType, data=pitmodeldf)
summary(fit) # show results
# diagnostics
png(file = paste(pathsavetab, "ANOVAdiagnostics/ANOVAdiagnostics-CO2.png", sep=""),width=6,height=6,units="in",res=400)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots
title("CO2 ANOVA Diagnostics", line = -2, outer = TRUE)
dev.off()
# tukey hst
TukeyHSD(fit)
print(model.tables(fit,"means"),digits=3)       #report the means and the number of subjects/cell

# CH4
fit <- aov(log(meanCH4ppm) ~ LUType, data=pitmodeldf)
summary(fit) # show results
# diagnostics
png(file = paste(pathsavetab, "ANOVAdiagnostics/ANOVAdiagnostics-CH4.png", sep=""),width=6,height=6,units="in",res=400)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots
title("CH4 ANOVA Diagnostics", line = -2, outer = TRUE)
dev.off()
# tukey hsd
TukeyHSD(fit)
print(model.tables(fit,"means"),digits=3)       #report the means and the number of subjects/cell



########################################################################
# BOXPLOTS

# only land use
png(file = paste(pathsavefigs, "soilpit-gas-anovaboxplot-nodepth.png", sep=""),width=6,height=4,units="in",res=400)
layout(matrix(c(1,2,3),1,3)) # optional layout
# N2O
boxplot(meanN2Oppm ~ LUType,data=pitmodeldf, ylab="N2O (ppm)")  
# CO2
boxplot(meanCO2ppm ~ LUType,data=pitmodeldf, ylab="CO2 (ppm)")  
text(x=1, y=14600, "p < 0.001", pos=3, cex=0.9)
text(x=1, y=13200, "***", pos=3, cex=1.4)
# CH4
boxplot(meanCH4ppm ~ LUType,data=pitmodeldf, ylab="CH4 (ppm)")  
dev.off()

# depth and land use

# for all box plots
atvec = c(1,2, 4,5, 7,8, 10,11, 13,14, 16,17, 19,20) # how to group boxes
colorvec = c("light grey","white")
namesvec = c("Ag 15","For 15","Ag 40","For 40","Ag 75","For 75","Ag 150","For 150","Ag 250","For 250","Ag 350","For 350","Ag 450","For 450")

png(file = paste(pathsavefigs, "soilpit-gas-anovaboxplot-depthgroup.png", sep=""),width=12,height=4,units="in",res=400)
layout(matrix(c(1,2,3),1,3)) # optional layout
# N2O
boxplot(meanN2Oppm ~ LUType + sampledepth,data=pitmodeldf, ylab="N2O (ppm)", col=colorvec,  las = 2, at = atvec, names=namesvec)  
legend("topright", c("Agriculture", "Forest"), fill=colorvec)
# CO2
boxplot(meanCO2ppm ~ LUType + sampledepth,data=pitmodeldf, ylab="CO2 (ppm)", col=colorvec,  las = 2, at = atvec, names=namesvec)  
legend("topright", c("Agriculture", "Forest"), fill=colorvec)
# CH4
boxplot(meanCH4ppm ~ LUType + sampledepth,data=pitmodeldf, ylab="CH4 (ppm)", col=colorvec,  las = 2, at = atvec, names=namesvec)  
legend("topright", c("Agriculture", "Forest"), fill=colorvec)
dev.off()


########################################################################
# NOTES AND TESTING




