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
fit <- lm(meanN2Oppm ~ meandegC + meanVW + Month, data=pitmodeldf)
summary(fit) # show results
fname = paste(pathsavetab, "regr_table_N2O" ,sep="")
regr_table(fit, fname)

# CO2
fit <- lm(meanCO2ppm ~ meandegC + meanVW + Month, data=pitmodeldf)
summary(fit) # show results
fname = paste(pathsavetab, "regr_table_CO2" ,sep="")
regr_table(fit, fname)

# CH4
fit <- lm(meanCH4ppm ~ meandegC + meanVW + sampledepth, data=pitmodeldf)
summary(fit) # show results
fname = paste(pathsavetab, "regr_table_CH4" ,sep="")
regr_table(fit, fname)


# Multiple Linear Regression Example 
fit <- lm(meanCO2ppm ~ meandegC + meanVW + LUType + Month + sampledepth, data=pitmodeldf)
summary(fit) # show results

# Multiple Linear Regression Example 
fit <- lm(meanCH4ppm ~ meandegC + meanVW + LUType + Month + sampledepth, data=pitmodeldf)
summary(fit) # show results



## model selection stuff

# complete cases
# pitmodeldf <- pitmodeldf[complete.cases(pitmodeldf),]

# model selection
model.current <- lm(meanN2Oppm ~ 1, data=pitmodeldf)
summary(model.current) # show results
add1(model.current, ~ pitmodeldf$meandegC + pitmodeldf$meanVW + pitmodeldf$sampledepth)
#drop1(fit)





########################################################################
# NOTES AND TESTING




