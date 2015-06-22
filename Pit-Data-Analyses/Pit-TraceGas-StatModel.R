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
# MODEL PLAYTIME

pitmodeldf$LUType <- factor(pitmodeldf$LUType)

# Multiple Linear Regression Example 
fit <- lm(meanN2Oppm ~ meandegC + meanVW + LUType + PitID + Month + sampledepth, data=pitmodeldf)
summary(fit) # show results

# Multiple Linear Regression Example 
fit <- lm(meanCO2ppm ~ meandegC + meanVW + LUType + Month + sampledepth, data=pitmodeldf)
summary(fit) # show results

# Multiple Linear Regression Example 
fit <- lm(meanCH4ppm ~ meandegC + meanVW + LUType + Month + sampledepth, data=pitmodeldf)
summary(fit) # show results




# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Rprocessed/"
pathsavefigs = "~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Analyses/PitGasFigures/"

# create a col to assign a color in the graphs
pitgasfull <- transform(pitgasfull, color.use = ifelse(LUtype=="M", as.character("darkorange"), ifelse(LUtype=="F", as.character("darkgreen"), as.character("darkblue"))))

# create a col to assign a better name to each land use
pitgasfull <- transform(pitgasfull, LUname = ifelse(LUtype=="M", as.character("Soya/Maize DC"), ifelse(LUtype=="F", as.character("Forest"), as.character("Soya SC"))))


########################################################################
# THROW OUT ONE WEIRD DATA POINT

# get rid of that one weird rerun vial that is way off from the other two
rerunid <- grep("F1-K4-250cm-C_rerun", pitgasfull$SampleName)
pitgasfull$ngN_cm3_N2O[rerunid] <- NA
pitgasfull$ngC_cm3_CO2[rerunid] <- NA
pitgasfull$ngC_cm3_CH4[rerunid] <- NA
pitgasfull$N2Oppm[rerunid] <- NA
pitgasfull$CO2ppm[rerunid] <- NA
pitgasfull$CH4ppm[rerunid] <- NA

# throw out vials that were drawn first in february
vialstodrop <- c("A","B") # c("A")
tmp <- grepl(paste(vialstodrop,collapse="|"),pitgasfull$SampleName) & grepl("Feb",pitgasfull$Month)
pitgasfull$ngN_cm3_N2O[tmp] <- NA
pitgasfull$ngC_cm3_CO2[tmp] <- NA
pitgasfull$ngC_cm3_CH4[tmp] <- NA
pitgasfull$N2Oppm[tmp] <- NA
pitgasfull$CO2ppm[tmp] <- NA
pitgasfull$CH4ppm[tmp] <- NA


########################################################################
# DESCRIPTIVE STATS SUMMARY

# Run the functions length, mean, and sd on the value of "change" for each group, 
# broken down by sex + condition

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# summarySE
summarytab1 <- summarySE(data=pitgasfull, measurevar="N2Oppm", c("pitID", "sampledepth", "SampleDate", "LUtype", "LUname", "color.use", "Month"), na.rm=TRUE, renameallcols=TRUE)
summarytab2 <- summarySE(data=pitgasfull, measurevar="CO2ppm", c("pitID", "sampledepth", "SampleDate"), na.rm=TRUE, renameallcols=TRUE)
summarytab3 <- summarySE(data=pitgasfull, measurevar="CH4ppm", c("pitID", "sampledepth", "SampleDate"), na.rm=TRUE, renameallcols=TRUE)

# join
pitgassummary <- join(x = summarytab1, y = summarytab2, by = c("pitID", "sampledepth", "SampleDate", "N"))
pitgassummary <- join(x = pitgassummary, y = summarytab3, by = c("pitID", "sampledepth", "SampleDate", "N"))


########################################################################
# SIMPLE SCATTERPLOTS OVER DEPTH

p1 <- ggplot(pitgassummary, aes(x=sampledepth, y=meanN2Oppm)) + geom_point(shape=1) + geom_line(aes(color=Month)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("Sample Depth (cm)") + geom_errorbar(aes(ymin=meanN2Oppm-seN2Oppm, ymax=meanN2Oppm+seN2Oppm), width=5)

p2 <- ggplot(pitgassummary, aes(x=sampledepth, y=meanCO2ppm)) + geom_point(shape=1) + geom_line(aes(color=Month)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("Sample Depth (cm)") + geom_errorbar(aes(ymin=meanCO2ppm-seCO2ppm, ymax=meanCO2ppm+seCO2ppm), width=5)

p3 <- ggplot(pitgassummary, aes(x=sampledepth, y=meanCH4ppm)) + geom_point(shape=1) + geom_line(aes(color=Month)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("Sample Depth (cm)") + geom_errorbar(aes(ymin=meanCH4ppm-seCH4ppm, ymax=meanCH4ppm+seCH4ppm), width=5)

# individ gas graphs
png(file = paste(pathsavefigs, "soilpit-tracegasconcentrations-N2O.png", sep=""),width=6,height=6,units="in",res=400)
p1 + labs(title = "Trace Gas Concentration, Tanguro Soil Pits") 
dev.off()

png(file = paste(pathsavefigs, "soilpit-tracegasconcentrations-CO2.png", sep=""),width=6,height=6,units="in",res=400)
p2 + labs(title = "Trace Gas Concentration, Tanguro Soil Pits") 
dev.off()

png(file = paste(pathsavefigs, "soilpit-tracegasconcentrations-CH4.png", sep=""),width=6,height=6,units="in",res=400)
p3 + labs(title = "Trace Gas Concentration, Tanguro Soil Pits") 
dev.off()

# grid.arrange
png(file = paste(pathsavefigs, "soilpit-tracegasconcentrations.png", sep=""),width=12,height=6,units="in",res=400)
grid.arrange(p1, p2, p3, nrow = 1, ncol = 3, main="Trace Gas Concentration, Tanguro Soil Pits")
dev.off()


########################################################################
# VIAL SAMPLING ORDER COMPARISON

p1 <- ggplot(pitgasfull, aes(x=sampledepth, y=N2Oppm)) + geom_point(shape=1, aes(color=sampleorder)) + facet_grid(Month ~ pitID) + geom_line(aes(color=sampleorder)) + xlab("Sample Depth (cm)") + scale_colour_brewer(palette = "Dark2", labels=c("A (Vial 1)", "B (Vial 2)", "C (Vial 3)"))

p2 <- ggplot(pitgasfull, aes(x=sampledepth, y=CO2ppm)) + geom_point(shape=1, aes(color=sampleorder)) + facet_grid(Month ~ pitID) + geom_line(aes(color=sampleorder)) + xlab("Sample Depth (cm)") + scale_colour_brewer(palette = "Dark2", labels=c("A (Vial 1)", "B (Vial 2)", "C (Vial 3)"))

p3 <- ggplot(pitgasfull, aes(x=sampledepth, y=CH4ppm)) + geom_point(shape=1, aes(color=sampleorder)) + facet_grid(Month ~ pitID) + geom_line(aes(color=sampleorder)) + xlab("Sample Depth (cm)") + scale_colour_brewer(palette = "Dark2", labels=c("A (Vial 1)", "B (Vial 2)", "C (Vial 3)"))

# individ gas graphs
png(file = paste(pathsavefigs, "soilpit-sampleorder-N2O.png", sep=""),width=10,height=6,units="in",res=400)
p1 + labs(title = "Sampling Order for Trace Gas Vials, Tanguro Soil Pits")
dev.off()

png(file = paste(pathsavefigs, "soilpit-sampleorder-CO2.png", sep=""),width=10,height=6,units="in",res=400)
p2 + labs(title = "Sampling Order for Trace Gas Vials, Tanguro Soil Pits")
dev.off()

png(file = paste(pathsavefigs, "soilpit-sampleorder-CH4.png", sep=""),width=10,height=6,units="in",res=400)
p3 + labs(title = "Sampling Order for Trace Gas Vials, Tanguro Soil Pits")
dev.off()

# grid.arrange
png(file = paste(pathsavefigs, "soilpit-sampleorder.png", sep=""),width=8,height=16,units="in",res=400)
grid.arrange(p1, p2, p3, nrow = 3, ncol = 1, main="Sampling Order for Trace Gas Vials, Tanguro Soil Pits")
dev.off()



########################################################################
# SAVE CSV

# save summary as csv
write.csv(pitgassummary, file=paste(pathsavefiles, "pitgassummary.csv", sep = ""), row.names=FALSE)  





########################################################################
# NOTES AND TESTING









# as pdfs


########################################################################
# SIMPLE SCATTERPLOTS OVER DEPTH

p1 <- ggplot(pitgassummary, aes(x=sampledepth, y=meanN2Oppm)) + geom_point(shape=1) + geom_line(aes(color=Month)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("Sample Depth (cm)") + geom_errorbar(aes(ymin=meanN2Oppm-seN2Oppm, ymax=meanN2Oppm+seN2Oppm), width=5)

p2 <- ggplot(pitgassummary, aes(x=sampledepth, y=meanCO2ppm)) + geom_point(shape=1) + geom_line(aes(color=Month)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("Sample Depth (cm)") + geom_errorbar(aes(ymin=meanCO2ppm-seCO2ppm, ymax=meanCO2ppm+seCO2ppm), width=5)

p3 <- ggplot(pitgassummary, aes(x=sampledepth, y=meanCH4ppm)) + geom_point(shape=1) + geom_line(aes(color=Month)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("Sample Depth (cm)") + geom_errorbar(aes(ymin=meanCH4ppm-seCH4ppm, ymax=meanCH4ppm+seCH4ppm), width=5)

# individ gas graphs
pdf(file = paste(pathsavefigs, "soilpit-tracegasconcentrations-N2O.pdf", sep=""),width=6,height=6)
p1 + labs(title = "Trace Gas Concentration, Tanguro Soil Pits") 
dev.off()

pdf(file = paste(pathsavefigs, "soilpit-tracegasconcentrations-CO2.pdf", sep=""),width=6,height=6)
p2 + labs(title = "Trace Gas Concentration, Tanguro Soil Pits") 
dev.off()

pdf(file = paste(pathsavefigs, "soilpit-tracegasconcentrations-CH4.pdf", sep=""),width=6,height=6)
p3 + labs(title = "Trace Gas Concentration, Tanguro Soil Pits") 
dev.off()

# grid.arrange
pdf(file = paste(pathsavefigs, "soilpit-tracegasconcentrations.pdf", sep=""),width=12,height=6)
grid.arrange(p1, p2, p3, nrow = 1, ncol = 3, main="Trace Gas Concentration, Tanguro Soil Pits")
dev.off()



########################################################################
# VIAL SAMPLING ORDER COMPARISON

p1 <- ggplot(pitgasfull, aes(x=sampledepth, y=N2Oppm)) + geom_point(shape=1, aes(color=sampleorder)) + facet_grid(Month ~ pitID) + geom_line(aes(color=sampleorder)) + xlab("Sample Depth (cm)") + scale_colour_brewer(palette = "Dark2", labels=c("A (Vial 1)", "B (Vial 2)", "C (Vial 3)"))

p2 <- ggplot(pitgasfull, aes(x=sampledepth, y=CO2ppm)) + geom_point(shape=1, aes(color=sampleorder)) + facet_grid(Month ~ pitID) + geom_line(aes(color=sampleorder)) + xlab("Sample Depth (cm)") + scale_colour_brewer(palette = "Dark2", labels=c("A (Vial 1)", "B (Vial 2)", "C (Vial 3)"))

p3 <- ggplot(pitgasfull, aes(x=sampledepth, y=CH4ppm)) + geom_point(shape=1, aes(color=sampleorder)) + facet_grid(Month ~ pitID) + geom_line(aes(color=sampleorder)) + xlab("Sample Depth (cm)") + scale_colour_brewer(palette = "Dark2", labels=c("A (Vial 1)", "B (Vial 2)", "C (Vial 3)"))

# individ gas graphs
pdf(file = paste(pathsavefigs, "soilpit-sampleorder-N2O.pdf", sep=""),width=10,height=6)
p1 + labs(title = "Sampling Order for Trace Gas Vials, Tanguro Soil Pits")
dev.off()

pdf(file = paste(pathsavefigs, "soilpit-sampleorder-CO2.pdf", sep=""),width=10,height=6)
p2 + labs(title = "Sampling Order for Trace Gas Vials, Tanguro Soil Pits")
dev.off()

pdf(file = paste(pathsavefigs, "soilpit-sampleorder-CH4.pdf", sep=""),width=10,height=6)
p3 + labs(title = "Sampling Order for Trace Gas Vials, Tanguro Soil Pits")
dev.off()

# grid.arrange
pdf(file = paste(pathsavefigs, "soilpit-sampleorder.pdf", sep=""),width=8,height=16)
grid.arrange(p1, p2, p3, nrow = 3, ncol = 1, main="Sampling Order for Trace Gas Vials, Tanguro Soil Pits")
dev.off()


