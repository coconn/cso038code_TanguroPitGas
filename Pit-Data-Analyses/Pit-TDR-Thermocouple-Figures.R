# Pit-TDR-Thermocouple-Figures.R
# taking temperature data associated with soil pits and making some figures
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# requires files created in PitCalcs-Thermocouple-Rcode.R


########################################################################
# BRING IN DATA / PREP

pitTDRsummary <- read.csv("~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Rprocessed/pitTDRsummarytable.csv", stringsAsFactors=FALSE)

library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(data.table)
library(lubridate)

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Rprocessed/"
pathsavefigs = "~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Analyses/PitTDRThermocoupleFigures/"


########################################################################
# DATA PREP

# get rid of rows with NA in sample depth
pitTDRsummary <- subset(pitTDRsummary, !is.na(pitTDRsummary$sampledepth))

# get rid of weird third depth in C2?  Why are these temps 18 degC, even across months?


########################################################################
# SIMPLE SCATTERPLOTS OVER DEPTH

p1 <- ggplot(pitTDRsummary[pitTDRsummary$DataType=="degC",], aes(x=sampledepth, y=measurement)) + geom_point(shape=1) + geom_line(aes(color=YearMonth)) + coord_flip() + scale_x_reverse() + facet_grid(PitID ~ .) + xlab("Sample Depth (cm)") + ylab("Temperature (C)") + geom_errorbar(aes(ymin=measurement-se, ymax=measurement+se), width=5)

p2 <- ggplot(pitTDRsummary[pitTDRsummary$DataType=="VW",], aes(x=sampledepth, y=measurement)) + geom_point(shape=1) + geom_line(aes(color=YearMonth)) + coord_flip() + scale_x_reverse() + facet_grid(PitID ~ .) + xlab("Sample Depth (cm)") + ylab("Volumetric Water Content (fraction)") + geom_errorbar(aes(ymin=measurement-se, ymax=measurement+se), width=5)

# save graphs
png(file = paste(pathsavefigs, "soilpit-temperature.png", sep=""),width=6,height=6,units="in",res=400)
p1 + labs(title = "Soil Temperature, Tanguro Soil Pits") 
dev.off()

png(file = paste(pathsavefigs, "soilpit-VW.png", sep=""),width=6,height=6,units="in",res=400)
p2 + labs(title = "VW, Tanguro Soil Pits") 
dev.off()


########################################################################
# NOTES AND TESTING

# get rid of weird third depth in C2?  Why are these temps 18 degC, even across months?

# add in seasonality as a variable?  make it the symbol shape or something?
# seems pointless since all 4 months are wet season
# seasonality variable
#pitTDRsummary <- transform(pitTDRsummary, Season = ifelse(Month=="Jan", as.character("Wet"), ifelse(Month=="Feb", as.character("Wet"), ifelse(Month=="Nov", as.character("Wet"), as.character("Soya SC")))))




# code if I need this:

# create a col to assign a better name to each land use
# pitgasfull <- transform(pitgasfull, LUname = ifelse(LUtype=="M", as.character("Soya/Maize DC"), ifelse(LUtype=="F", as.character("Forest"), as.character("Soya SC"))))




