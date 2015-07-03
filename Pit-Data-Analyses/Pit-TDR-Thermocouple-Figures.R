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

# put YearMonthOrder in as variable
pitTDRsummary <- transform(pitTDRsummary, YearMonthOrder = paste(substr(pitTDRsummary$YearMonth, 6, 8), substr(pitTDRsummary$YearMonth, 1, 4), sep="-"))
# put YearMonthOrder in as variable
pitTDRsummary$YearMonthOrder <- factor(pitTDRsummary$YearMonthOrder, levels = c("11-2013", "12-2013", "1-2015", "2-2014"))
# add forest vs. agriculture label
pitTDRsummary$ForAgri <- "Forest"
pitTDRsummary$ForAgri[grepl("Soya", pitTDRsummary$LUType)] <- "Agriculture"

# get rid of VW for C2, Jan 2015, since it looks like the sensors are on the fritz
pitTDRsummary <- pitTDRsummary[!(pitTDRsummary$PitID=="C2" & pitTDRsummary$Month=="Jan" & pitTDRsummary$DataType=="VW"),]

# get rid of weird third depth in C2 for temperature?  Why are these temps 18 degC, even across months?


########################################################################
# SIMPLE SCATTERPLOTS OVER DEPTH

# temp and VW by pit ID
p1 <- ggplot(pitTDRsummary[pitTDRsummary$DataType=="degC",], aes(x=sampledepth, y=measurement)) + geom_point(shape=1) + geom_line(aes(color=YearMonthOrder)) + coord_flip() + scale_x_reverse() + facet_grid(PitID ~ .) + xlab("Sample Depth (cm)") + ylab("Temperature (C)") + geom_errorbar(aes(ymin=measurement-se, ymax=measurement+se), width=5) + scale_colour_discrete(name="Sampling Period") + theme_bw()

p2 <- ggplot(pitTDRsummary[pitTDRsummary$DataType=="VW",], aes(x=sampledepth, y=measurement)) + geom_point(shape=1) + geom_line(aes(color=YearMonthOrder)) + coord_flip() + scale_x_reverse() + facet_grid(PitID ~ .) + xlab("Sample Depth (cm)") + ylab(expression(Volumetric~Water~Content~"("*cm^"3"~cm^"-3"*")")) + geom_errorbar(aes(ymin=measurement-se, ymax=measurement+se), width=5) + scale_colour_discrete(name="Sampling Period") + theme_bw()

# temp and VW by land use type
p3 <- ggplot(pitTDRsummary[pitTDRsummary$DataType=="degC",], aes(x=sampledepth, y=measurement)) + geom_point(shape=1) + geom_line(aes(color=YearMonthOrder, linetype=PitID)) + coord_flip() + scale_x_reverse() + facet_grid(LUType ~ .) + xlab("Sample Depth (cm)") + ylab("Temperature (C)") + geom_errorbar(aes(ymin=measurement-se, ymax=measurement+se), width=5) + scale_colour_discrete(name="Sampling Period") + theme_bw()

p4 <- ggplot(pitTDRsummary[pitTDRsummary$DataType=="VW",], aes(x=sampledepth, y=measurement)) + geom_point(shape=1) + geom_line(aes(color=YearMonthOrder, linetype=PitID)) + coord_flip() + scale_x_reverse() + facet_grid(LUType ~ .) + xlab("Sample Depth (cm)") + ylab(expression(Volumetric~Water~Content~"("*cm^"3"~cm^"-3"*")")) + geom_errorbar(aes(ymin=measurement-se, ymax=measurement+se), width=5) + scale_colour_discrete(name="Sampling Period") + theme_bw()

# temp and VW by forest vs. agriculture
p5 <- ggplot(pitTDRsummary[pitTDRsummary$DataType=="degC",], aes(x=sampledepth, y=measurement)) + geom_point(shape=1) + geom_line(aes(color=YearMonthOrder, linetype=PitID)) + coord_flip() + scale_x_reverse() + facet_grid(ForAgri ~ .) + xlab("Sample Depth (cm)") + ylab("Temperature (C)") + geom_errorbar(aes(ymin=measurement-se, ymax=measurement+se), width=5) + scale_colour_discrete(name="Sampling Period") + theme_bw()

p6 <- ggplot(pitTDRsummary[pitTDRsummary$DataType=="VW",], aes(x=sampledepth, y=measurement)) + geom_point(shape=1) + geom_line(aes(color=YearMonthOrder, linetype=PitID)) + coord_flip() + scale_x_reverse() + facet_grid(ForAgri ~ .) + xlab("Sample Depth (cm)") + ylab(expression(Volumetric~Water~Content~"("*cm^"3"~cm^"-3"*")")) + geom_errorbar(aes(ymin=measurement-se, ymax=measurement+se), width=5) + scale_colour_discrete(name="Sampling Period") + theme_bw()


########################################################################
# SAVE SCATTERPLOTS OVER DEPTH

# save graphs, by pit ID
png(file = paste(pathsavefigs, "soilpit-temperature.png", sep=""),width=6,height=6,units="in",res=400)
p1 + labs(title = "Soil Temperature, Tanguro Soil Pits") 
dev.off()

png(file = paste(pathsavefigs, "soilpit-VW.png", sep=""),width=6,height=6,units="in",res=400)
p2 + labs(title = "VW, Tanguro Soil Pits") 
dev.off()

# save graphs, by ForAgri
png(file = paste(pathsavefigs, "soilpit-temperature-ForAgri.png", sep=""),width=6,height=6,units="in",res=400)
p5 + labs(title = "Soil Temperature, Tanguro Soil Pits") 
dev.off()

png(file = paste(pathsavefigs, "soilpit-VW-ForAgri.png", sep=""),width=6,height=6,units="in",res=400)
p6 + labs(title = "VW, Tanguro Soil Pits") 
dev.off()



########################################################################
# NOTES AND TESTING


#### get temp and VW data from APP1 and Area3 pit for Jan 2015!



# get rid of weird third depth in C2?  Why are these temps 18 degC, even across months?

# add in seasonality as a variable?  make it the symbol shape or something?
# seems pointless since all 4 months are wet season
# seasonality variable
#pitTDRsummary <- transform(pitTDRsummary, Season = ifelse(Month=="Jan", as.character("Wet"), ifelse(Month=="Feb", as.character("Wet"), ifelse(Month=="Nov", as.character("Wet"), as.character("Soya SC")))))




# code if I need this:

# create a col to assign a better name to each land use
# pitgasfull <- transform(pitgasfull, LUname = ifelse(LUtype=="M", as.character("Soya/Maize DC"), ifelse(LUtype=="F", as.character("Forest"), as.character("Soya SC"))))




