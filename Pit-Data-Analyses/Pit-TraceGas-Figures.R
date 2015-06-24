# Pit-TraceGas-Figures.R
# taking trace gas vial data associated with soil pits and making some figures
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# requires files created in GC-Rcode-fileloop.R


########################################################################
# BRING IN DATA / PREP

pitgasfull <- read.csv("~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Rprocessed/pitgasfull.csv", stringsAsFactors=FALSE)

library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(data.table)
library(lubridate)

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

# put YearMonth in as variable
pitgassummary <- transform(pitgassummary, YearMonth = paste(year(pitgassummary$SampleDate),month(pitgassummary$SampleDate),sep="-"))
# put YearMonthOrder in as variable
pitgassummary <- transform(pitgassummary, YearMonthOrder = paste(month(pitgassummary$SampleDate),year(pitgassummary$SampleDate),sep="-"))
pitgassummary$YearMonthOrder <- factor(pitgassummary$YearMonthOrder, levels = c("12-2013", "1-2015", "2-2014"))


########################################################################
# SIMPLE SCATTERPLOTS OVER DEPTH

p1 <- ggplot(pitgassummary, aes(x=sampledepth, y=meanN2Oppm)) + geom_point(shape=1) + geom_line(aes(color=YearMonthOrder)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("Sample Depth (cm)") + ylab("N2O (ppm)") + geom_errorbar(aes(ymin=meanN2Oppm-seN2Oppm, ymax=meanN2Oppm+seN2Oppm), width=5) + scale_colour_discrete(name="Sampling Period")

p2 <- ggplot(pitgassummary, aes(x=sampledepth, y=meanCO2ppm)) + geom_point(shape=1) + geom_line(aes(color=YearMonthOrder)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("Sample Depth (cm)") + ylab("CO2 (ppm)") + geom_errorbar(aes(ymin=meanCO2ppm-seCO2ppm, ymax=meanCO2ppm+seCO2ppm), width=5) + scale_colour_discrete(name="Sampling Period")

p3 <- ggplot(pitgassummary, aes(x=sampledepth, y=meanCH4ppm)) + geom_point(shape=1) + geom_line(aes(color=YearMonthOrder)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("Sample Depth (cm)") + ylab("CH4 (ppm)") + geom_errorbar(aes(ymin=meanCH4ppm-seCH4ppm, ymax=meanCH4ppm+seCH4ppm), width=5) + scale_colour_discrete(name="Sampling Period")

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
# versions with shared legend
source("~/Documents/GITHUB/RPersonalFunctionsChristine/grid_arrange_shared_legend.r")

png(file = paste(pathsavefigs, "soilpit-tracegasconcentrations_legend.png", sep=""),width=4,height=12,units="in",res=400)
grid_arrange_shared_legend(p1, p2, p3, nrow = 1, ncol = 3, main="Trace Gas Concentration, Tanguro Soil Pits")
dev.off()

png(file = paste(pathsavefigs, "soilpit-tracegasconcentrations.png", sep=""),width=16,height=6,units="in",res=400)
grid.arrange(p1, p2, p3, nrow = 1, ncol = 3, main="Trace Gas Concentration, Tanguro Soil Pits")
dev.off()


########################################################################
# VIAL SAMPLING ORDER COMPARISON

# p1 <- ggplot(pitgasfull, aes(x=sampledepth, y=N2Oppm)) + geom_point(shape=1, aes(color=sampleorder)) + facet_grid(Month ~ pitID) + geom_line(aes(color=sampleorder)) + xlab("Sample Depth (cm)") + scale_colour_brewer(palette = "Dark2", labels=c("A (Vial 1)", "B (Vial 2)", "C (Vial 3)", "D (Vial 4)"))
# 
# p2 <- ggplot(pitgasfull, aes(x=sampledepth, y=CO2ppm)) + geom_point(shape=1, aes(color=sampleorder)) + facet_grid(Month ~ pitID) + geom_line(aes(color=sampleorder)) + xlab("Sample Depth (cm)") + scale_colour_brewer(palette = "Dark2", labels=c("A (Vial 1)", "B (Vial 2)", "C (Vial 3)", "D (Vial 4)"))
# 
# p3 <- ggplot(pitgasfull, aes(x=sampledepth, y=CH4ppm)) + geom_point(shape=1, aes(color=sampleorder)) + facet_grid(Month ~ pitID) + geom_line(aes(color=sampleorder)) + xlab("Sample Depth (cm)") + scale_colour_brewer(palette = "Dark2", labels=c("A (Vial 1)", "B (Vial 2)", "C (Vial 3)", "D (Vial 4)"))
# 
# # individ gas graphs
# png(file = paste(pathsavefigs, "Sample Order Figures/soilpit-sampleorder-N2O.png", sep=""),width=10,height=6,units="in",res=400)
# p1 + labs(title = "Sampling Order for Trace Gas Vials, Tanguro Soil Pits")
# dev.off()
# 
# png(file = paste(pathsavefigs, "Sample Order Figures/soilpit-sampleorder-CO2.png", sep=""),width=10,height=6,units="in",res=400)
# p2 + labs(title = "Sampling Order for Trace Gas Vials, Tanguro Soil Pits")
# dev.off()
# 
# png(file = paste(pathsavefigs, "Sample Order Figures/soilpit-sampleorder-CH4.png", sep=""),width=10,height=6,units="in",res=400)
# p3 + labs(title = "Sampling Order for Trace Gas Vials, Tanguro Soil Pits")
# dev.off()
# 
# # grid.arrange
# png(file = paste(pathsavefigs, "Sample Order Figures/soilpit-sampleorder.png", sep=""),width=8,height=16,units="in",res=400)
# grid.arrange(p1, p2, p3, nrow = 3, ncol = 1, main="Sampling Order for Trace Gas Vials, Tanguro Soil Pits")
# dev.off()



########################################################################
# SAVE CSV

# save summary as csv
write.csv(pitgassummary, file=paste(pathsavefiles, "pitgassummary.csv", sep = ""), row.names=FALSE)  





########################################################################
# NOTES AND TESTING









# as pdfs


########################################################################
# SIMPLE SCATTERPLOTS OVER DEPTH
# 
# p1 <- ggplot(pitgassummary, aes(x=sampledepth, y=meanN2Oppm)) + geom_point(shape=1) + geom_line(aes(color=Month)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("Sample Depth (cm)") + geom_errorbar(aes(ymin=meanN2Oppm-seN2Oppm, ymax=meanN2Oppm+seN2Oppm), width=5)
# 
# p2 <- ggplot(pitgassummary, aes(x=sampledepth, y=meanCO2ppm)) + geom_point(shape=1) + geom_line(aes(color=Month)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("Sample Depth (cm)") + geom_errorbar(aes(ymin=meanCO2ppm-seCO2ppm, ymax=meanCO2ppm+seCO2ppm), width=5)
# 
# p3 <- ggplot(pitgassummary, aes(x=sampledepth, y=meanCH4ppm)) + geom_point(shape=1) + geom_line(aes(color=Month)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("Sample Depth (cm)") + geom_errorbar(aes(ymin=meanCH4ppm-seCH4ppm, ymax=meanCH4ppm+seCH4ppm), width=5)
# 
# # individ gas graphs
# pdf(file = paste(pathsavefigs, "soilpit-tracegasconcentrations-N2O.pdf", sep=""),width=6,height=6)
# p1 + labs(title = "Trace Gas Concentration, Tanguro Soil Pits") 
# dev.off()
# 
# pdf(file = paste(pathsavefigs, "soilpit-tracegasconcentrations-CO2.pdf", sep=""),width=6,height=6)
# p2 + labs(title = "Trace Gas Concentration, Tanguro Soil Pits") 
# dev.off()
# 
# pdf(file = paste(pathsavefigs, "soilpit-tracegasconcentrations-CH4.pdf", sep=""),width=6,height=6)
# p3 + labs(title = "Trace Gas Concentration, Tanguro Soil Pits") 
# dev.off()
# 
# # grid.arrange
# pdf(file = paste(pathsavefigs, "soilpit-tracegasconcentrations.pdf", sep=""),width=12,height=6)
# grid.arrange(p1, p2, p3, nrow = 1, ncol = 3, main="Trace Gas Concentration, Tanguro Soil Pits")
# dev.off()
# 
# 
# 
# ########################################################################
# # VIAL SAMPLING ORDER COMPARISON
# 
# p1 <- ggplot(pitgasfull, aes(x=sampledepth, y=N2Oppm)) + geom_point(shape=1, aes(color=sampleorder)) + facet_grid(Month ~ pitID) + geom_line(aes(color=sampleorder)) + xlab("Sample Depth (cm)") + scale_colour_brewer(palette = "Dark2", labels=c("A (Vial 1)", "B (Vial 2)", "C (Vial 3)"))
# 
# p2 <- ggplot(pitgasfull, aes(x=sampledepth, y=CO2ppm)) + geom_point(shape=1, aes(color=sampleorder)) + facet_grid(Month ~ pitID) + geom_line(aes(color=sampleorder)) + xlab("Sample Depth (cm)") + scale_colour_brewer(palette = "Dark2", labels=c("A (Vial 1)", "B (Vial 2)", "C (Vial 3)"))
# 
# p3 <- ggplot(pitgasfull, aes(x=sampledepth, y=CH4ppm)) + geom_point(shape=1, aes(color=sampleorder)) + facet_grid(Month ~ pitID) + geom_line(aes(color=sampleorder)) + xlab("Sample Depth (cm)") + scale_colour_brewer(palette = "Dark2", labels=c("A (Vial 1)", "B (Vial 2)", "C (Vial 3)"))
# 
# # individ gas graphs
# pdf(file = paste(pathsavefigs, "soilpit-sampleorder-N2O.pdf", sep=""),width=10,height=6)
# p1 + labs(title = "Sampling Order for Trace Gas Vials, Tanguro Soil Pits")
# dev.off()
# 
# pdf(file = paste(pathsavefigs, "soilpit-sampleorder-CO2.pdf", sep=""),width=10,height=6)
# p2 + labs(title = "Sampling Order for Trace Gas Vials, Tanguro Soil Pits")
# dev.off()
# 
# pdf(file = paste(pathsavefigs, "soilpit-sampleorder-CH4.pdf", sep=""),width=10,height=6)
# p3 + labs(title = "Sampling Order for Trace Gas Vials, Tanguro Soil Pits")
# dev.off()
# 
# # grid.arrange
# pdf(file = paste(pathsavefigs, "soilpit-sampleorder.pdf", sep=""),width=8,height=16)
# grid.arrange(p1, p2, p3, nrow = 3, ncol = 1, main="Sampling Order for Trace Gas Vials, Tanguro Soil Pits")
# dev.off()
# 
# 
