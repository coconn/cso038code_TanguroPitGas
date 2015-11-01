# Pit-TraceGas-DiffusivityModel-Analysis.R
#
# applying a diffusion model to the trace gas data (actually, applied in an excel doc)
# analyzing and graphing that data
#
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# see also:
# cso038code_TanguroPitGas/Diffusion Calcs/diffusion calcs.xlsx
# Pit-TraceGas-DiffusivityModel-MethodsExploration.R



########################################################################
# BRING IN DATA / PREP

library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(data.table)
library(lubridate)
library(xlsx)

# where to save outputs
pathfile = "~/Documents/GITHUB/cso038code_TanguroPitGas/Diffusion Calcs/"
pathsavefiles = "~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Rprocessed/"
pathsavefigs = "~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Analyses/PitGasFigures/"

# bring in data
data <- read.xlsx(paste(pathfile,"diffusion calcs.xlsx",sep=""),"DATASET")
pitdf <- as.data.frame(data, stringsAsFactors=FALSE)

# create a col to assign a color in the graphs
pitdf <- transform(pitdf, color.use = ifelse(LUtype=="M", as.character("darkorange"), ifelse(LUtype=="F", as.character("darkgreen"), as.character("darkblue"))))

# create a col to assign a better name to each land use
pitdf <- transform(pitdf, LUname = ifelse(LUtype=="M", as.character("Soya/Maize DC"), ifelse(LUtype=="F", as.character("Forest"), as.character("Soya SC"))))

# create columns to change units
pitdf$flux_N2O_ngNcm2h <- pitdf$flux_N2O*10000
pitdf$flux_CO2_ugCcm2h <- pitdf$flux_CO2*100
pitdf$flux_CH4_ugCcm2h <- pitdf$flux_CH4*100

########################################################################
# DESCRIPTIVE STATS SUMMARY

# Run the functions length, mean, and sd on the value of "change" for each group, 
# broken down by sex + condition

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# summarySE
summarytab1 <- summarySE(data=pitdf, measurevar="flux_N2O_ngNcm2h", c("pitID", "sampledepth", "SampleDate", "LUtype", "LUname", "color.use", "Month"), na.rm=TRUE, renameallcols=TRUE)
summarytab2 <- summarySE(data=pitdf, measurevar="flux_CO2_ugCcm2h", c("pitID", "sampledepth", "SampleDate"), na.rm=TRUE, renameallcols=TRUE)
summarytab3 <- summarySE(data=pitdf, measurevar="flux_CH4_ugCcm2h", c("pitID", "sampledepth", "SampleDate"), na.rm=TRUE, renameallcols=TRUE)

# join
pitfluxsummary <- join(x = summarytab1, y = summarytab2, by = c("pitID", "sampledepth", "SampleDate", "N"))
pitfluxsummary <- join(x = pitfluxsummary, y = summarytab3, by = c("pitID", "sampledepth", "SampleDate", "N"))

# put YearMonth in as variable
pitfluxsummary <- transform(pitfluxsummary, YearMonth = paste(year(pitfluxsummary$SampleDate),month(pitfluxsummary$SampleDate),sep="-"))
# put YearMonthOrder in as variable
pitfluxsummary <- transform(pitfluxsummary, YearMonthOrder = paste(month(pitfluxsummary$SampleDate),year(pitfluxsummary$SampleDate),sep="-"))
pitfluxsummary$YearMonthOrder <- factor(pitfluxsummary$YearMonthOrder, levels = c("12-2013", "1-2015", "2-2014"))
# add forest vs. agriculture label
pitfluxsummary$ForAgri <- "Forest"
pitfluxsummary$ForAgri[grepl("Soya", pitfluxsummary$LUname)] <- "Agriculture"


########################################################################
# SIMPLE SCATTERPLOTS OVER DEPTH

# gas by pit ID
p1 <- ggplot(pitdf, aes(x=sampledepth, y=flux_N2O_ngNcm2h)) + geom_point(shape=1) + geom_line(aes(color=Month)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("Sample Depth (cm)") + ylab("N2O flux (ng N cm^-2 h^-1)") + scale_colour_discrete(name="Sampling Period") + theme_bw()

p2 <- ggplot(pitdf, aes(x=sampledepth, y=flux_CO2_ugCcm2h)) + geom_point(shape=1) + geom_line(aes(color=Month)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("Sample Depth (cm)") + ylab("CO2 flux (ug C cm^-2 h^-1)") + scale_colour_discrete(name="Sampling Period") + theme_bw()

p3 <- ggplot(pitdf, aes(x=sampledepth, y=flux_CH4_ugCcm2h)) + geom_point(shape=1) + geom_line(aes(color=Month)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("Sample Depth (cm)") + ylab("CH4 flux (ug C cm^-2 h^-1)") + scale_colour_discrete(name="Sampling Period") + theme_bw()

# gas by land use type
p4 <- ggplot(pitfluxsummary, aes(x=sampledepth, y=meanflux_N2O_ngNcm2h)) + geom_point(shape=1) + geom_line(aes(color=YearMonthOrder, linetype=pitID)) + coord_flip() + scale_x_reverse() + facet_grid(LUname ~ .) + xlab("Sample Depth (cm)") + ylab("N2O flux (ng N cm^-2 h^-1)") + scale_colour_discrete(name="Sampling Period") + theme_bw()

p5 <- ggplot(pitfluxsummary, aes(x=sampledepth, y=meanflux_CO2_ugCcm2h)) + geom_point(shape=1) + geom_line(aes(color=YearMonthOrder, linetype=pitID)) + coord_flip() + scale_x_reverse() + facet_grid(LUname ~ .) + xlab("Sample Depth (cm)") + ylab("CO2 flux (ug C cm^-2 h^-1)") + scale_colour_discrete(name="Sampling Period") + theme_bw()

p6 <- ggplot(pitfluxsummary, aes(x=sampledepth, y=meanflux_CH4_ugCcm2h)) + geom_point(shape=1) + geom_line(aes(color=YearMonthOrder, linetype=pitID)) + coord_flip() + scale_x_reverse() + facet_grid(LUname ~ .) + xlab("Sample Depth (cm)") + ylab("CH4 flux (ug C cm^-2 h^-1)") + scale_colour_discrete(name="Sampling Period") + theme_bw()

# gas by forest vs. agriculture
p7 <- ggplot(pitfluxsummary, aes(x=sampledepth, y=meanflux_N2O_ngNcm2h)) + geom_point(shape=1) + geom_line(aes(color=YearMonthOrder, linetype=pitID)) + coord_flip() + scale_x_reverse() + facet_grid(ForAgri ~ .) + xlab("Sample Depth (cm)") + ylab("N2O flux (ng N cm^-2 h^-1)") + scale_colour_discrete(name="Sampling Period") + theme_bw()

p8 <- ggplot(pitfluxsummary, aes(x=sampledepth, y=meanflux_CO2_ugCcm2h)) + geom_point(shape=1) + geom_line(aes(color=YearMonthOrder, linetype=pitID)) + coord_flip() + scale_x_reverse() + facet_grid(ForAgri ~ .) + xlab("Sample Depth (cm)") + ylab("CO2 flux (ug C cm^-2 h^-1)") + scale_colour_discrete(name="Sampling Period") + theme_bw()

p9 <- ggplot(pitfluxsummary, aes(x=sampledepth, y=meanflux_CH4_ugCcm2h)) + geom_point(shape=1) + geom_line(aes(color=YearMonthOrder, linetype=pitID)) + coord_flip() + scale_x_reverse() + facet_grid(ForAgri ~ .) + xlab("Sample Depth (cm)") + ylab("CH4 flux (ug C cm^-2 h^-1)") + scale_colour_discrete(name="Sampling Period") + theme_bw()



########################################################################
# SAVE SCATTERPLOTS OVER DEPTH

########################################################################
# SAVE SCATTERPLOTS OVER DEPTH

# versions with shared legend
source("~/Documents/GITHUB/RPersonalFunctionsChristine/grid_arrange_shared_legend.r")

# gas by pit ID
png(file = paste(pathsavefigs, "soilpit-tracegasproduction_pitID.png", sep=""),width=4,height=16,units="in",res=400)
grid_arrange_shared_legend(p1, p2, p3, nrow = 1, ncol = 3, main="Trace Gas Production, Tanguro Soil Pits")
dev.off()

a <- p1 + theme(legend.position="none"); b <- p2 + theme(legend.position="none")
png(file = paste(pathsavefigs, "soilpit-tracegasproduction_pitID_alt.png", sep=""),width=10,height=6,units="in",res=400)
grid.arrange(a, b, p3, nrow = 1, ncol = 3, main="Trace Gas Production, Tanguro Soil Pits", widths=c(1,1,1.5))
dev.off()


# gas by land use type
a <- p4 + theme(legend.position="none"); b <- p5 + theme(legend.position="none")
png(file = paste(pathsavefigs, "soilpit-tracegasproduction_LUtype_alt.png", sep=""),width=10,height=6,units="in",res=400)
grid.arrange(a, b, p6, nrow = 1, ncol = 3, main="Trace Gas Production, Tanguro Soil Pits", widths=c(1,1,1.5))
dev.off()

# doesn't work because the line type legend doesn't also get moved over
# png(file = paste(pathsavefigs, "soilpit-tracegasconcentrations_LUtype.png", sep=""),width=4,height=16,units="in",res=400)
# grid_arrange_shared_legend(p3, p4, p5, nrow = 1, ncol = 3, main="Trace Gas Concentration, Tanguro Soil Pits")
# dev.off()


# gas by forest vs. agriculture
a <- p7 + theme(legend.position="none"); b <- p8 + theme(legend.position="none")
png(file = paste(pathsavefigs, "soilpit-tracegasproduction_ForAgri_alt.png", sep=""),width=10,height=6,units="in",res=400)
grid.arrange(a, b, p9, nrow = 1, ncol = 3, main="Trace Gas Production, Tanguro Soil Pits", widths=c(1,1,1.5))
dev.off()

# doesn't work because the line type legend doesn't also get moved over
# png(file = paste(pathsavefigs, "soilpit-tracegasconcentrations_LUtype.png", sep=""),width=4,height=16,units="in",res=400)
# grid_arrange_shared_legend(p3, p4, p5, nrow = 1, ncol = 3, main="Trace Gas Concentration, Tanguro Soil Pits")
# dev.off()




########################################################################
# NOTES AND TESTING






