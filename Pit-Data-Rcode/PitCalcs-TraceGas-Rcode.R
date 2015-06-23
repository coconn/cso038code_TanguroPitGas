# PitCalcs-TraceGas-Rcode.R
# taking trace gas vial data associated with soil pits and making a tidy data set
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# requires files created in GC-Rcode-fileloop.R

# output products:
# pitgasfull.csv: master csv of soil pit gas sampling
# pitgassummary.csv 


########################################################################
# BRING IN DATA, MAKE DATAFRAME

library(lubridate)

vialDFfull <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rprocessed/vialDFfull.csv", stringsAsFactors=FALSE)

# make new column to ensure no repeats (easycallname = unique per chamber)
vialDFfull$easycallname <- do.call(paste, c(vialDFfull[c("Site", "SampleDate", "Chamber")], sep = "_")) 

#in case I want to see all the names outside of R environment
#write.csv(vialDFfull, file=paste("vialDFfull_prac.csv", sep = ""), row.names=FALSE)  

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Rprocessed/"
pathsavefigs = "~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Analyses/PitGasFigures/"



########################################################################
# GET PIT DATA

# Pits: m8, k4, c2 (forest); mu (mutun, soy)

toMatch <- c("cm") #"M8", "K4", "MU", "C2" # C2 doesn't work because it goes with some side-by-side chamber trials
pitgas <- subset(vialDFfull, grepl(paste(toMatch,collapse="|"), vialDFfull$SampleName))



########################################################################
# ELIMINATE UNACCEPTABLE DATA

# no pressure vials
nopressureid <- pitgas$Pressure=="N"
NoPressureCount <- length(which(pitgas$Pressure=="N"))

pitgas$ngN_cm3_N2O[nopressureid] <- NA
pitgas$ngC_cm3_CO2[nopressureid] <- NA
pitgas$ngC_cm3_CH4[nopressureid] <- NA

pitgas$N2Oppm[nopressureid] <- NA
pitgas$CO2ppm[nopressureid] <- NA
pitgas$CH4ppm[nopressureid] <- NA

# print info
print(paste("VIAL PRESSURE INFO: there was/were ", NoPressureCount, " vial(s) with no pressure.", sep = ""))

# some vials got repeated because of GC autosampler problems
rerunid <- grep("Rerun_", pitgas$SampleName)

pitgas$ngN_cm3_N2O[rerunid] <- NA
pitgas$ngC_cm3_CO2[rerunid] <- NA
pitgas$ngC_cm3_CH4[rerunid] <- NA

pitgas$N2Oppm[rerunid] <- NA
pitgas$CO2ppm[rerunid] <- NA
pitgas$CH4ppm[rerunid] <- NA

# any other vials that didn't seem to get sampled?
nodata <- pitgas$N2Oraw<1.0

pitgas$ngN_cm3_N2O[nodata] <- NA
pitgas$ngC_cm3_CO2[nodata] <- NA
pitgas$ngC_cm3_CH4[nodata] <- NA

pitgas$N2Oppm[nodata] <- NA
pitgas$CO2ppm[nodata] <- NA
pitgas$CH4ppm[nodata] <- NA

# recall that all three of these vials got rerun in the GC, so there is no data that is truly missing, only blank rows to be struck

# some of the land use codes are "r" because of the rerun vials
# site and LUtype have "re" and "r"
Rid <- grep("R", pitgas$Site)

samplenamesRe <- pitgas$SampleName[Rid]
sitetmp <- substr(samplenamesRe, 7, 8)
LUtmp <- substr(samplenamesRe, 7, 7)

pitgas$Site[Rid] <- sitetmp
pitgas$LUtype[Rid] <- LUtmp


########################################################################
# ADD USEFUL COLUMNS FOR PIT INFO

# pit ID
pitgas$pitID <- -9999
pitgas$pitID[grep("M8", pitgas$SampleName)] <- "M8"
pitgas$pitID[grep("K4", pitgas$SampleName)] <- "K4"
pitgas$pitID[grep("MU", pitgas$SampleName)] <- "MU"
pitgas$pitID[grep("C2", pitgas$SampleName)] <- "C2"

# depth
pitgas$sampledepth <- -9999
pitgas$sampledepth[grep("15cm", pitgas$SampleName)] <- 15
pitgas$sampledepth[grep("40cm", pitgas$SampleName)] <- 40
pitgas$sampledepth[grep("75cm", pitgas$SampleName)] <- 75
pitgas$sampledepth[grep("150cm", pitgas$SampleName)] <- 150
pitgas$sampledepth[grep("250cm", pitgas$SampleName)] <- 250
pitgas$sampledepth[grep("350cm", pitgas$SampleName)] <- 350
pitgas$sampledepth[grep("450cm", pitgas$SampleName)] <- 450

# sample order
pitgas$sampleorder <- -9999
pitgas$sampleorder[grep("cm-A", pitgas$SampleName)] <- "A"
pitgas$sampleorder[grep("cm-B", pitgas$SampleName)] <- "B"
pitgas$sampleorder[grep("cm-C", pitgas$SampleName)] <- "C"


########################################################################
# IMPROVE DATE INFO

pitgas$SampleDate <- gsub("[.]","/",pitgas$SampleDate)
pitgas$SampleDate <- as.Date(pitgas$SampleDate, format="%Y/%m/%d")

# month of sampling
pitgas <- transform(pitgas, Month = month(pitgas$SampleDate, label=TRUE))


########################################################################
# BRING IN THE CENA JANKOWSKI DATA, RBIND

cenapitvials <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-RawFolders/2015 Feb CENA Jankowski data/cenajankowskipitdata.csv", stringsAsFactors=FALSE)

# make dates the same
cenapitvials$SampleDate <- as.Date(cenapitvials$SampleDate, format="%m/%d/%y")

# throw out that insanely high CO2 measurement
cenapitvials$CO2ppm[grep("122,658,983", cenapitvials$CO2ppm)] <- NA

# make sure these are numeric
cenapitvials$N2Oppm <- as.numeric(cenapitvials$N2Oppm)
cenapitvials$CO2ppm <- as.numeric(cenapitvials$CO2ppm)
cenapitvials$CH4ppm <- as.numeric(cenapitvials$CH4ppm)

# combine
pitgas <- rbind(pitgas,cenapitvials)

# MU and MU are different for some reason when I try to graph them; this line solves that
pitgas$pitID[grep("MU", pitgas$pitID)] <- "MU"


########################################################################
# SAVE CSV

# save pitgasfull as csv file
pitgasfull <- pitgas
write.csv(pitgasfull, file=paste(pathsavefiles, "pitgasfull.csv", sep = ""), row.names=FALSE)  




########################################################################
# NOTES AND TESTING

# possibility that when I did rbind above that the categories of some of the variables changed, but doesn't look like it:
# str(pitgas)
# str(cenapitvials)



