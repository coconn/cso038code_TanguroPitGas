###############################################################
# Imports TDR data from two different types of sensors,
# converts units, and creates a single dataframe for data 
# analysis.
# 
# This version splits up the CR10X files to treat varying file structure
# from including Thermocouples in 2 of the 3 CR10X pits,
# and then puts them all together with the CR1000 files which are uniform

# AUTHORS: Paulo Brando, Paul Lefebvre, Marcia Macedo
# LAST UPDATED: Aug 1 2013 to separate out site ID into its own column, calculate averages, and fix months to numeric
################################################################

# clearing variables that may be stored in memory
rm(list=ls()) 

#need a couple of R packages: chron,  reshape,  lattice, reshape2

#
# coefficients for quadratic equation to calculate VWC:
# the quadratic factor
# (use these for all pits, later apply separate coefficients to Soy vs Forest pits)
a<- -.0015
# the multiplier
b<- .0943
# the intercept
c<- -1.0998


######################################################
library(chron)
library(lattice)
library(reshape2)
#######################################################

#######################################################
#set the working directory
setwd("/Users/Paul/Documents/Products/Dropbox/Paul/R/TDR/latest/")
#######################################################

#######################################################
#list the file names in the dir
f_pit = list.files(pattern="*.dat")
#######################################################

#######################################################
#separate pits CR1000 from CR10X, and CR10X pits from eachother
CR1000 = f_pit[grepl("CR1000", f_pit)]
CR10X_AP4 = f_pit[grepl("CR10X_AP4", f_pit)]
CR10X_A1A = f_pit[grepl("CR10X_A1A", f_pit)]
CR10X_A1C = f_pit[grepl("CR10X_A1C", f_pit)]
# #######################################################
# 
# 
# ############################################CR10X#####################################################################
# # Import all files from pits with CR10X
# # need to treat each CR10X pit separately
# # A1A and A1C have Temperature in col. 5, G26 doesn't
# # so skip col 5 for those two pits
# ###############################
# #set up uniform column labels for CR10X files
# 
col.names= c("prog","yr","day","hr", paste("V", 5:16, sep="."),"sourcefile")
# 
# #Pit APP4, labelled as AP4 to keep label lengths uniform:
CR10X_AP4.list = list()
for(i in 1:length(CR10X_AP4))
{
	cat("filename:", CR10X_AP4[i], "\n")
	CR10X_AP4.list[[i]] = data.frame(read.csv(CR10X_AP4[i], header=F)[,c(1:4,6:17)], NAMES=CR10X_AP4[i])
  head(CR10X_AP4.list[[i]])
  names(CR10X_AP4.list[[i]]) = col.names
}
CR10X_AP4.import = do.call("rbind", CR10X_AP4.list)

head(CR10X_AP4.import)

# 
# #Pit G26 - labelled as A1C for compatibility with Resistivity transect A1C for Area 1, Parcela C
CR10X_A1C.list = list()
for(i in 1:length(CR10X_A1C))
{
  cat("filename:", CR10X_A1C[i], "\n")
  
  CR10X_A1C.list[[i]] = data.frame(read.csv(CR10X_A1C[i], header=F)[,1:16], NAMES=CR10X_A1C[i])
  names(CR10X_A1C.list[[i]]) = col.names
}
CR10X_A1C.import = do.call("rbind", CR10X_A1C.list)

head(CR10X_A1C.import)

# 
# 
# # Pit C1 - labelled as A1A for compatiblity with Resistivity transect in Area 1, Parcela A
CR10X_A1A.list = list()
for(i in 1:length(CR10X_A1A))
{
  cat("filename:", CR10X_A1A[i], "\n")
  CR10X_A1A[1]
  head(CR10X_A1A[i])
  CR10X_A1A.list[[i]] = data.frame(read.csv(CR10X_A1A[i], header=F)[,c(1:4,6:17)],  NAMES=CR10X_A1A[i])
  #head(CR10X_A1A.list[[i]])
  names(CR10X_A1A.list[[i]]) = col.names
}
CR10X_A1A.import = do.call("rbind", CR10X_A1A.list)

head(CR10X_A1A.import)
# 
# 
# #################################
# #Now put all CR10X files together
CR10X.import = rbind(CR10X_AP4.import, CR10X_A1A.import, CR10X_A1C.import)
#View(CR10X.import)
# 
# ###############################
# #Fix dates
# 
CR10X.import$Date = as.Date(CR10X.import$day, 
						origin=as.Date(paste(CR10X.import$yr,1,1,sep="-")))
# #not sure if this next line does anything:
CR10X.import$hr = ifelse(CR10X.import$hr==2400, 0, CR10X.import$hr) # this makes sure that 24 (midnight) gets recorded as hour 0 (also midnight, but CR1000 calls it 0 and CR10X calls it 24, so need to make the same)
CR10X.import$Hour = chron(times = paste(CR10X.import$hr/100, "00", "00", sep=":")) # this converts hour from an integer to an hour:min:second thing

# #add Site name before moving on
CR10X.import$site = substr(CR10X.import$sourcefile,7,9)

#remove the sourcefile name column so uniqu() will work
CR10X.import$sourcefile = NULL 

nrow(CR10X.import) #count rows before cleaning duplicates
CR10X.clean = unique(CR10X.import) # clean duplicate rows
# 
nrow(CR10X.clean) #count rows after cleaning duplicates
# 
CR10X.import = CR10X.clean #replace the dataframe with cleaned version; retain name for later occurrences in script

#write.csv(CR10X.clean, file = "CR10Xtstpits_all.csv", row.names=F)

###############################CR1000################################################################################
#Import all files from pits running CR1000 loggers
#formats are the same for all of these dataloggers so one input routine handles all pits

CR1000.list = list()
for(i in 1:length(CR1000))
{
  
  CR1000.list[[i]] = data.frame(read.csv(CR1000[i], skip=4, header=F)[c(1:1,3:14)], NAMES=CR1000[i])
}
CR1000.import = do.call("rbind", CR1000.list)


#######################################################
#Fix dates
CR1000.import$Date = as.Date(substring(CR1000.import$V1, 1, 10))
CR1000.import$Hour = chron(times = substring(CR1000.import$V1, 11, 19))
# add site label
CR1000.import$Site = substr(CR1000.import$NAMES,8,10)

#start cleaning up the CR1000 data - remove duplicates
head(CR1000.import)
CR1000.import$NAMES = NULL
head(CR1000.import)
nrow(CR1000.import)
CR1000.clean = unique(CR1000.import)
nrow(CR1000.clean)
CR1000.import = CR1000.clean

#######################################################

############################Combine data from the two types of dataloggers#######################################################

#Prepare to rbind files
NAMES.COL= c(paste("V", 1:12,sep="."), "Date","Hour", "Site")

i.1 = CR1000.import[,-c(1:1)]
i.2 = CR10X.import[, -c(1:4)]

colnames(i.1) = NAMES.COL
head(i.1)
nrow(i.1)
colnames(i.2) = NAMES.COL
head(i.2)
nrow(i.2)
#######################################################


pits = rbind(i.1, i.2)

head(pits)
nrow(pits)

#######################################################
#order according to names, date and hour
pits = pits[order(pits$Site,pits$Date, pits$Hour),]
head(pits)
write.csv(pits, file = "pits.csv")

set negative values to NA
temp = data.frame(pits[,c(1:12)])
is.na(temp) <- temp< 0

pits.na = cbind(temp, pits[,c(13:15)])

#write.csv(pits.na, file = "pits.csv")
#######################################################
# strip things down to necessities for calculating VWC

simp <- pits[,c(1:12)]
nrow(simp)
head(pits)
head(simp)


#XXXXXXX problems here XXXXXXXXXX
#non-numeric argument to binary operator - 
tdr <- ((simp^2)*a) + (simp*b) + c
nrow(tdr)
names(tdr)= c("VWCsfc","VWC30cm","VWC50cm","VWC1m","VWC2m","VWC3m","VWC4m","VWC5m","VWC6m","VWC7m","VWC8m","VWC9m")
head(tdr)

#######################################################
#combine calculated VWC with dates/times/sites
pits$Year = years(pits$Date)

pits$Month = as.numeric(months(as.numeric(pits$Date)))
head(pits)
VWC <-cbind(pits[16:18],tdr)
head(VWC)

#Calculate monthly averages by site

df_melt <- melt(VWC, id = c("Site","Year", "Month"))
VWCave = dcast(df_melt, Site + Year + Month ~variable, mean)
VWCave
mm_10m = dcast(df_melt, Site + Year + Month ~variable, mean)

##Calculate how many mm of H2O is stored in top 9.5m of soil
#calculates using each sample point as midpoint of a meter of soil; 
#30cm acts as 25cm for top 0-50cm; then we use 1m as .5 to 1.5, etc..
#

VWCave$mmH2O = (VWCave$VWC30cm * 500) + (VWCave$VWC1m *1000) + (VWCave$VWC2m *1000) + (VWCave$VWC3m *1000)+ (VWCave$VWC4m *1000)+ (VWCave$VWC5m *1000)+ (VWCave$VWC6m *1000)+ (VWCave$VWC7m *1000)+ (VWCave$VWC8m *1000)+ (VWCave$VWC9m *1000)
VWCave

write.csv(VWCave, file = "VWCave.csv", row.names=F)







