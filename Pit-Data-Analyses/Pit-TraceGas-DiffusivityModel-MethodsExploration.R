# Pit-TraceGas-DiffusivityModel-MethodsExploration.R
#
# applying a diffusion model to the trace gas data
# IMPORTANT: nothing actually happens in this script!  Christine did everything for the diffusivity --> flux estimates by hand in excel, using "diffusion calcs.xlsx".  See the first section below for an explanation.
#
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# requires files created in:
# PitCalcs-TraceGas-Rcode.R
# PitCalcs-Thermocouple-Rcode.R



########################################################################
# WHAT IS THIS SCRIPT?

# Christine was trying to figure out how to work a diffusivity --> flux at depth model
# She read through Davidson et al. 2006 and Davidson and Trumbore 1995
# This script was working through the method (primarily from Davidson 2006, but using some wisdom from 1995) with a small fake set of data so she would understand how to do each step
# From there, she worked through the steps by hand in excel
# See cso038code_TanguroPitGas/Diffusion Calcs/diffusion calcs.xlsx
# Importantly, to create the "concentration dC.dz R data" tab in diffusion calcs.xlsx, she used Tanguro_soils_concentration_dCdz.R and pasted the results into excel
# So, the diffusivity model results are in the last tab in diffusion calcs.xlsx
# Then she plotted and analyzed those results in Pit-TraceGas-DiffusivityModel-Analysis.R




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

# TDR data
pitTDRsummary <- read.csv("~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Rprocessed/pitTDRsummarytable.csv", stringsAsFactors=FALSE)


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
# CALCULATE OTHER THINGS NEEDED FOR THE DIFFUSIVITY MODEL
#
# see Davidson et al. 2006
#

# total porosity
# Total porosity (cm3 cm􏰀3) was calculated from measures of BD:
# total porosity = 1 - (BD/PD)
# where BD is the measured bulk density 
# and PD is the weighted average particle density of minerals 
# (assumed particle density of 2.65 g cm)
# and organic matter (assumed particle density of 1.4 g cm􏰀3):
# PD = [(OM * 1.4) + (100 - OM) * 2.65]/100
# where OM is the percent organic matter, assuming that it is 58% carbon.

# how many depths? ans: 7
length(unique(pitgasfull$sampledepth))

# total porosity
BD <- c(1,1,1,1) # bulk density at each depth
OM <- c(0,0,0,0) # percent organic matter at each depth; using 0% since Davidson 1995 assumed it was all minerals, since the OM and iron amounts are small and thus trivial to overall porosity, so I guess I can just do that, too
PD <- ((OM * 1.4) + (100 - OM) * 2.65)/100
totalporosity <- 1 - (BD/PD)

# water filled porosity and air filled porosity
# water filled porositythe same as the TDR volumetric soil water content
waterporosity <- c(0.22,0.22,0.22,0.22)
# air filled porosity is total minus water filled
airporosity <- totalporosity - waterporosity

# diffusion coefficients in air for each gas
# from Fuller et al. 1966 as read off a table in Pritchard and Currie 1982
D_o_N2O <- 0.122
D_o_CO2 <- 0.136
D_o_CH4 <- 0.81

# calculate effective diffusivity in soil (D_P)
# from Davidson and Trumbore 1995, eq. 3, originally from Millington 1959
# diffusion of gases through wet media
# D_P/D_o = airporosity^(4/3)*(airporosity/totalporosity)^2
# where D_P is diffusion coefficient in soil (solve for), D_o is diffusion coefficient in air (from lit)
D_P_N2O = ((airporosity^(4/3))*(airporosity/totalporosity)^2)*D_o_N2O
D_P_CO2 = ((airporosity^(4/3))*(airporosity/totalporosity)^2)*D_o_CO2
D_P_CH4 = ((airporosity^(4/3))*(airporosity/totalporosity)^2)*D_o_CH4

# get the concentration gradient for each gas at each depth
# bring in gas data
# plot the spline fit
# get the derivative at each depth point
# final things to make:
dC_dz_N2O <- c(-0.5,-1,-2,-10) # concentration gradient (first derivative of the exponential fit of the gas concentr.)
dC_dz_CO2 <- c(-0.5,-1,-2,-10)
dC_dz_CH4 <- c(-0.5,-1,-2,-10)

# temperature
T <- c(298,298,298,298) # kelvin soil air temp

# use fick's law to get the flux from each depth
flux_N2O <- D_P_N2O * dC_dz_N2O * 52700/T
flux_CO2 <- D_P_CO2 * dC_dz_CO2 * 52700/T
flux_CH4 <- D_P_CH4 * dC_dz_CH4 * 52700/T






# running things to collect next time I am at Tanguro in the pits
# OM % at each depth in each pit
# iron content at each depth in each pit
# BD at each depth in each pit



