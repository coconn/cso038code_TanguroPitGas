# Pit-TraceGas-WetSeasonSurface-StatModel.R
# looking at the surface fluxes from the Tanguro GHG paper from only the wet season to help us interpret the pit gas production model
# disrupted N project
# CS O'Connell, UMN EEB/IonE


########################################################################
# BRING IN DATA / PREP

library(ggplot2)
library(gridExtra)
#library(scales)
library(plyr)
#library(data.table)
library(reshape2)
library(tidyr)
library(magrittr)
library(lubridate)

# where to save outputs
pathsavetab = "~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Analyses/PitTables/"
pathsavefigs = "~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Analyses/PitGasFigures/"

# gen_data_aov_onlymeansdN.R
source("~/Documents/GITHUB/RPersonalFunctionsChristine/gen_data_aov_onlymeansdN.r")

# data

# forest, soybean/maize, soybean

# wet season flux means
N2Omeans <- c(1.383691, 1.671915, 0.8115679)
CO2means <- c(13.59264, 16.84562, 22.226)
CH4means <- c(-0.0002809052, -0.0004615075, -0.0004677696)

# wet season flux std
N2Ostds <- c(1.813256208, 4.771374396, 1.165351991)
CO2stds <- c(4.386105812, 12.176281566, 20.234899779)
CH4stds <- c(0.006081409, 0.001392989, 0.005359183)

# sample size
# see Tables-TanguroN2OLosses-StatModel-TwoWayANOVALUtypeSeason.R
samplesizes <- c(87, 189, 39)

# power through these and then I pasted them into an excel doc

# n2o wet season flux
N2Osimulated_data <- gen_data_aov_onlymeansdN(means=N2Omeans, sds=N2Ostds, samplesizes=samplesizes)
av <- aov(y ~ group, data = N2Osimulated_data)
summary(av)
TukeyHSD(av)

# CO2 wet season flux
CO2simulated_data <- gen_data_aov_onlymeansdN(means=CO2means, sds=CO2stds, samplesizes=samplesizes)
av <- aov(y ~ group, data = CO2simulated_data)
summary(av)
TukeyHSD(av)

# CH4 wet season flux
CH4simulated_data <- gen_data_aov_onlymeansdN(means=CH4means, sds=CH4stds, samplesizes=samplesizes)
av <- aov(y ~ group, data = CH4simulated_data)
summary(av)
TukeyHSD(av)

# pasted these outcomes into an excel
# see Tanguro-wetseason-surfaceflux-onewayANOVA.xlsx




