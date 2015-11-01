# Tanguro_soils_concentration_dCdz.R
#
# get the rate of change (dC/dz) for the concentrations in Tanguro pits
#
# BEWARE: CHRISTINE PANIC STRATEGY... this code just facilitates this entire thing happening by hand in excel
#
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# requires:
# made csv by hand in "diffusion calcs" folder



########################################################################
# BRING IN DATA / PREP

df <- read.csv("~/Documents/GITHUB/cso038code_TanguroPitGas/Diffusion Calcs/Tanguro_soils_concentration_dCdz.csv", stringsAsFactors=FALSE)

library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(data.table)
library(lubridate)
library(splines)

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso038code_TanguroPitGas/Diffusion Calcs/"
pathsavefigs = "~/Documents/GITHUB/cso038code_TanguroPitGas/Diffusion Calcs/"


########################################################################
# LOOK AT JUST ONE PIT FIRST

# only run this when you're starting off, then be sure not to paper over your thing after that
devdf_full <- data.frame(sampledepth = c(NA), derivative = c(NA), Month = c(NA), pitID = c(NA), devdepth = c(NA))

# run this before switching to another gas
#devdf_full_N2O <- devdf_full
#devdf_full_N2O$dCdz_N2O <- devdf_full_N2O$derivative

# combos: MU Dec/Feb, C2 Feb, K4 Feb/Dec, M8 Dec/Feb
monthname <- "Feb"
pitIDname <- "M8"

lmdf <- df[df$Month==monthname,]
lmdf2 <- lmdf[lmdf$pitID==pitIDname,]
# switch out the gas between ngC_cm3_CH4, ngN_cm3_N2O, ngC_cm3_CO2
y <- lmdf2$ngN_cm3_N2O
x <- lmdf2$sampledepth

# graph base points
p <- ggplot(lmdf2, aes(x = sampledepth, y = ngN_cm3_N2O)) + geom_point()
print(p)

# model
model1 <- lm(y ~ poly(x, 1))
summary(model1)
model2 <- lm(y ~ poly(x, 2))
summary(model2)
model3 <- lm(y ~ poly(x, 3))
summary(model3)
#modelNS <- lm(y~ns(x),data=lmdf2)
#summary(modelNS)
modelLoess <- loess(y~x,data=lmdf2)
summary(modelLoess)

# put models onto graph
p + stat_smooth(method = "lm", formula = y ~ poly(x, 1), size = 1)
p + stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1)
p + stat_smooth(method = "lm", formula = y ~ poly(x, 3), size = 1)
#p + stat_smooth(method = "lm", formula = y ~ ns(x), size = 1)
p + stat_smooth(method = "loess", size = 1)

# decide which
modelusehere <- modelLoess

# get the derivative of the desired model

X <- data.frame(t=seq(0,450,length=450)) # make an ordered sequence
Y <- predict(modelusehere,newdata=data.frame(x=X$t)) # calculate predictions for that sequence
plot(X$t,Y,type="l",main="Original fit") # check

dY <- diff(Y)/diff(X$t)  # the derivative of your function
dX <- rowMeans(embed(X$t,2)) # centers the X values for plotting
plot(dX,dY,type="l",main="Derivative") #check

# copy the model type and R^2 over to the spreadsheet
summary(modelusehere)$r.squared

# get the derivative at each depth

# draw the derivatives from this depth
devdepths <- c(17,39,74,149,249,349,449)

devdf <- data.frame(sampledepth = c(15,15,15,40,40,40,75,75,75,150,150,150,250,250,250,350,350,350,450,450,450))

devdf$derivative[1:3] <- dY[devdepths[1]]
devdf$derivative[4:6] <- dY[devdepths[2]]
devdf$derivative[7:9] <- dY[devdepths[3]]
devdf$derivative[10:12] <- dY[devdepths[4]]
devdf$derivative[13:15] <- dY[devdepths[5]]
devdf$derivative[16:18] <- dY[devdepths[6]]
devdf$derivative[19:21] <- dY[devdepths[7]]

devdf$devdepth[1:3] <- devdepths[1] + 1
devdf$devdepth[4:6] <- devdepths[2] + 1
devdf$devdepth[7:9] <- devdepths[3] + 1
devdf$devdepth[10:12] <- devdepths[4] + 1
devdf$devdepth[13:15] <- devdepths[5] + 1
devdf$devdepth[16:18] <- devdepths[6] + 1
devdf$devdepth[19:21] <- devdepths[7] + 1

devdf$Month <- monthname
devdf$pitID <- pitIDname

# put onto the full devdf_full
devdf_full <- rbind(devdf_full,devdf)

# copy this over to the right place in excel





########################################################################
# NOTES AND TESTING




