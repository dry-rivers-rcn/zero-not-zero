#******************************************************************************
# Download and plot USGS data for zero-not-zero conceptual figure
# Kendra Kaiser August 14th, 2019; George H. Allen
#kendra rocks (hashtag)
#******************************************************************************

# this code downloads USGS data and plots the hydrographs seen in Figure 3 of 
# Zimmer et al. WATER



# install.packages("dataRetrieval")
# install.packages("tidyverse")
library(dataRetrieval)
library(tidyverse)
pCode = "00060"
setwd("~/research/2019_09_20_DryRivRCN_workshop/git/zero-not-zero/")



#******************************************************************************
# a Frozen gauge location -------------------------
Frozen="15896000" # Kuparuk R, Deadhorse, AK; alt SF Grand River near Cash, SD  06356500
FrozStart= as.Date("2017-10-01") #"2017-09-01"
FrozEnd = as.Date("2018-06-01") #"2018-07-01

FrozenQ = readNWISuv(siteNumbers = Frozen, parameterCd = pCode, 
                      startDate = FrozStart, endDate = FrozEnd
                      ) %>% renameNWISColumns() %>% data.frame


# b Flow Reversal ---------------------------------
FlowReverse= "02236125" 
ReverseStart=as.Date("2014-01-15")
ReverseEnd=as.Date("2014-02-15")

FlowReverseQ = readNWISuv(siteNumbers = FlowReverse, parameterCd = pCode, 
                           startDate = ReverseStart, endDate = ReverseEnd
                           ) %>% renameNWISColumns() %>% data.frame
FlowReverseQ$Flow_Inst = FlowReverseQ$Flow_Inst*0.028316847


# c Data/equipment error ---------------------------
DataError= "01646500" #potomac river near wash DC
DataErrorStart=as.Date("2018-06-24")
DataErrorEnd=as.Date("2018-07-02")

DataErrorQ = readNWISuv(siteNumbers = DataError, parameterCd = pCode, 
                         startDate = DataErrorStart, endDate = DataErrorEnd
                         ) %>% renameNWISColumns() %>% data.frame
DataErrorQ$Flow_Inst = DataErrorQ$Flow_Inst*0.028316847


# d Anthro Groundwater Pumping Removes flow 
AnthroGW = "07139000"  # Arkansas River at Garden City 
AgwStart = as.Date("2018-05-01") 
AgwEnd = as.Date("2018-07-15") 

AnthroGWQ = readNWISuv(siteNumbers = AnthroGW, parameterCd = pCode, 
                        startDate = AgwStart, endDate = AgwEnd
                        ) %>% renameNWISColumns() %>% data.frame
AnthroGWQ$Flow_Inst = AnthroGWQ$Flow_Inst*0.028316847


# e Anthro Upstream Surface water loss from diverting around a dam - might not need this --------
# Load non- USGS data --------
MilnerStart = as.Date("2014-10-01") 
MilnerEnd = as.Date("2015-09-31") 

Milner = read.csv("MilnerID_Data.csv", header=T, stringsAsFactors=F) #13087505 Milner Lwr Pwr Plant at Milner
Milner[,1] = as.Date(Milner[,1], "%m/%d/%y %H:%M")
Milner = Milner[Milner$Timestamp..UTC.07.00. > MilnerStart & Milner$Timestamp..UTC.07.00. < MilnerEnd, ]
names(Milner)[2] = "Q_cms"
Milner$Q_cms = Milner$Q_cms*0.028316847



#-----Conveyance around gauge -----------------
# AnthroConveyanceA= "08358400" #Rio Grande Floodway
# AnthroConveyanceB= "08358300" #Rio Grande Conveyance
# AConvStart=as.Date("2005-08-01")
# AConvEnd=as.Date("2005-11-01") 
# 
# AnthroConveyanceAQ = readNWISuv(siteNumbers = AnthroConveyanceA, parameterCd = pCode, 
#                                  startDate = AConvStart, endDate = AConvEnd
#                                  ) %>% renameNWISColumns() %>% data.frame
# AnthroConveyanceAQ$Flow_Inst = AnthroConveyanceAQ$Flow_Inst*0.028316847
# 
# AnthroConveyanceBQ = readNWISuv(siteNumbers = AnthroConveyanceB, parameterCd = pCode, 
#                                  startDate = AConvStart, endDate = AConvEnd
#                                  ) %>% renameNWISColumns() %>% data.frame


# Naturally occuring isolated pools --------------
# IsolatedPool= "09512500" #Agua Fria River nr Mayer, AZ
# IsoStart=as.Date("2018-06-01") 
# IsoEnd=as.Date("2018-08-01") 
# 
# IsoQ = readNWISuv(siteNumbers = IsolatedPool, parameterCd = pCode, 
#                    startDate = IsoStart, endDate = IsoEnd
#                    ) %>% renameNWISColumns() %>% data.frame
# IsoQ$Flow_Inst = IsoQ$Flow_Inst*0.028316847


# ----- Karst Bypass around gauge ------------
# Bypass= "" #bypass through karst Palanisamy and Workman 2015 - need data or gage number
# BypassStart=as.Date("")
# BypassEnd=as.Date("")
# 
#can probably remove this one
# UpstreamLoss= "06879650" #kings Creek nr Manhattan, KS
# UpStart=as.Date("") #"2018-12-01 - when you zoom in it's actually at 1 cfs - better time-frame?
# UpEnd=as.Date("") #"2018-07-01












#******************************************************************************
# PLOT FIGURE 3
#******************************************************************************



pdfOutPath = "Figure3.pdf"
pdf(pdfOutPath, width = 7.5, height = 7.5 )

layout(matrix(1:6, nrow=3, byrow=T))
par(mar=c(4,4,1,1))



# a Frozen gauge location -------------------------
D = FrozenQ$dateTime
Q = FrozenQ$Flow_Inst
plot(D, Q, type="n", col="blue", xlab="", ylab="Q (cms)")

boxVec = rep(0, length(Q))
boxVec[FrozenQ$Flow_Inst_cd == "A e"] = 1
boxDif = diff(boxVec)
boxLeft = D[boxDif == 1]
boxRight = D[boxDif == -1]
boxBottom = min(Q)
boxTop = max(Q)


plot(FrozenQ$dateTime, FrozenQ$Flow_Inst, 
     type="l", col="blue", xlab="", ylab="Q (cms)", 
     lwd=1)
# lines(FrozenQ$dateTime[FrozenQ$Flow_Inst <= 0.001],
#       FrozenQ$Flow_Inst[FrozenQ$Flow_Inst <= 0.001], 
#       lty="dotted", col='white', lwd=3)
# axis.POSIXct(1, FrozenQ$dateTime, format="%Y", padj=1.8)

rect(boxLeft, boxBottom, boxRight, boxTop)

# b Flow Reversal ---------------------------------
posQ = FlowReverseQ$Flow_Inst
negQ = FlowReverseQ$Flow_Inst
posQind = FlowReverseQ$Flow_Inst > 0
negQind = FlowReverseQ$Flow_Inst < 0

plot(FlowReverseQ$dateTime, FlowReverseQ$Flow_Inst, type="n", col="blue", xlab="", ylab="", lwd=1)

posQ[negQind] = NA
negQ[posQind] = NA

lines(FlowReverseQ$dateTime, posQ, col="blue", xlab="", ylab="", lwd=1)
lines(FlowReverseQ$dateTime, negQ, col="light blue", xlab="", ylab="", lwd=1)
abline(h=0)

axis.POSIXct(1, FlowReverseQ$dateTime, format="%Y", padj=1.8)


# c Data/equipment error ---------------------------
plot(DataErrorQ$dateTime, DataErrorQ$Flow_Inst, type="l", main="", 
     col="blue", xlab="", ylab="Q (cms)", lwd=1)
# lines(DataErrorQ$dateTime[DataErrorQ$Flow_Inst <= 0.001], 
#       DataErrorQ$Flow_Inst[DataErrorQ$Flow_Inst <= 0.001], lty="dotted", col='white', lwd=3)
axis.POSIXct(1, DataErrorQ$dateTime, format="%Y", padj=1.8)


# d Anthro Groundwater Pumping Removes flow 
plot(AnthroGWQ$dateTime, AnthroGWQ$Flow_Inst, type="l", main="" ,
     col="blue", xlab="Date", ylab="Q (cms)", lwd=1)
# lines(AnthroGWQ$dateTime[AnthroGWQ$Flow_Inst <= 0.001], AnthroGWQ$Flow_Inst[AnthroGWQ$Flow_Inst <= 0.001], lty="dotted", col='white', lwd=3)
axis.POSIXct(1, AnthroGWQ$dateTime, format="%Y", padj=1.8)


# e Anthro Upstream Surface water loss from diverting around a dam - might not need this
plot(Milner$Timestamp..UTC.07.00., Milner$Q_cms, type="l", main="" ,
     col="blue", xlab="Date", ylab="Q (cms)", lwd=1)
axis.POSIXct(1, Milner$Timestamp..UTC.07.00., format="%Y", padj=1.8)


# Naturally occuring isolated pools --------------
# plot(IsoQ$dateTime, IsoQ$Flow_Inst, type="l", main="",
#      col="blue", xlab="", ylab="", lwd=1)
# # lines(IsoQ$dateTime[IsoQ$Flow_Inst <= 0.001], IsoQ$Flow_Inst[IsoQ$Flow_Inst <= 0.001], 
# #       lty="dotted", col='white', lwd=3)
# axis.POSIXct(1, IsoQ$dateTime, format="%Y", padj=1.8)


# plot(AnthroConveyanceAQ$dateTime, AnthroConveyanceAQ$Flow_Inst, type="l", main= "",
#      col="blue", xlab="Date", ylab="", lwd=1)
# # lines(AnthroConveyanceAQ$dateTime[AnthroConveyanceAQ$Flow_Inst <= 0.001], AnthroConveyanceAQ$Flow_Inst[AnthroConveyanceAQ$Flow_Inst <= 0.001], lty="dotted", col='white', lwd=3)
# axis.POSIXct(1, AnthroConveyanceAQ$dateTime, format="%Y", padj=1.8)

dev.off()
cmd = paste('open', pdfOutPath)
system(cmd)


# update vertical axis