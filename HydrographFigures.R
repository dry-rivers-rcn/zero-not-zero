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
FrozStart= as.Date("2017-8-01") #"2017-09-01"
FrozEnd = as.Date("2018-7-31") #"2018-07-01

FrozenQ = readNWISdv(siteNumbers = Frozen, parameterCd = pCode, 
                     startDate = FrozStart, endDate = FrozEnd
)# %>% renameNWISColumns() %>% data.frame

# b Flow Reversal ---------------------------------
# FlowReverse= "02236125"
# ReverseStart=as.Date("2014-01-15")
# ReverseEnd=as.Date("2014-02-15")

FlowReverse = "04194085"
ReverseStart = as.Date("2012-05-01")
ReverseEnd = as.Date("2012-06-30")


FlowReverseQ = readNWISdv(siteNumbers = FlowReverse, parameterCd = pCode, 
                          startDate = ReverseStart, endDate = ReverseEnd
)# %>% renameNWISColumns() %>% data.frame
FlowReverseQ$X_00060_00003 = FlowReverseQ$X_00060_00003*0.028316847


# c Data/equipment error ---------------------------
DataError = "01646500" #potomac river near wash DC
DataErrorStart = as.Date("2019-06-24")
DataErrorEnd = as.Date("2019-09-02")

DataErrorQ = readNWISuv(siteNumbers = DataError, parameterCd = pCode, 
                        startDate = DataErrorStart, endDate = DataErrorEnd
                        ) %>% renameNWISColumns() %>% data.frame
DataErrorQ$Flow_Inst = DataErrorQ$Flow_Inst*0.028316847


# d Anthro Upstream Surface water loss from diverting around a dam - might not need this --------
# Load non- USGS data --------
MilnerStart = as.Date("2014-10-01") 
MilnerEnd = as.Date("2015-09-30") 

Milner = read.csv("MilnerID_Data.csv", header=T, stringsAsFactors=F) #13087505 Milner Lwr Pwr Plant at Milner
Milner[,1] = as.Date(Milner[,1], "%m/%d/%y %H:%M")
Milner = Milner[Milner$Timestamp..UTC.07.00. > MilnerStart & Milner$Timestamp..UTC.07.00. < MilnerEnd, ]
names(Milner)[2] = "Q_cms"
Milner$Q_cms = Milner$Q_cms*0.028316847

# e Anthro Groundwater Pumping Removes flow 
AnthroGW = "07139000"  # Arkansas River at Garden City 
AgwStart = as.Date("2018-05-01") 
AgwEnd = as.Date("2018-07-15") 

AnthroGWQ = readNWISdv(siteNumbers = AnthroGW, parameterCd = pCode, 
                       startDate = AgwStart, endDate = AgwEnd
)# %>% renameNWISColumns() %>% data.frame
AnthroGWQ$X_00060_00003 = AnthroGWQ$X_00060_00003*0.028316847






#-----Conveyance around gauge -----------------
# AnthroConveyanceA= "08358400" #Rio Grande Floodway
# AnthroConveyanceB= "08358300" #Rio Grande Conveyance
# AConvStart=as.Date("2005-08-01")
# AConvEnd=as.Date("2005-11-01") 
# 
# AnthroConveyanceAQ = readNWISdv(siteNumbers = AnthroConveyanceA, parameterCd = pCode, 
#                                  startDate = AConvStart, endDate = AConvEnd
#                                  ) %>% renameNWISColumns() %>% data.frame
# AnthroConveyanceAQ$Flow_Inst = AnthroConveyanceAQ$Flow_Inst*0.028316847
# 
# AnthroConveyanceBQ = readNWISdv(siteNumbers = AnthroConveyanceB, parameterCd = pCode, 
#                                  startDate = AConvStart, endDate = AConvEnd
#                                  ) %>% renameNWISColumns() %>% data.frame


# Naturally occuring isolated pools --------------
# IsolatedPool= "09512500" #Agua Fria River nr Mayer, AZ
# IsoStart=as.Date("2018-06-01") 
# IsoEnd=as.Date("2018-08-01") 
# 
# IsoQ = readNWISdv(siteNumbers = IsolatedPool, parameterCd = pCode, 
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

# set up plot:
pdfOutPath = "Figure3.pdf"
pdf(pdfOutPath, width=4, height = 9)

layout(matrix(1:5, nrow=5, byrow=T), heights=c(3,3,3,3,3.5))
par(mar=c(3,4,2,1))



# a Ice -------------------------
D = as.POSIXlt(FrozenQ$Date)
Q = FrozenQ$X_00060_00003

plot(D, Q/1e4, type="l", 
     xlab="", ylab="Q (cms)", 
     las=1, lwd=1.5,col="blue")
abline(h=0, lty=1, col=rgb(1,1,1,0.8))
abline(h=0, lty=2)
axis.POSIXct(1, D, format="%Y", padj=1.8)
legend("topleft", "2.1 Frozen surface water", border=F, bty="n", text.font=2)
mtext("(a)", padj=-2, adj=0, outer=F, cex=0.65, font=2)
mtext(expression(10^4), adj=0, outer=F, cex=0.65)

# b Flow Reversal ---------------------------------
D = as.POSIXlt(FlowReverseQ$Date)
Q = FlowReverseQ$X_00060_00003
plot(D, Q, type="l", 
     ylim=c(min(Q), 1.5),
     xlab="", ylab="Q (cms)", 
     las=1, lwd=1.5, col="blue")
abline(h=0, lty=1, col=rgb(1,1,1,0.8))
abline(h=0, lty=2)
axis.POSIXct(1, D, format="%Y", padj=1.8)
legend("topleft", "2.2 Flow reversal", border=F, bty="n", text.font=2)
mtext("(b)", padj=-2, adj=0, outer=F, cex=0.65, font=2)
mtext(expression(10^0), adj=0, outer=F, cex=0.65)


# c Data/equipment error ---------------------------
D = as.POSIXlt(DataErrorQ$dateTime)
Q = DataErrorQ$Flow_Inst
plot(D, Q/10^3, type="l", main="",
     xlab="", ylab="Q (cms)", 
     ylim=c(0, 1.5), #xaxt="n",
     lwd=1.5, col="blue", las=1)
abline(h=0, lty=1, col=rgb(1,1,1,0.8))
abline(h=0, lty=2)
axis.POSIXct(1, D, format="%Y", padj=1.8)
legend("topleft", "2.3 Data/equipment error", border=F, bty="n", text.font=2)
mtext("(c)", padj=-2, adj=0, outer=F, cex=0.65, font=2)
mtext(expression(10^3), adj=0, outer=F, cex=0.65)

# d Anthro Upstream Surface water loss from diverting around a dam
D = as.POSIXlt(Milner$Timestamp..UTC.07.00.)
Q = Milner$Q_cms
plot(D, Q, type="l", main="" ,
     ylim=c(min(Q), 120),
     xlab="", ylab="Q (cms)", 
     las=1, lwd=1.5, col="blue")
abline(h=0, lty=1, col=rgb(1,1,1,0.8))
abline(h=0, lty=2)
axis.POSIXct(1, D, format="%Y", padj=1.8)
legend("topleft", "2.4b Surface water withdrawals", border=F, bty="n", text.font=2)
mtext("(d)", padj=-2, adj=0, outer=F, cex=0.65, font=2)
mtext(expression(10^0), adj=0, outer=F, cex=0.65)

# e Anthro Groundwater Pumping Removes flow  ---------------------------
par(mar=c(6,4,2,1))
D = as.POSIXlt(AnthroGWQ$Date)
Q = AnthroGWQ$X_00060_00003
plot(D, Q*1e2, type="l", main="" ,
     xlab="Date", ylab="Q (cms)", 
     col="blue", las=1, lwd=1.5)
abline(h=0, lty=1, col=rgb(1,1,1,0.8))
abline(h=0, lty=2)
axis.POSIXct(1, D, format="%Y", padj=1.8)
legend("topleft", "2.4b Groundwater pumping", border=F, bty="n", text.font=2)
mtext("(e)", padj=-2, adj=0, outer=F, cex=0.65, font=2)
mtext(expression(10^1), adj=0, outer=F, cex=0.65)



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



