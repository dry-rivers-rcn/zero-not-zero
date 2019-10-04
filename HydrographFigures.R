#******************************************************************************
# Download and plot USGS data for zero-not-zero conceptual figure
# George H. Allen; Kendra Kaiser 
# Sept 26, 2019; 
#******************************************************************************
# Summary: 
# this code downloads USGS data and plots the hydrographs seen in Figure 3 of 
# Zimmer et al. WATER


#******************************************************************************
# load libraries and set wd:
#******************************************************************************
require(dataRetrieval)
require(tidyverse)
setwd("~/research/2019_09_20_DryRivRCN_workshop/git/zero-not-zero/")


#******************************************************************************
# Download data from USGS/read in data from local drive:
#******************************************************************************

# a Frozen gauge location
Frozen="15896000" # Kuparuk R, Deadhorse, AK; alt SF Grand River near Cash, SD  06356500
FrozStart= as.Date("2017-8-01") #"2017-09-01"
FrozEnd = as.Date("2018-7-31") #"2018-07-01
FrozenQ = readNWISdv(siteNumbers = Frozen, parameterCd = "00060", 
                     startDate = FrozStart, endDate = FrozEnd) 


# b Flow Reversal
FlowReverse = "04194085"
ReverseStart = as.Date("2012-05-01")
ReverseEnd = as.Date("2012-06-30")
FlowReverseQ = readNWISdv(siteNumbers = FlowReverse, parameterCd = "00060", 
                          startDate = ReverseStart, endDate = ReverseEnd)
FlowReverseQ$X_00060_00003 = FlowReverseQ$X_00060_00003*0.028316847


# c Data/equipment error
DataErrorStart = as.Date("2014-10-01") 
DataErrorEnd = as.Date("2019-09-30") 
DataErrorQ = read.csv("data-for-George.csv", header=T, stringsAsFactors=F
                      ) %>% renameNWISColumns() %>% data.frame
DataErrorQ$X101280_Flow_cd = DataErrorQ$X101280_Flow_cd*0.028316847


# d Natural source loss
NatLoss = "09512500"
NatLossStart = as.Date("2019-07-01")
NatLossEnd = as.Date("2019-08-01")
NatLossQ = readNWISuv(siteNumbers = NatLoss, parameterCd = "00060", 
                      startDate = NatLossStart, endDate = NatLossEnd
                      ) %>% renameNWISColumns() %>% data.frame
NatLossQ$Flow_Inst = NatLossQ$X101280_Flow_cd*0.028316847


# e Anthro Upstream Surface water loss from diverting around a dam - might not need this
# Load non- USGS data
MilnerStart = as.Date("2014-10-01") 
MilnerEnd = as.Date("2015-09-30") 
Milner = read.csv("MilnerID_Data.csv", header=T, stringsAsFactors=F) 
Milner[,1] = as.Date(Milner[,1], "%m/%d/%y %H:%M")
Milner = Milner[Milner$Timestamp..UTC.07.00. > MilnerStart & 
                  Milner$Timestamp..UTC.07.00. < MilnerEnd, ]
names(Milner)[2] = "Q_cms"
Milner$Q_cms = Milner$Q_cms*0.028316847


# f Anthro Groundwater Pumping Removes flow 
AnthroGW = "07139000"  # Arkansas River at Garden City 
AgwStart = as.Date("2018-05-01") 
AgwEnd = as.Date("2018-07-15") 

AnthroGWQ = readNWISdv(siteNumbers = AnthroGW, parameterCd = "00060", 
                       startDate = AgwStart, endDate = AgwEnd
)# %>% renameNWISColumns() %>% data.frame
AnthroGWQ$X_00060_00003 = AnthroGWQ$X_00060_00003*0.028316847



#******************************************************************************
# PLOT FIGURE 3
#******************************************************************************

# set up plot:
pdfOutPath = "Figure3.pdf"
pdf(pdfOutPath, width=7, height=4)

layout(matrix(1:6, nrow=2, byrow=T))# , heights=c(3,3,3,))
par(mar=c(3,4,2,1))

# a Ice
D = as.POSIXlt(FrozenQ$Date)
Q = FrozenQ$X_00060_00003

plot(D, Q/1e4, type="l", 
     ylim=c(0,7),
     xlab="", ylab="", 
     las=1, lwd=1.5,col="blue")
abline(h=0, lty=1, col=rgb(1,1,1,0.8))
abline(h=0, lty=2)
axis.POSIXct(1, D, format="%Y", padj=1.2)
legend("topleft", "2.1 Frozen surface water", border=F, bty="n", text.font=2)
mtext("(a)", padj=-2, adj=0, outer=F, cex=0.65, font=2)
mtext(expression(10^4), adj=0, outer=F, cex=0.65)
mtext(side=1, line=2, "", cex=0.65, font=2, padj=1.8)
mtext(side=2, line=2, "Discharge [m3/s]", cex=0.65, font=2, padj=-1)


# b Flow Reversal
D = as.POSIXlt(FlowReverseQ$Date)
Q = FlowReverseQ$X_00060_00003
plot(D, Q, type="l", 
     ylim=c(min(Q), 1.5),
     xlab="", ylab="", 
     las=1, lwd=1.5, col="blue")
abline(h=0, lty=1, col=rgb(1,1,1,0.8))
abline(h=0, lty=2)
axis.POSIXct(1, D, format="%Y", padj=1.2)
legend("topleft", "2.2 Flow reversal", border=F, bty="n", text.font=2)
mtext("(b)", padj=-2, adj=0, outer=F, cex=0.65, font=2)
#mtext(expression(10^0), adj=0, outer=F, cex=0.65)
mtext(side=1, line=2, "", cex=0.65, font=2, padj=1.8)
mtext(side=2, line=2, "", cex=0.65, font=2, padj=-1)


# c Data/equipment error
D_raw = as.POSIXlt(DataErrorQ$datetime.1, format="%m/%d/%Y %H:%M")
subInd = which(D_raw > as.POSIXlt("09/16/2019 00:00" , format="%m/%d/%Y %H:%M"))
D = as.POSIXlt(DataErrorQ$datetime.1, format="%m/%d/%Y %H:%M")[subInd]
Q = DataErrorQ$X101280_Flow_cd[subInd]
plot(D, Q*1e2, type="l", main="",
     xlab="", ylab="", 
     ylim=c(0, 3), #xaxt="n",
     lwd=1.5, col="blue", las=1)
abline(h=0, lty=1, col=rgb(1,1,1,0.8))
abline(h=0, lty=2)
axis.POSIXct(1, D, format="%Y", padj=1.2)
legend("topleft", "2.3 Data/equipment error", border=F, bty="n", text.font=2)
mtext("(c)", padj=-2, adj=0, outer=F, cex=0.65, font=2)
mtext(expression(10^-2), adj=0, outer=F, cex=0.65)
mtext(side=1, line=2, "", cex=0.65, font=2, padj=1.8)
mtext(side=2, line=2, "", cex=0.65, font=2, padj=-1)


# d 2.4a Natural upstream source loss or flow bypass 
D = as.POSIXlt(NatLossQ$dateTime)
Q = NatLossQ$Flow_Inst
plot(D, Q, type="l", main="",
     xlab="", ylab="", 
     ylim=c(0, 0.6), #xaxt="n",
     lwd=1.5, col="blue", las=1)
abline(h=0, lty=1, col=rgb(1,1,1,0.8))
abline(h=0, lty=2)
axis.POSIXct(1, D, format="%Y", padj=1.2)
legend("topleft", "2.4a Natural source loss", border=F, bty="n", text.font=2)
mtext("(d)", padj=-2, adj=0, outer=F, cex=0.65, font=2)
#mtext(expression(10^0), adj=0, outer=F, cex=0.65)
mtext(side=1, line=2, "Date", cex=0.65, font=2, padj=1.8)
mtext(side=2, line=2, "Discharge [m3/s]", cex=0.65, font=2, padj=-1)


# e Anthro Upstream Surface water loss from diverting around a dam
D = as.POSIXlt(Milner$Timestamp..UTC.07.00.)
Q = Milner$Q_cms
plot(D, Q, type="l", main="" ,
     ylim=c(min(Q), 110),
     xlab="", ylab="", 
     las=1, lwd=1.5, col="blue")
abline(h=0, lty=1, col=rgb(1,1,1,0.8))
abline(h=0, lty=2)
axis.POSIXct(1, D, format="%Y", padj=1.2)
legend("topleft", "2.4b Water withdrawals", border=F, bty="n", text.font=2)
mtext("(e)", padj=-2, adj=0, outer=F, cex=0.65, font=2)
#mtext(expression(10^0), adj=0, outer=F, cex=0.65)
#mtext(side=1, line=2, "Date", cex=0.65, font=2, padj=1.8)
mtext(side=2, line=2, "", cex=0.65, font=2, padj=-1)


# f Anthro Groundwater Pumping Removes flow
D = as.POSIXlt(AnthroGWQ$Date)
Q = AnthroGWQ$X_00060_00003
plot(D, Q, type="l", main="",
     ylim=c(min(Q), 0.4),
     xlab="", ylab="", 
     col="blue", las=1, lwd=1.5)
abline(h=0, lty=1, col=rgb(1,1,1,0.8))
abline(h=0, lty=2)
axis.POSIXct(1, D, format="%Y", padj=1.2)
legend("topleft", "2.4b Groundwater pumping", border=F, bty="n", text.font=2)
mtext("(f)", padj=-2, adj=0, outer=F, cex=0.65, font=2)
#mtext(expression(10^-2), adj=0, outer=F, cex=0.65)
#mtext(side=1, line=2, "Date", cex=0.65, font=2, padj=1.8)
mtext(side=2, line=2, "", cex=0.65, font=2, padj=-1)


dev.off()
cmd = paste('open', pdfOutPath)
system(cmd)


