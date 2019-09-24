#------------------------------------------------------------------
# Download and plot USGS data for zero-not-zero conceptual figure
# Kendra Kaiser August 14th, 2019
#kendra rocks
#------------------------------------------------------------------
# install.packages("dataRetrieval")
# install.packages("tidyverse")
library(dataRetrieval)
library(tidyverse)
pCode <- "00060"
setwd("~/Documents/GitRepos/zero-not-zero/Figures")

#if loading in data, need to set working directory to source folder - not sure how to automate this

# -------- Load non- USGS data --------
#Anthro Upstream Surface water loss from diverting around a dam - might not need this
#Milner<- read.csv("MilnerID_Data.csv", header = TRUE, stringsAsFactors =FALSE) #13087505 Milner Lwr Pwr Plant at Milner
#Milner[,1]<-as.Date(Milner[,1], "%m/%d/%y %H:%M")

#----- Frozen gauge location -------------------------
Frozen<-"15896000" # Kuparuk R, Deadhorse, AK; alt SF Grand River near Cash, SD  06356500
FrozStart<- as.Date("2018-10-01") #"2017-09-01"
FrozEnd <- as.Date("2109-06-01") #"2018-07-01

FrozenQ <- readNWISuv(siteNumbers = Frozen, parameterCd = pCode, startDate = FrozStart, endDate = FrozEnd) %>% renameNWISColumns() %>% data.frame

#----- Flow Reversal ---------------------------------
FlowReverse<- "04087170" #Milwaukee
ReverseStart<-as.Date("2019-01-15")
ReverseEnd<-as.Date("2019-03-15")

FlowReverseQ <- readNWISuv(siteNumbers = FlowReverse, parameterCd = pCode, startDate = ReverseStart, endDate = ReverseEnd) %>% renameNWISColumns() %>% data.frame

#----- Data/equipment error ---------------------------
DataError<- "01646500" #potomac river near wash DC
DataErrorStart<-as.Date("2019-06-24")
DataErrorEnd<-as.Date("2019-07-02")

DataErrorQ <- readNWISuv(siteNumbers = DataError, parameterCd = pCode, startDate = DataErrorStart, endDate = DataErrorEnd) %>% renameNWISColumns() %>% data.frame

#-----Naturally occuring isolated pools --------------
IsolatedPool<- "09512500" #Agua Fria River nr Mayer, AZ
IsoStart<-as.Date("2019-06-01") 
IsoEnd<-as.Date("2019-08-01") 

IsoQ <- readNWISuv(siteNumbers = IsolatedPool, parameterCd = pCode, startDate = IsoStart, endDate = IsoEnd) %>% renameNWISColumns() %>% data.frame

# ----- Karst Bypass around gauge ------------
Bypass<- "" #bypass through karst Palanisamy and Workman 2015 - need data or gage number
BypassStart<-as.Date("")
BypassEnd<-as.Date("")

#can probably remove this one
UpstreamLoss<- "06879650" #kings Creek nr Manhattan, KS
UpStart<-as.Date("") #"2018-12-01 - when you zoom in it's actually at 1 cfs - better time-frame?
UpEnd<-as.Date("") #"2018-07-01

#-------
#------ Groundwater Pumping Removes flow 
AnthroGW<- "07139000"  # Arkansas River at Garden City 
AgwStart<-as.Date("2019-05-01") 
AgwEnd<-as.Date("2019-07-15") 

AnthroGWQ <- readNWISuv(siteNumbers = AnthroGW, parameterCd = pCode, startDate = AgwStart, endDate = AgwEnd) %>% renameNWISColumns() %>% data.frame

#-----Conveyance around gauge -----------------
AnthroConveyanceA<- "08358400" #Rio Grande Floodway
AnthroConveyanceB<- "08358300" #Rio Grande Conveyance
AConvStart<-as.Date("2005-08-01")
AConvEnd<-as.Date("2005-11-01") 

AnthroConveyanceAQ <- readNWISuv(siteNumbers = AnthroConveyanceA, parameterCd = pCode, startDate = AConvStart, endDate = AConvEnd) %>% renameNWISColumns() %>% data.frame

AnthroConveyanceBQ <- readNWISuv(siteNumbers = AnthroConveyanceB, parameterCd = pCode, startDate = AConvStart, endDate = AConvEnd) %>% renameNWISColumns() %>% data.frame


#-----------Plotting----------------------------
pdf("Frozen.pdf", width = 3.5, height =4 )
plot(FrozenQ$dateTime, FrozenQ$Flow_Inst, main = "Frozen Gauge: Kuparuk River, Deadhorse, AK ", type="l", col="blue", xlab="Date", ylab="Discharge (cfs)", lwd=2, cex.main=0.75)
lines(FrozenQ$dateTime[FrozenQ$Flow_Inst <= 0.001], FrozenQ$Flow_Inst[FrozenQ$Flow_Inst <= 0.001], lty="dotted", col='white', lwd=3)
axis.POSIXct(1, FrozenQ$dateTime, format="%b")
axis.POSIXct(1, FrozenQ$dateTime, format="%Y", padj=1.8)
dev.off()

pdf("FlowReverse.pdf", width = 3.5, height =4 )
plot(FlowReverseQ$dateTime, FlowReverseQ$Flow_Inst, type="l", main="Flow Reversal: Milwaukee River, WI",
     col="blue", xlab="Date", ylab="Discharge (cfs)", lwd=1, cex.main=0.75)
lines(FlowReverseQ$dateTime[FlowReverseQ$Flow_Inst <= 0.001], FlowReverseQ$Flow_Inst[FlowReverseQ$Flow_Inst <= 0.001], lty="dotted", col='white', lwd=1)
axis.POSIXct(1, FlowReverseQ$dateTime, format="%b")
axis.POSIXct(1, FlowReverseQ$dateTime, format="%Y", padj=1.8)
dev.off()

pdf("DataError.pdf", width = 3.5, height =4 )
plot(DataErrorQ$dateTime, DataErrorQ$Flow_Inst, type="l", main="Equipment Error: Potomac River,           Washington, DC", col="blue", xlab="Date", ylab="Discharge (cfs)", lwd=2, cex.main=0.75)
lines(DataErrorQ$dateTime[DataErrorQ$Flow_Inst <= 0.001], DataErrorQ$Flow_Inst[DataErrorQ$Flow_Inst <= 0.001], lty="dotted", col='white', lwd=3)
axis.POSIXct(1, DataErrorQ$dateTime, format="%b")
axis.POSIXct(1, DataErrorQ$dateTime, format="%Y", padj=1.8)
dev.off()

pdf("UpstreamLoss.pdf", width = 3.5, height =4 )
plot(IsoQ$dateTime, IsoQ$Flow_Inst, type="l", main="Upstream Flow Loss: Agua Fria River, Mayer, AZ",
     col="blue", xlab="Date", ylab="Discharge (cfs)", lwd=2, cex.main=0.75)
lines(IsoQ$dateTime[IsoQ$Flow_Inst <= 0.001], IsoQ$Flow_Inst[IsoQ$Flow_Inst <= 0.001], lty="dotted", col='white', lwd=3)
axis.POSIXct(1, IsoQ$dateTime, format="%b")
axis.POSIXct(1, IsoQ$dateTime, format="%Y", padj=1.8)
dev.off()

pdf("AnthroGW.pdf", width = 3.5, height =4 )
plot(AnthroGWQ$dateTime, AnthroGWQ$Flow_Inst, type="l", main="Groundwater Pumping: Arkansas River, Garden City, AK" ,
     col="blue", xlab="Date", ylab="Discharge (cfs)", lwd=2, cex.main=0.75)
lines(AnthroGWQ$dateTime[AnthroGWQ$Flow_Inst <= 0.001], AnthroGWQ$Flow_Inst[AnthroGWQ$Flow_Inst <= 0.001], lty="dotted", col='white', lwd=3)
axis.POSIXct(1, AnthroGWQ$dateTime, format="%b")
axis.POSIXct(1, AnthroGWQ$dateTime, format="%Y", padj=1.8)
dev.off()

pdf("AnthroBypass.pdf", width = 3.5, height =4 )
plot(AnthroConveyanceAQ$dateTime, AnthroConveyanceAQ$Flow_Inst, type="l", main= "Gauge Bypass, Rio Grande Floodway",
     col="blue", xlab="Date", ylab="Discharge (cfs)", lwd=2, cex.main=0.75)
lines(AnthroConveyanceAQ$dateTime[AnthroConveyanceAQ$Flow_Inst <= 0.001], AnthroConveyanceAQ$Flow_Inst[AnthroConveyanceAQ$Flow_Inst <= 0.001], lty="dotted", col='white', lwd=3)
axis.POSIXct(1, AnthroConveyanceAQ$dateTime, format="%b")
axis.POSIXct(1, AnthroConveyanceAQ$dateTime, format="%Y", padj=1.8)
dev.off()

pdf("AnthroBypassChan.pdf", width = 3.5, height =4 )
plot(AnthroConveyanceBQ$dateTime, AnthroConveyanceBQ$Flow_Inst, type="l", 
     col="blue", xlab="Date", ylab="Discharge (cfs)")
axis.POSIXct(1, AnthroConveyanceBQ$dateTime, format="%b")
axis.POSIXct(1, AnthroConveyanceBQ$dateTime, format="%Y", padj=1.8)
dev.off()



#-----------Plotting all on one page----------------------------

pdfOutPath = "Figure3.pdf"
pdf(pdfOutPath, width = 7.5, height = 7.5 )

layout(matrix(1:6, nrow=3, byrow=T))
par(mar=c(4,4,1,1))
FlowReverseQ$Flow_Inst = FlowReverseQ$Flow_Inst*0.028316847
plot(FrozenQ$dateTime, FrozenQ$Flow_Inst, 
     type="l", col="blue", xlab="", ylab="Q (cms)", 
     lwd=2, cex.main=0.75)
lines(FrozenQ$dateTime[FrozenQ$Flow_Inst <= 0.001],
      FrozenQ$Flow_Inst[FrozenQ$Flow_Inst <= 0.001], 
      lty="dotted", col='white', lwd=3)
axis.POSIXct(1, FrozenQ$dateTime, format="%Y", padj=1.8)

FlowReverseQ$Flow_Inst = FlowReverseQ$Flow_Inst*0.028316847
plot(FlowReverseQ$dateTime, FlowReverseQ$Flow_Inst, type="l", 
     col="blue", xlab="", ylab="", lwd=1, cex.main=0.75)
lines(FlowReverseQ$dateTime[FlowReverseQ$Flow_Inst <= 0], 
      FlowReverseQ$Flow_Inst[FlowReverseQ$Flow_Inst <= 0], 
      col='red', lwd=1)
axis.POSIXct(1, FlowReverseQ$dateTime, format="%Y", padj=1.8)


DataErrorQ$Flow_Inst = DataErrorQ$Flow_Inst*0.028316847
plot(DataErrorQ$dateTime, DataErrorQ$Flow_Inst, type="l", main="", 
     col="blue", xlab="", ylab="Q (cms)", lwd=2, cex.main=0.75)
# lines(DataErrorQ$dateTime[DataErrorQ$Flow_Inst <= 0.001], 
#       DataErrorQ$Flow_Inst[DataErrorQ$Flow_Inst <= 0.001], lty="dotted", col='white', lwd=3)
axis.POSIXct(1, DataErrorQ$dateTime, format="%Y", padj=1.8)


IsoQ$Flow_Inst = IsoQ$Flow_Inst*0.028316847
plot(IsoQ$dateTime, IsoQ$Flow_Inst, type="l", main="",
     col="blue", xlab="", ylab="", lwd=2, cex.main=0.75)
# lines(IsoQ$dateTime[IsoQ$Flow_Inst <= 0.001], IsoQ$Flow_Inst[IsoQ$Flow_Inst <= 0.001], 
#       lty="dotted", col='white', lwd=3)
axis.POSIXct(1, IsoQ$dateTime, format="%Y", padj=1.8)


AnthroGWQ$Flow_Inst = AnthroGWQ$Flow_Inst*0.028316847
plot(AnthroGWQ$dateTime, AnthroGWQ$Flow_Inst, type="l", main="" ,
     col="blue", xlab="Date", ylab="Q (cms)", lwd=2, cex.main=0.75)
# lines(AnthroGWQ$dateTime[AnthroGWQ$Flow_Inst <= 0.001], AnthroGWQ$Flow_Inst[AnthroGWQ$Flow_Inst <= 0.001], lty="dotted", col='white', lwd=3)
axis.POSIXct(1, AnthroGWQ$dateTime, format="%Y", padj=1.8)


AnthroConveyanceAQ$Flow_Inst = AnthroConveyanceAQ$Flow_Inst*0.028316847
plot(AnthroConveyanceAQ$dateTime, AnthroConveyanceAQ$Flow_Inst, type="l", main= "",
     col="blue", xlab="Date", ylab="", lwd=2, cex.main=0.75)
# lines(AnthroConveyanceAQ$dateTime[AnthroConveyanceAQ$Flow_Inst <= 0.001], AnthroConveyanceAQ$Flow_Inst[AnthroConveyanceAQ$Flow_Inst <= 0.001], lty="dotted", col='white', lwd=3)
axis.POSIXct(1, AnthroConveyanceAQ$dateTime, format="%Y", padj=1.8)


# plot(AnthroConveyanceBQ$dateTime, AnthroConveyanceBQ$Flow_Inst, type="l", 
#      col="blue", xlab="Date", ylab="Q (cms)")
# axis.POSIXct(1, AnthroConveyanceBQ$dateTime, format="%b")
# axis.POSIXct(1, AnthroConveyanceBQ$dateTime, format="%Y", padj=1.8)

dev.off()
cmd = paste('open', pdfOutPath)
system(cmd)
