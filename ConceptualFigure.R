#------------------------------------------------------------------
# Download and plot USGS data for zero-not-zero conceptual figure
# Kendra Kaiser August 14th, 2019
#------------------------------------------------------------------

library(dataRetrieval)
library(tidyverse)
pCode <- "00060"

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
ReverseStart<-as.Date("2018-12-15")
ReverseEnd<-as.Date("2019-04-01")

FlowReverseQ <- readNWISuv(siteNumbers = FlowReverse, parameterCd = pCode, startDate = ReverseStart, endDate = ReverseEnd) %>% renameNWISColumns() %>% data.frame

#----- Data/equipment error ---------------------------
DataError<- "01646500" #potomac river near wash DC
DataErrorStart<-as.Date("2019-06-24")
DataErrorEnd<-as.Date("2019-07-02")

#-----Naturally occuring isolated pools --------------
IsolatedPool<- "09512500" #Agua Fria River nr Mayer, AZ
IsoStart<-as.Date("2019-06-01") 
IsoEnd<-as.Date("2019-08-01") 

IsoQ <- readNWISuv(siteNumbers = FlowReverse, parameterCd = pCode, startDate = IsoStart, endDate = IsoEnd) %>% renameNWISColumns() %>% data.frame

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
AgwEnd<-as.Date("2019-09-15") 

AnthroGWQ <- readNWISuv(siteNumbers = AnthroGW, parameterCd = pCode, startDate = AgwStart, endDate = AgwEnd) %>% renameNWISColumns() %>% data.frame

#-----Conveyance around gauge -----------------
AnthroConveyanceA<- "08358400" #Rio Grande Floodway
AnthroConveyanceB<- "08358300" #Rio Grande Conveyance
AConvStart<-as.Date("2005-08-01")
AConvEnd<-as.Date("2005-11-01") 

AnthroConveyanceAQ <- readNWISuv(siteNumbers = AnthroConveyanceA, parameterCd = pCode, startDate = AConvStart, endDate = AConvEnd) %>% renameNWISColumns() %>% data.frame

AnthroConveyanceBQ <- readNWISuv(siteNumbers = AnthroConveyanceB, parameterCd = pCode, startDate = AConvStart, endDate = AConvEnd) %>% renameNWISColumns() %>% data.frame


#-----------Plotting----------------------------

plot(FrozenQ$dateTime, FrozenQ$Flow_Inst, type="l", 
     col="blue", xlab="Date", ylab="Discharge (cfs)")
axis.POSIXct(1, FrozenQ$dateTime, format="%b")
axis.POSIXct(1, FrozenQ$dateTime, format="%Y", padj=1.8)

plot(FlowReverseQ$dateTime, FlowReverseQ$Flow_Inst, type="l", 
     col="blue", xlab="Date", ylab="Discharge (cfs)")
axis.POSIXct(1, FlowReverseQ$dateTime, format="%b")
axis.POSIXct(1, FlowReverseQ$dateTime, format="%Y", padj=1.8)

plot(IsolatedPoolQ$dateTime, IsolatedPoolQ$Flow_Inst, type="l", 
     col="blue", xlab="Date", ylab="Discharge (cfs)")
axis.POSIXct(1, IsolatedPoolQ$dateTime, format="%b")
axis.POSIXct(1, IsolatedPoolQ$dateTime, format="%Y", padj=1.8)

plot(AnthroGWQ$dateTime, AnthroGWQ$Flow_Inst, type="l", 
     col="blue", xlab="Date", ylab="Discharge (cfs)")
axis.POSIXct(1, AnthroGWQ$dateTime, format="%b")
axis.POSIXct(1, AnthroGWQ$dateTime, format="%Y", padj=1.8)

plot(AnthroConveyanceAQ$dateTime, AnthroConveyanceAQ$Flow_Inst, type="l", 
     col="blue", xlab="Date", ylab="Discharge (cfs)")
axis.POSIXct(1, AnthroConveyanceAQ$dateTime, format="%b")
axis.POSIXct(1, AnthroConveyanceAQ$dateTime, format="%Y", padj=1.8)

plot(AnthroConveyanceBQ$dateTime, AnthroConveyanceBQ$Flow_Inst, type="l", 
     col="blue", xlab="Date", ylab="Discharge (cfs)")
axis.POSIXct(1, AnthroConveyanceBQ$dateTime, format="%b")
axis.POSIXct(1, AnthroConveyanceBQ$dateTime, format="%Y", padj=1.8)
