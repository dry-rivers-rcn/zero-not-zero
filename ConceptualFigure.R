#------------------------------------------------------------------
# Download and plot USGS data for zero-not-zero conceptual figure
# Kendra Kaiser August 14th, 2019
#------------------------------------------------------------------

library(dataRetrieval)
library(tidyverse)

#set working directory to source folder - not sure how to automate this

# -------- Load non- USGS data --------
#Anthro Upstream Surface water loss
Milner<- read.csv("MilnerID_Data.csv", header = TRUE, stringsAsFactors =FALSE) #13087505 Milner Lwr Pwr Plant at Milner
Milner[,1]<-as.Date(Milner[,1], "%m/%d/%y %H:%M")


frozen<-"15896000" # Kuparuk R, Deadhorse, AK; alt SF Grand River near Cash, SD  06356500
FrozStart<- as.Date("2018-10-01") #"2017-09-01"
FrozEnd <- as.Date("2109-06-01") #"2018-07-01

FlowReverse<- "04087170" #Milwaukee
ReverseStart<-as.Date("2018-12-15")
ReverseEnd<-as.Date("2019-04-01")

IsolatedPool<- "09512500" #Agua Fria River nr Mayer, AZ
IsoStart<-as.Date("2019-06-01") 
IsoEnd<-as.Date("2019-08-01") 

SourceLoss<- "" #Woodforde, Australia -- need data for importing
SourceStart<-as.Date("")
SourceEnd<-as.Date("")

AnthroSourceLoss<- ""
ASourceStart<-as.Date("")
ASourceEnd<-as.Date("")

UpstreamLoss<- "06879650" #kings Creek nr Manhattan, KS
UpStart<-as.Date("") #"2018-12-01 - when you zoom in it's actually at 1 cfs - better time-frame?
UpEnd<-as.Date("") #"2018-07-01

AnthroUpstreamLossGW<- ""  # Arkansas River near Dodge City - need the correct USGS gauge number
AUpStart<-as.Date("") #2000-10-01 ?? 
AUpEnd<-as.Date("") #2001-10-01

AnthroUpstreamLossSW<- ""  # Milner stretch on the Snake
AswUpStart<-as.Date("") #
AswUpEnd<-as.Date("") #

Bypass<- "" #bypass through karst Palanisamy and Workman 2015 - need data or gage number
BypassStart<-as.Date("")
BypassEnd<-as.Date("")

AnthroBypassA<- "08358400" #Rio Grand Floodway
AnthroBypassB<- "08358300" #Rio Grand Conveyance
ABypassStart<-as.Date("") #1991-06-01 Any more recent data that shows this? 
ABypassEnd<-as.Date("") #1991-09-01

