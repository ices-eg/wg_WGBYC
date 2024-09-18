## Preprocess data, write TAF data tables

## Before: the data
## After: the data ready:-)

#some packages
library(icesTAF)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)
library(flextable)
library(ftExtra)
library(officer)
require(openxlsx)



## TOR A example
print("TOR A")
#read the data
D1<-read.taf(taf.data.path("getD1/D1.csv"))
D2<-read.taf(taf.data.path("getD2/D2.csv"))
D3<-read.taf(taf.data.path("getD3/D3.csv"))

# read and simplify country codes tables
ctrycodes<-read.taf(taf.data.path("vocab/ctrycodes.csv")) %>%
	transmute(country=Key,ctryname=tolower(Description))%>%
	mutate(ctryname=paste0(toupper(substr(ctryname,1,1)),substr(ctryname,2,999)))%>%
	mutate(ctryname=ifelse(ctryname=="United kingdom","United Kingdom",ctryname))%>%
	distinct()
# read and simplify country codes tables
monmeth<-read.taf(taf.data.path("vocab/monmeth.csv")) %>%
	transmute(monitoringMethod=Key,monmethname=Description)%>%
	distinct()
# read and simplify country codes tables
gearcodes<-read.taf(taf.data.path("vocab/gearcodes.csv")) %>%
	transmute(metierL3=Key,L3name=Description)%>%
	distinct()
# read and simplify monprog table
monprog<-read.taf(taf.data.path("vocab/monprog.csv")) %>%
	transmute(monitoringProgramType=Key,monprog=Description)%>%
	distinct()

#add plain country names, monitoring method, time filter
D1<-D1%>%
	left_join(ctrycodes,by="country")%>%
	left_join(gearcodes,by="metierL3")%>%
	filter(year%in%2017:2023)
D2<-D2%>%
	left_join(ctrycodes,by="country")%>%
	left_join(gearcodes,by="metierL3")%>%
	left_join(monmeth,by="monitoringMethod")%>%
	left_join(monprog,by="monitoringProgramType")%>%
	filter(year%in%2017:2023)
D3<-D3%>%
	left_join(ctrycodes,by="country")%>%
	left_join(gearcodes,by="metierL3")%>%
	filter(year%in%2017:2023)
