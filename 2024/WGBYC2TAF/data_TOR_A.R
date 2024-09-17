## Preprocess data, write TAF data tables

## Before: the data
## After: the data ready:-)

library(icesTAF)
#some packages
library(dplyr)
library(tidyr)

mkdir("data")

## TOR A example
print("TOR A")
#read the data
D1<-read.taf(taf.data.path("D1.csv"))
D2<-read.taf(taf.data.path("D2.csv"))
D3<-read.taf(taf.data.path("D3.csv"))
# read and simplify country codes tables
ctrycodes<-read.taf(taf.data.path("ctrycodes.csv")) %>%
	transmute(country=Key,ctryname=tolower(Description))%>%
	mutate(ctryname=paste0(toupper(substr(ctryname,1,1)),substr(ctryname,2,999)))%>%
	mutate(ctryname=ifelse(ctryname=="United kingdom","United Kingdom",ctryname))%>%
	distinct()
# read and simplify country codes tables
monmeth<-read.taf(taf.data.path("monmeth.csv")) %>%
	transmute(monitoringMethod=Key,monmethname=Description)%>%
	distinct()
# read and simplify country codes tables
gearcodes<-read.taf(taf.data.path("gearcodes.csv")) %>%
	transmute(metierL3=Key,L3name=Description)%>%
	distinct()

#add plain country names and monitoring method
D1<-D1%>%
	left_join(ctrycodes,by="country")%>%
	left_join(gearcodes,by="metierL3")%>%
	filter(year%in%2017:2023)
D2<-D2%>%
	left_join(ctrycodes,by="country")%>%
	left_join(gearcodes,by="metierL3")%>%
	left_join(monmeth,by="monitoringMethod")%>%
	filter(year%in%2017:2023)
D3<-D3%>%
	left_join(ctrycodes,by="country")%>%
	left_join(gearcodes,by="metierL3")%>%
	filter(year%in%2017:2023)

print("Monitoring method overview")
# prepare data for fig 3.3 (reported monitoring method)
#all
monmethall<-D2%>%group_by(monmethname,year)%>%
	summarise(das=sum(daysAtSeaOb,na.rm=T),.groups="drop") %>% 
	group_by(year)%>%
	mutate(tot=sum(das))%>%
	ungroup()%>%
	mutate(reldas=das/tot*100)
#by ctry
monmethctry<-D2%>%group_by(monmethname,ctryname,year)%>%
	summarise(das=sum(daysAtSeaOb,na.rm=T),.groups="drop") %>%
	group_by(year,ctryname)%>%
	mutate(tot=sum(das))%>%
	ungroup()%>%
	mutate(reldas=das/tot*100)

#save the data
write.taf(monmethall,dir="data")
write.taf(monmethctry,dir="data")

#prepare data for sample coverage by monitoring effort by gear by georegions
monmethgear<-D2%>%group_by(monmethname,ecoregion,L3name,year)%>%
	summarise(das=sum(daysAtSeaOb,na.rm=T),.groups="drop") 
write.taf(monmethgear,dir="data")


