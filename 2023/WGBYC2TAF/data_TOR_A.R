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
	filter(year%in%2017:2022)
D2<-D2%>%
	left_join(ctrycodes,by="country")%>%
	left_join(gearcodes,by="metierL3")%>%
	left_join(monmeth,by="monitoringMethod")%>%
	filter(year%in%2017:2022)
D3<-D3%>%
	left_join(ctrycodes,by="country")%>%
	left_join(gearcodes,by="metierL3")%>%
	filter(year%in%2017:2022)

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


#green table
print("green table")
greentableD1<-D1%>%
	group_by(ctryname,year)%>%
	summarise(n=n())%>%
	mutate(year=paste0("D1_",year))%>%
	tidyr::pivot_wider(values_from=n,names_from=year,names_sort=T)
greentableD2<-D2%>%
	group_by(ctryname,year)%>%
	summarise(n=n())%>%
	mutate(year=paste0("D2_",year))%>%
	tidyr::pivot_wider(values_from=n,names_from=year,names_sort=T)
greentableD3<-D3%>%
	group_by(ctryname,year)%>%
	summarise(n=n())%>%
	mutate(year=paste0("D3_",year))%>%
	tidyr::pivot_wider(values_from=n,names_from=year,names_sort=TRUE)
greentable<-full_join(full_join(greentableD1,greentableD2),greentableD3)%>%
	ungroup()%>%data.frame()

#greentable bis check if at least one values is present in "important field"
# daysAtSeaF : at least 1 value not empty
greentableD1check<-D1%>%
	group_by(ctryname,year)%>%
	summarise(na1=sum(daysAtSeaF))%>%
	ungroup()%>%
	mutate(year=paste0("D1_",year)) %>%
	tidyr::pivot_wider(values_from=na1,names_from=year,names_sort=T,values_fill=-9)
# daysAtSeaOb : at least 1 value not empty
greentableD2check<-D2%>%
	group_by(ctryname,year)%>%
	summarise(na1=sum(daysAtSeaOb))%>%
	ungroup()%>%
	mutate(year=paste0("D2_",year)) %>%
	tidyr::pivot_wider(values_from=na1,names_from=year,names_sort=T,values_fill=-9)
# at least numerical value (zero included) in incidents column
greentableD3check<-D3%>%
	mutate(nbinc=sum(incidentsWithPingers,incidentsWithoutPingers,na.rm=T))%>%
	group_by(ctryname,year)%>%
	summarise(nbinc=sum(nbinc))%>%
	ungroup()%>%
	mutate(year=paste0("D3_",year)) %>%
	tidyr::pivot_wider(values_from=nbinc,names_from=year,names_sort=T,values_fill=-9)
greentablecheck<-full_join(full_join(greentableD1check,greentableD2check),greentableD3check)%>%
	ungroup()%>%data.frame()


