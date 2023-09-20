## Preprocess data, write TAF data tables

## Before: the data
## After: the data ready:-)

library(icesTAF)
#some packages
library(dplyr)
library(tidyr)
library(sf)

mkdir("data")

## TOR A example
print("TOR A")
#prepare D2 for ploting obs type
D2<-read.taf(taf.data.path("D2.csv"))
# read and simplify country codes tables
ctrycodes<-read.taf(taf.data.path("ctrycodes.csv"))%>%
	transmute(country=Key,ctryname=Description)%>%
	distinct()
# read and simplify country codes tables
monmeth<-read.taf(taf.data.path("monmeth.csv")) %>%
	transmute(monitoringMethod=Key,monmethname=Description)%>%
	distinct()
#add plain country names and monitoring method
D2<-D2%>%
	left_join(ctrycodes)%>%
	left_join(monmeth)
#add plein monitoring method names

# prepare data for fig 3.3 (reported monitoring method)
#all
monmethall<-D2%>%group_by(monmethname,year)%>%
	summarise(das=sum(daysAtSeaOb,na.rm=T)) %>%
	group_by(year)%>%
	mutate(tot=sum(das))%>%
	ungroup()%>%
	mutate(reldas=das/tot*100)%>%
	filter(year%in%2017:2022)
#by ctry
monmethctry<-D2%>%group_by(monmethname,ctryname,year)%>%
	summarise(das=sum(daysAtSeaOb,na.rm=T)) %>%
	group_by(year,ctryname)%>%
	mutate(tot=sum(das))%>%
	ungroup()%>%
	mutate(reldas=das/tot*100)%>%
	filter(year%in%2017:2022)

#save the data
write.taf(monmethall,dir="data")
write.taf(monmethctry,dir="data")

#TOR C example
print("TOR C")
#prepare D3 for join
D3<-read.taf(taf.data.path("D3.csv"))
D2<-read.taf(taf.data.path("D2.csv"))
#quick and dirty
D3simple<-D3%>%
	mutate(i1=individualsWithPingers,
	       i2=individualsWithoutPingers)%>%
	mutate(i1=ifelse(i1<0,0,i1))%>%
	mutate(i2=ifelse(i2<0,0,i2))%>%
	mutate(i2=ifelse(i2>26,0,i2))%>%
	mutate(i=i1+i2)%>%
	left_join(D2)%>%
	group_by(ecoregion,metierL4,year,species)%>%
	summarise(n=sum(i))
D2simple<-D2%>%
	group_by(ecoregion,metierL4,year)%>%
	summarise(das=sum(daysAtSeaOb,na.rm=T))
bycrate<-full_join(D3simple,D2simple)%>%
	filter(year==2021,grepl("Delphinus",species))%>%
	mutate(species="xxx",bycatch_rate=n/das)
write.taf(bycrate,dir="data")


#tor D example
print("TOR D")
#map effort example
#read the data, add plain gear name, and spatial stuff
D1<-read.taf(taf.data.path("D1.csv"))
gearcodes<-read.taf(taf.data.path("gearcodes.csv")) %>%
	transmute(metierL3=Key,nameL3=Description)%>%
	distinct()

allgeo<-readRDS(taf.data.path("allgeo.rds")) %>%
	filter(!type%in%c("ices_rectangle","ports")) %>%
	#adapt GSA to wgbyc coding
	mutate(offname=ifelse(type=="gfcm_gsa",as.numeric(sub("GSA","",offname)),offname))%>%
	mutate(offname=ifelse(offname==111,11.1,offname))%>%
	mutate(offname=ifelse(offname==112,11.2,offname))

D1<-D1%>%left_join(gearcodes)
effmap<-D1%>%group_by(year,nameL3,areaType,areaCode)%>%
	summarise(das=sum(daysAtSeaF,na.rm=T))%>%
	ungroup()%>%
	filter(year%in%2017:2022)
#join with spatial stuff
effmap<-left_join(effmap,allgeo,by=c("areaCode"="offname"))%>%
	st_as_sf()
saveRDS(effmap,file="./data/effmap.rds")



