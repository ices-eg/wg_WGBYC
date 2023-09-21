## Preprocess data, write TAF data tables

## Before: the data
## After: the data ready:-)

library(icesTAF)
#some packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)
library(flextable)
library(ftExtra)
library(officer)
require(openxlsx)
require(countrycode)


mkdir("data")

## TOR A example
print("TOR A")
#wgbyc effort
efbyc<-read.taf(taf.data.path("D1.csv"))
countrycode(unique(efbyc$country),"iso2c","iso3c")

#read effort
#read MIXFISH data
print(load("../../../data/WGMIXFISH/WGMIXFISH_effort_IE_FR_2018-2022_for_WGBYC.Rdata"))
efmix<-ef
rm(ef)
#read rdb data
efrdb<-read.csv("../../../data/RDB/RDB CE Effort 2009 2022.csv")
ctrylist<-data.frame(FlagCountry=sort(unique(efrdb$FlagCountry)),country=NA)
ctrylist$country<-countrycode(ctrylist$FlagCountry,"iso3c","iso2c")
ctrylist<-ctrylist%>%
	mutate(country=ifelse(FlagCountry=="ENG","GB",country),
	       country=ifelse(FlagCountry=="NIR","GB",country),
	       country=ifelse(FlagCountry=="SCT","GB",country),
	       country=ifelse(FlagCountry=="WLS","GB",country))
efrdb<-left_join(efrdb,ctrylist)
#HCE RBDES
efrdbes<-read.csv("../../../data/HCE RDBES/CommercialEffort.csv")

#try to select the right year
tot1<-efbyc%>%
	filter(year%in%2017:2022,grepl("27.",areaCode))%>%
	group_by(country,area=substr(areaCode,1,4),year)%>%
	summarise(dasbyc=sum(daysAtSeaF,na.rm=T),
		  nbbyc=sum(vesselsF,na.rm=T))
tot2<-efmix%>%
	filter(Year%in%2017:2022,grepl("27.",Area))%>%
	group_by(country=Country,area=substr(Area,1,4),year=Year)%>%
	summarise(dasmix=sum(DaysAtSea,na.rm=T),
		  nbmix=sum(NoVessels,na.rm=T))
tot3<-efrdb%>%
	filter(Year%in%2017:2022,grepl("27.",Area))%>%
	group_by(country,area=substr(Area,1,4),year=Year)%>%
	summarise(dasrdb=sum(as.numeric(DaysAtSea),na.rm=T),
		  nbrdb=NA)
tot4<-efrdbes%>%
	filter(CEyear%in%2017:2022,grepl("27.",CEarea))%>%
	group_by(country=CEvesselFlagCountry,area=substr(CEarea,1,4),CEyear)%>%
	summarise(dasrdbes=sum(CEofficialDaysAtSea,na.rm=T),
		  nbrdbes=sum(CEnumberOfUniqueVessels,na.rm=T))

totall<-full_join(full_join(full_join(tot1,tot2),tot3),tot4)%>%
	transmute(country,area,year,dasbyc,dasmix,dasrdb,dasrdbes)
rez1<-totall%>%
	tidyr::pivot_longer(4:7)%>%
	mutate(value=ifelse(value==0,NA,value))%>%
	filter(!is.na(country))

plt<-ggplot(rez1,aes(x=area,y=value,color=name,fill=name))+
	geom_bar(stat="identity",position=position_dodge())+
	facet_grid(country~year,scale="free")+
	scale_y_log10()+
	theme_bw()+
	theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1))+
	xlab("Total days at sea")
	
	
#monitoring methods overview
monmethall<-read.taf("data/monmethall.csv")
plt<-ggplot(monmethall,aes(x=year,y=reldas,
			    stratum=monmethname,
			    alluvium=monmethname,
			    fill=monmethname,
			    label=monmethname))+
	geom_flow(stat="alluvium")+
	geom_stratum()+
	theme_bw()+
	labs(fill="Monitoring method")+
	ylab("Proportion of annual DAS by monitoring method (%)")
taf.png("output/monitoring_method_overview.png")
print(plt)


wb<-createWorkbook()
addWorksheet(wb,1)
writeData(wb,1,totall)
saveWorkbook(wb,file="output/testeffort.xlsx",overwrite=T)



efmix%>%filter(Year==2022)%>%
	group_by(country=Country)%>%summarise(ef1=sum(DaysAtSea))%>%
	ungroup()
D1%>%filter(year==2022,country%in%c("FR","IE"),tblUploadID==155)%>%
	group_by(country)%>%summarise(ef2=sum(daysAtSeaF))%>%
	ungroup()
u1<-efmix%>%filter(Year==2022)%>%
	group_by(country=Country,area=substr(Area,1,6))%>%summarise(ef1=sum(DaysAtSea))%>%
	ungroup()
u2<-D1%>%filter(year==2022,country%in%c("FR","IE"),tblUploadID==187)%>%
	group_by(country,area=substr(areaCode,1,6))%>%summarise(ef2=sum(daysAtSeaF))%>%
	ungroup()
u12<-full_join(u1,u2)

u1
#pb effort fr
pipo<-D1%>%filter(country=="FR")
uu<-pipo%>%group_by(tblUploadID,year,quarter,month,areaType,areaCode,metierL3,metierL4,metierL5,metierL6,vesselLengthRange)%>%mutate(n=n())




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
# read and simplify monprog table
monprog<-read.taf(taf.data.path("monprog.csv")) %>%
	transmute(monitoringProgramType=Key,monprog=Description)%>%
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
	left_join(monprog,by="monitoringProgramType")%>%
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

#plt output here
#monitoring methods overview
monmethall<-read.taf("data/monmethall.csv")
plt<-ggplot(monmethall,aes(x=year,y=reldas,
			    stratum=monmethname,
			    alluvium=monmethname,
			    fill=monmethname,
			    label=monmethname))+
	geom_flow(stat="alluvium")+
	geom_stratum()+
	theme_bw()+
	labs(fill="Monitoring method")+
	ylab("Proportion of annual DAS by monitoring method (%)")
taf.png("output/monitoring_method_overview.png")
print(plt)
dev.off()

#monitoring methods: country view
monmethctry<-read.taf("data/monmethctry.csv")
#clean 
plt<-ggplot(monmethctry,aes(x=year,y=reldas,
			    stratum=monmethname,
			    alluvium=monmethname,
			    fill=monmethname,
			    label=monmethname))+
	geom_flow(stat="alluvium")+
	geom_stratum()+
	facet_wrap(~ctryname)+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
	labs(fill="Monitoring method")+
	ylab("Proportion of annual DAS by monitoring method (%)")
taf.png("output/monitoring_method_country.png")
print(plt)
dev.off()

#moniroting by program 
monprogctry<-D2%>%group_by(monprog,ctryname,year)%>%
	summarise(das=sum(daysAtSeaOb,na.rm=T),.groups="drop") %>%
	group_by(year,ctryname)%>%
	mutate(tot=sum(das))%>%
	ungroup()%>%
	mutate(reldas=das/tot*100)
write.taf(monprogctry,dir="data")
#a graph
plt<-ggplot(monprogctry,aes(x=year,y=reldas,
			    stratum=monprog,
			    alluvium=monprog,
			    fill=monprog,
			    label=monprog))+
	geom_flow(stat="alluvium")+
	geom_stratum()+
	facet_wrap(~ctryname)+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
	labs(fill="Monitoring method")+
	ylab("Proportion of annual DAS by monitoring program (%)")

taf.png("output/monitoring_program_country.png")
print(plt)
dev.off()


#prepare data for sample coverage by monitoring effort by gear by georegions
monmethgear<-D2%>%group_by(monmethname,ecoregion,L3name,year)%>%
	summarise(das=sum(daysAtSeaOb,na.rm=T),.groups="drop") 
write.taf(monmethgear,dir="data")

#monitoring methods: ecoregion/gear view
monmethgear<-read.taf("data/monmethgear.csv")
listecoregion<-na.omit(unique(monmethgear$ecoregion))
for(i in listecoregion){
	tmp<-monmethgear[monmethgear$ecoregion==i,]%>%
		filter(!is.na(L3name))
	plt<-ggplot(tmp,aes(x=year,y=das,
			    stratum=monmethname,
			    alluvium=monmethname,
			    fill=monmethname,
			    label=monmethname))+
	geom_flow(stat="alluvium")+
	geom_stratum()+
	facet_wrap(~L3name,scale="free")+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
	labs(fill="Monitoring method")+
	ggtitle(i)+
	ylab("Sum of annual DAS by monitoring method")
	filename<-paste0("output/monitoring_method_",gsub(" ","",i),".png")
	print(filename)
	taf.png(filename)
	print(plt)
	dev.off()
}

#prepare data for sample coverage by monitoring effort by georegions
monmethecoregion<-D2%>%group_by(monmethname,ecoregion,year)%>%
	summarise(das=sum(daysAtSeaOb,na.rm=T),.groups="drop") %>%
	filter(!is.na(ecoregion))
write.taf(monmethecoregion,dir="data")

	plt<-ggplot(monmethecoregion%>%mutate(ecoregion=sub(" ","\n",ecoregion))
		    ,aes(x=year,y=das,
			    stratum=monmethname,
			    alluvium=monmethname,
			    fill=monmethname,
			    label=monmethname))+
	geom_flow(stat="alluvium")+
	geom_stratum()+
	facet_wrap(~ecoregion,scale="free")+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
	labs(fill="Monitoring method")+
	ggtitle(i)+
	ylab("Sum of annual DAS by monitoring method")
	filename<-paste0("output/monitoring_method_ecoregion.png")
	print(filename)
	taf.png(filename)
	print(plt)
	dev.off()


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

#build up a submission table from old D123
subtableD1<-D1%>%
	group_by(ctryname,year)%>%
	summarise(n=paste0(sort(unique(submissionYear)),collapse=","))%>%
	mutate(year=paste0("D1_",year))%>%
	tidyr::pivot_wider(values_from=n,names_from=year,names_sort=T)
subtableD2<-D2%>%
	group_by(ctryname,year)%>%
	summarise(n=paste0(sort(unique(submissionYear)),collapse=","))%>%
	mutate(year=paste0("D2_",year))%>%
	tidyr::pivot_wider(values_from=n,names_from=year,names_sort=T)
subtableD3<-D3%>%
	group_by(ctryname,year)%>%
	summarise(n=paste0(sort(unique(submissionYear)),collapse=","))%>%
	mutate(year=paste0("D3_",year))%>%
	tidyr::pivot_wider(values_from=n,names_from=year,names_sort=T)
subtable<-full_join(full_join(subtableD1,subtableD2),subtableD3)%>%
	ungroup()%>%data.frame()




nametab<-names(greentable)[-1]
nametab<-paste0(nametab,"='",substr(nametab,4,7),"'",collapse=",")
nametab<-eval(parse(text=paste0("list(",nametab,")")))
nametab<-c(list(ctryname="Year of data"),nametab)
bgcolormat<-ifelse(greentable[,-1]<0.00001,"white","darkgreen")
charcolormat<-matrix("white",nrow=nrow(subtable),ncol=ncol(subtable)-1)
charcolormat["2023"==subtable[,-1]]<-"yellow"
#charcolormat["2021,2022"==subtable[,-1]]<-"yellow"
big_border<-fp_border(color="black", width = 2)
#remove number
greentable[is.na(greentable)]<-0

ftgreentable<-flextable(subtable)%>%
	bg(j=2:ncol(greentable),bg=bgcolormat)%>%
	color(j=2:ncol(greentable),color=charcolormat) %>%
	set_header_labels(values=nametab) %>%
	add_header_row(values=c("","Fishing Effort (D1 table)",
				"Monitoring Effort (D2 table)",
				"Bycatch Events (D3 table)"),colwidths=c(1,6,6,6))%>%
	theme_box()%>%
	align(align="center",part="header")%>%
	align(align="center",part="body")%>%
	vline(j=c(1,7,13),border=big_border)%>%
	fontsize(part="all",size=6)%>%
	autofit(add_w=0,add_h=0)
ftgreentable

#save as docx object ?
save_as_docx(ftgreentable,path='./output/greentable.docx',
	     pr_section=prop_section(
				page_size=page_size(orient="landscape"),
				page_margins=page_mar(bottom=0.1,top=.1,right=.1,left=.1,header=0,footer=0,gutter=0)
				)
		)

#big long useless table
#Annex_table
tab2<-D2%>%
	filter(year==2022)%>%
	group_by(ecoregion,areaCode,metierL3,monitoringMethod)%>%
	summarise(dasobs=sum(daysAtSeaOb,na.rm=T))%>%
	ungroup()
tab1<-D1%>%
	filter(year==2022)%>%
	group_by(ecoregion,metierL3,areaCode)%>%
	summarise(dastot=sum(daysAtSeaF,na.rm=T))%>%
	ungroup()
tab3<-D3%>%
	filter(year==2022)%>%
	group_by(ecoregion,areaCode,metierL3,monitoringMethod,species,classname)%>%
	summarise(nb=sum(individualsWithPingers,individualsWithoutPingers,na.rm=T),
		  inc=sum(incidentsWithPingers,incidentsWithoutPingers,na.rm=T))%>%
	ungroup()

tab123<-left_join(left_join(tab2,tab1),tab3)%>%filter(!is.na(species))%>%
	transmute(ecoregion,areaCode,metierL3,dastot,monitoringMethod,dasobs,
		  moncov=100*(dasobs/dastot),classname,species,nb,inc)%>%
	mutate(dasobs=ifelse(dasobs==0,NA,dasobs))%>%
	mutate(moncov=ifelse(dasobs==0,NA,moncov))%>%
	transmute(Ecoregion=ecoregion,
		  AreaCode=areaCode,
		  MetierL3=metierL3,
		  `Fishing Effort (das)`=dastot,
		  MonitoringMethod=monitoringMethod,
		  `Total Observed Effort (das)`=dasobs,
		  `Monitoring Coverage (%)`=moncov,
		  classname,Species=species,
		  Total_No_Specimens=nb,
		  Incidents=inc)

#write excel file
wb<-createWorkbook()
addWorksheet(wb,1)
writeData(wb,1,tab123)
saveWorkbook(wb,file="output/TOR_A_long_table.xlsx",overwrite=T)

#short table
tab2<-D2%>%
	filter(year==2022)%>%
	group_by(ecoregion,metierL3,monitoringMethod)%>%
	summarise(dasobs=sum(daysAtSeaOb,na.rm=T))%>%
	ungroup()
tab1<-D1%>%
	filter(year==2022)%>%
	group_by(ecoregion,metierL3)%>%
	summarise(dastot=sum(daysAtSeaF,na.rm=T))%>%
	ungroup()
tab3<-D3%>%
	filter(year==2022)%>%
	group_by(ecoregion,metierL3,monitoringMethod,species,classname)%>%
	summarise(nb=sum(individualsWithPingers,individualsWithoutPingers,na.rm=T),
		  inc=sum(incidentsWithPingers,incidentsWithoutPingers,na.rm=T))%>%
	ungroup()

tab123<-left_join(left_join(tab2,tab1),tab3)%>%filter(!is.na(species))%>%
	transmute(ecoregion,metierL3,dastot,monitoringMethod,dasobs,
		  moncov=100*(dasobs/dastot),classname,species,nb,inc)%>%
	mutate(dasobs=ifelse(dasobs==0,NA,dasobs))%>%
	mutate(moncov=ifelse(dasobs==0,NA,moncov))%>%
	transmute(Ecoregion=ecoregion,
		  MetierL3=metierL3,
		  `Fishing Effort (das)`=dastot,
		  MonitoringMethod=monitoringMethod,
		  `Total Observed Effort (das)`=dasobs,
		  `Monitoring Coverage (%)`=moncov,
		  classname,Species=species,
		  Total_No_Specimens=nb,
		  Incidents=inc)

#write excel file
wb<-createWorkbook()
addWorksheet(wb,1)
writeData(wb,1,tab123)
saveWorkbook(wb,file="output/TOR_A_short_table.xlsx",overwrite=T)


#shorter table
tab2<-D2%>%
	filter(year==2022)%>%
	group_by(ecoregion)%>%
	summarise(dasobs=sum(daysAtSeaOb,na.rm=T))%>%
	ungroup()
tab1<-D1%>%
	filter(year==2022)%>%
	group_by(ecoregion)%>%
	summarise(dastot=sum(daysAtSeaF,na.rm=T))%>%
	ungroup()
tab3<-D3%>%
	filter(year==2022)%>%
	mutate(classname=ifelse(is.na(classname),"Reptiles",classname))%>%
	group_by(Ecoregion=ecoregion,classname)%>%
	summarise(nbspp=n_distinct(species),
		  nb=sum(individualsWithPingers,individualsWithoutPingers,na.rm=T),
		  inc=sum(incidentsWithPingers,incidentsWithoutPingers,na.rm=T))%>%
	ungroup()%>%
	mutate(info=paste(paste0(inc," inc."),paste0(nb," ind."),paste0(nbspp," spp"),sep="/"))%>%
	select(Ecoregion,classname,info)%>%
	tidyr::pivot_wider(values_from=info,names_from=classname)

tab123<-left_join(tab1,tab2)%>%
	filter(!is.na(ecoregion))%>%
	transmute(Ecoregion=ecoregion,
		  `Fishing Effort (das)`=dastot,
		  `Total Observed Effort (das)`=dasobs,
		  `Monitoring Coverage (%)`=100*(dasobs/dastot))%>%
	left_join(tab3)


#write excel file
wb<-createWorkbook()
addWorksheet(wb,1)
writeData(wb,1,tab123)
saveWorkbook(wb,file="output/TOR_A_shorter_table.xlsx",overwrite=T)



#shorter table detailed effort
tab2<-D2%>%
	filter(year==2022)%>%
	group_by(ecoregion,L3name)%>%
	summarise(dasobs=sum(daysAtSeaOb,na.rm=T))%>%
	ungroup()
tab1<-D1%>%
	filter(year==2022)%>%
	group_by(ecoregion,L3name)%>%
	summarise(dastot=sum(daysAtSeaF,na.rm=T))%>%
	ungroup()
tab12<-full_join(tab1,tab2)%>%
	mutate(moncov=100*(dasobs/dastot))%>%
	mutate(moncov=ifelse(is.finite(moncov),moncov,0))%>%
	mutate(info=paste0(round(dastot,0)," das (",round(moncov,3),"% cov.)"))%>%
	select(ecoregion,L3name,info)
tab3<-D3%>%
	filter(year==2022)%>%
	group_by(ecoregion,L3name)%>%
	summarise(inc=sum(incidentsWithPingers,incidentsWithoutPingers,na.rm=T))%>%
	mutate(inc=ifelse(is.finite(inc),inc,0))%>%
	ungroup()
tab123<-full_join(tab12,tab3)%>%
	mutate(inc=ifelse(is.finite(inc),inc,0))%>%
	mutate(info=paste0(info," das/ ",inc," inc."))%>%
	select(ecoregion,L3name,info)%>%
	pivot_wider(values_from=info,names_from=L3name)%>%
	filter(!is.na(ecoregion))

#write excel file
wb<-createWorkbook()
addWorksheet(wb,1)
writeData(wb,1,tab123)
saveWorkbook(wb,file="output/TOR_A_cool_effort_shorter_table.xlsx",overwrite=T)


