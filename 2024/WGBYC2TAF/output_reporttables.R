## WGBYC report table

source("data.R")

#big long useless table
#Annex_table
tab2<-D2%>%
	filter(year==2023)%>%
	group_by(ecoregion,areaCode,metierL3,monitoringMethod)%>%
	summarise(dasobs=sum(daysAtSeaOb,na.rm=T))%>%
	ungroup()
tab1<-D1%>%
	filter(year==2023)%>%
	group_by(ecoregion,metierL3,areaCode)%>%
	summarise(dastot=sum(daysAtSeaF,na.rm=T))%>%
	ungroup()
tab3<-D3%>%
	filter(year==2023)%>%
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
saveWorkbook(wb,file="output/TOR_A_long_table_bycatch_only.xlsx",overwrite=T)

#big long useless table with all info
#Annex_table
tab2<-D2%>%
	filter(year==2023)%>%
	group_by(ecoregion,areaCode,metierL3,monitoringMethod)%>%
	summarise(dasobs=sum(daysAtSeaOb,na.rm=T))%>%
	ungroup()
tab1<-D1%>%
	filter(year==2023)%>%
	group_by(ecoregion,metierL3,areaCode)%>%
	summarise(dastot=sum(daysAtSeaF,na.rm=T))%>%
	ungroup()
tab3<-D3%>%
	filter(year==2023)%>%
	group_by(ecoregion,areaCode,metierL3,monitoringMethod,species,classname)%>%
	summarise(nb=sum(individualsWithPingers,individualsWithoutPingers,na.rm=T),
		  inc=sum(incidentsWithPingers,incidentsWithoutPingers,na.rm=T))%>%
	ungroup()

tab123<-full_join(full_join(tab2,tab1),tab3)%>%#filter(!is.na(species))%>%
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
	filter(year==2023)%>%
	group_by(ecoregion,metierL3,monitoringMethod)%>%
	summarise(dasobs=sum(daysAtSeaOb,na.rm=T))%>%
	ungroup()
tab1<-D1%>%
	filter(year==2023)%>%
	group_by(ecoregion,metierL3)%>%
	summarise(dastot=sum(daysAtSeaF,na.rm=T))%>%
	ungroup()
tab3<-D3%>%
	filter(year==2023)%>%
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
	filter(year==2023)%>%
	group_by(ecoregion)%>%
	summarise(dasobs=sum(daysAtSeaOb,na.rm=T))%>%
	ungroup()
tab1<-D1%>%
	filter(year==2023)%>%
	group_by(ecoregion)%>%
	summarise(dastot=sum(daysAtSeaF,na.rm=T))%>%
	ungroup()
tab3<-D3%>%
	filter(year==2023)%>%
	mutate(classname=ifelse(is.na(classname),"Reptiles",classname))%>%
	group_by(Ecoregion=ecoregion,classname)%>%
	summarise(nbspp=n_distinct(species),
		  nb=sum(individualsWithPingers,individualsWithoutPingers,na.rm=T),
		  inc=sum(incidentsWithPingers,incidentsWithoutPingers,na.rm=T))%>%
	ungroup()%>%
	mutate(info=paste(paste0(inc," inc."),paste0(nb," ind."),paste0(nbspp," spp"),sep="/"))%>%
	select(Ecoregion,classname,info)%>%
	tidyr::pivot_wider(values_from=info,names_from=classname)

stop()

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
	filter(year==2023)%>%
	group_by(ecoregion,L3name)%>%
	summarise(dasobs=sum(daysAtSeaOb,na.rm=T))%>%
	ungroup()
tab1<-D1%>%
	filter(year==2023)%>%
	group_by(ecoregion,L3name)%>%
	summarise(dastot=sum(daysAtSeaF,na.rm=T))%>%
	ungroup()
tab12<-full_join(tab1,tab2)%>%
	mutate(moncov=100*(dasobs/dastot))%>%
	mutate(moncov=ifelse(is.finite(moncov),moncov,0))%>%
	mutate(info=paste0(round(dastot,0)," das (",round(moncov,3),"% cov.)"))%>%
	select(ecoregion,L3name,info)
tab3<-D3%>%
	filter(year==2023)%>%
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


#another plot for tor A
#nb of incident by classname and monitoring method
tab2<-D2%>%
	group_by(monmethname,year,monitoringMethod)%>%
	summarise(dasobs=sum(daysAtSeaOb,na.rm=T))%>%
	ungroup()
tab3<-D3%>%
	mutate(classname=ifelse(is.na(classname),"Reptiles",classname))%>%
	mutate(classname=ifelse(classname%in%c("Aves","Reptiles","Mammalia","Elasmobranchii"),classname,"Other"))%>%
	group_by(year,monitoringMethod,classname)%>%
	summarise(nbspp=n_distinct(species),
		  nb=sum(individualsWithPingers,individualsWithoutPingers,na.rm=T),
		  inc=sum(incidentsWithPingers,incidentsWithoutPingers,na.rm=T))%>%
	ungroup()
tab23<-full_join(tab2,tab3)%>%filter(!is.na(classname))
rez<-tab23%>%
	transmute(monmethname,year,classname,inc)
	plt<-ggplot(rez,aes(x=year,y=inc,
			    stratum=monmethname,
			    alluvium=monmethname,
			    fill=monmethname,
			    label=monmethname))+
	geom_flow(stat="alluvium")+
	geom_stratum()+
	facet_wrap(~classname,scale="free")+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
	labs(fill="Monitoring method")+
	ylab("Total number of incidents")

	filename<-paste0("output/monitoring_method_classname.png")
	print(filename)
	taf.png(filename)
	print(plt)
	dev.off()



#another plot for tor A
#nb of incident by classname and monitoring method by ecoregion
tab2<-D2%>%
	group_by(monmethname,year,monitoringMethod,ecoregion)%>%
	summarise(dasobs=sum(daysAtSeaOb,na.rm=T))%>%
	ungroup()
tab3<-D3%>%
	mutate(classname=ifelse(is.na(classname),"Reptiles",classname))%>%
	mutate(classname=ifelse(classname%in%c("Aves","Reptiles","Mammalia","Elasmobranchii"),classname,"Other"))%>%
	group_by(year,monitoringMethod,classname,ecoregion)%>%
	summarise(nbspp=n_distinct(species),
		  nb=sum(individualsWithPingers,individualsWithoutPingers,na.rm=T),
		  inc=sum(incidentsWithPingers,incidentsWithoutPingers,na.rm=T))%>%
	ungroup()
tab23<-full_join(tab2,tab3)%>%filter(!is.na(classname))
rez<-tab23%>%
	transmute(monmethname,year,classname,ecoregion,inc)%>%
	mutate(ecoregion=sub(" ","\n",ecoregion))

	plt<-ggplot(rez,aes(x=year,y=inc,
			    stratum=monmethname,
			    alluvium=monmethname,
			    fill=monmethname,
			    label=monmethname))+
	geom_flow(stat="alluvium")+
	geom_stratum()+
	facet_grid(ecoregion~classname,scale="free")+
	theme_bw()+
	scale_y_log10()+
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
	      strip.text.y = element_text(angle = 0,size=6))+
	labs(fill="Monitoring method")+
	ylab("Total number of incidents")

	filename<-paste0("output/monitoring_method_classname_ecoregion.png")
	print(filename)
	taf.png(filename)
	print(plt)
	dev.off()


