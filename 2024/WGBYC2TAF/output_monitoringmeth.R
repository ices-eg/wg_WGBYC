## Monitoring method plot

#source("data.R")

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


#plt output here
#monitoring methods overview
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

#monitoring methods: ecoregion/gear view
monmethgear1<-D2%>%group_by(monmethname,ecoregion,L3name,year)%>%
	summarise(das=sum(daysAtSeaOb,na.rm=T),.groups="drop")%>%
	filter(!monmethname%in%c("Logbook","Port observers"))
monmethgear2<-monmethgear1%>%
	mutate(L3name="All metier")%>%
	group_by(monmethname,ecoregion,L3name,year)%>%
	summarise(das=sum(das,na.rm=T),.groups="drop")
monmethgear<-rbind(monmethgear1,monmethgear2)%>%
	mutate(monmethname=as.factor(monmethname))


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





