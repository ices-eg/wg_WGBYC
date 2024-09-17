## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)

mkdir("output")

library(ggplot2)
library(ggalluvial)
library(dplyr)

#TOR A output 


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
#saveRDS(plt,file="output/monitoring_method_overview.rds")

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
	ylab("Sum of annual DAS by monitoring method (%)")
	filename<-paste0("output/monitoring_method_",gsub(" ","",i),".png")
	print(filename)
	taf.png(filename)
	print(plt)
	dev.off()
}
