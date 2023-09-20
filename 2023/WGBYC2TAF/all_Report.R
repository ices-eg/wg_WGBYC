## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)

mkdir("report")

library(ggplot2)
library(ggalluvial)
library(dplyr)
library(sf)

#tor A example
monmethall<-read.taf("data/monmethall.csv")
monmethctry<-read.taf("data/monmethctry.csv")


#figures
plt1<-ggplot(monmethall,aes(x=year,y=reldas,
			    stratum=monmethname,
			    alluvium=monmethname,
			    fill=monmethname,
			    label=monmethname))+
	geom_flow(stat="alluvium")+
	geom_stratum()+
	theme_bw()+
	labs(fill="Monitoring method")+
	ylab("Propotion of annual DAS by monitoring method (%)")
taf.png("monitoring_method_all.png")
print(plt1)
dev.off()

plt2<-ggplot(monmethctry,aes(x=year,y=reldas,
			    stratum=monmethname,
			    alluvium=monmethname,
			    fill=monmethname,
			    label=monmethname))+
	geom_flow(stat="alluvium")+
	geom_stratum()+
	facet_wrap(~ctryname)+
	theme_bw()+
	labs(fill="Monitoring method")+
	ylab("Propotion of annual DAS by monitoring method (%)")
taf.png("monitoring_method_ctry.png")
print(plt2)
dev.off()

#tor C
bycrate<-read.taf("data/bycrate.csv")

library(flextable)
tf <- tempfile(fileext = ".docx",tmpdir="report")

library(officer)
ft1 <- flextable(bycrate)
save_as_docx(ft1, path = tf)


#tor D
effmap<-readRDS("data/effmap.rds")

#simplify to speed up example
plt<-ggplot()+
	geom_sf(data=effmap%>%filter(year==2022)%>%slice(1:10),aes(fill=das),alpha=0.5)+
	#facet_grid(year~nameL3)
	facet_wrap(~nameL3)


taf.png("effort_map.png")
print(plt)
dev.off()
