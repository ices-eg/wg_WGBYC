#try to implement TAF stuff for WGBYC from scratch
library(TAF)

#working dir
setwd("../WGBYC2TAF")
#make TAF skeleton
#taf.skeleton()

#DOWNLOAD stuff
#see internal file 01_get_wgbyc_data.R
D1<-read.csv("../data/wgbycdb/D1.csv")
D2<-read.csv("../data/wgbycdb/D2.csv")
D3<-read.csv("../data/wgbycdb/D3.csv")
D4<-read.csv("../data/wgbycdb/D4.csv")
D5<-read.csv("../data/wgbycdb/D5.csv")

#write data
write.taf(D1,dir="boot/initial/data")
write.taf(D2,dir="boot/initial/data")
write.taf(D3,dir="boot/initial/data")
write.taf(D4,dir="boot/initial/data")
write.taf(D5,dir="boot/initial/data",quote=T)

stop()


#upload inital data
#draft.data(
#	   data.file=NULL,
#	   data.scripts="wgbyc_data.R",
#	   originator="WGBYC",
#	   title="WGBYC data download",
#	   file=TRUE,
#	   append=FALSE)

#add true D1,D2 and D3 table into TAAAAAFFF
draft.data(
	   data.files="D3.csv",
	   originator="Laurent 24/09/2025",
	   year=2025,
	   title="D3",
	   period="2025",
	   file=TRUE,
	   append=F
	   )

draft.data(
	   data.files="D2.csv",
	   originator="Laurent 24/09/2025",
	   year=2025,
	   title="D2",
	   period="2025",
	   file=TRUE,
	   append=T
	   )

draft.data(
	   data.files="D1.csv",
	   originator="Laurent 24/09/2025",
	   year=2025,
	   title="D1",
	   period="2025",
	   file=TRUE,
	   append=T
	   )

draft.data(
	   data.files="D4.csv",
	   originator="Laurent 24/09/2025",
	   year=2025,
	   title="D4",
	   period="2025",
	   file=TRUE,
	   append=T
	   )

draft.data(
	   data.files="D5.csv",
	   originator="Laurent 24/09/2025",
	   year=2025,
	   title="D5",
	   period="2025",
	   file=TRUE,
	   append=T
	   )

#ices vocab code
library(icesVocab)
ctrycodes <- getCodeList("ISO_3166")
write.taf(ctrycodes,dir="boot/initial/data",quote=T)
gearcodes <- getCodeList("GearGroup")
write.taf(gearcodes,dir="boot/initial/data")
monmeth<-getCodeList("BYC_MonMethod")
write.taf(monmeth,dir="boot/initial/data")
monprog<-getCodeList("BYC_MonProgr")
write.taf(monprog,dir="boot/initial/data")

draft.data(
	   data.files="monprog.csv",
	   originator="Laurent 24/09/2025",
	   year=2025,
	   title="monprog",
	   file=TRUE,
	   append=T
	   )

draft.data(
	   data.files="ctrycodes.csv",
	   originator="Laurent 24/09/2025",
	   year=2025,
	   title="ctrycodes",
	   file=TRUE,
	   append=T
	   )

draft.data(
	   data.files="gearcodes.csv",
	   originator="Laurent 24/09/2025",
	   year=2025,
	   title="gearcodes",
	   file=TRUE,
	   append=T
	   )

draft.data(
	   data.files="monmeth.csv",
	   originator="Laurent 18/09/2025",
	   year=2025,
	   title="monmeth",
	   file=TRUE,
	   append=T
	   )

#test script stuff



draft.data(
	   data.files="allgeo.rds",
	   originator="Laurent 18/09/2025",
	   year=2022,
	   title="allgeo",
	   file=TRUE,
	   append=T
	   )

draft.data(
	   data.files="reportTemplate.docx",
	   originator="Laurent 25/09/2025",
	   year=2025,
	   title="reportTemplate",
	   file=TRUE,
	   append=T
	   )

#run taf.boot
taf.boot() #process software and data
#make.all() # process data/model/output/report
