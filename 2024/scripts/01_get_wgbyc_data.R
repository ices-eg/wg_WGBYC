	#tokenize data exchange
	library(icesConnect) #ask a token using  ices_token
	# read the data from ices API
	library(httr)
	library(jsonlite)
	linkD1<-"https://bycatch.ices.dk/api/GetD1_Fishing_effort"
	linkD2<-"https://bycatch.ices.dk/api/GetD2_Bycatch_monitoring_effort"
	linkD3<-"https://bycatch.ices.dk/api/GetD3_BycatchEvent"
	linkD4<-"https://bycatch.ices.dk/api/GetOverviewSubmissionTable/2023"
	linkD5<-"https://bycatch.ices.dk/api/GetByCatchRoadMapListSpecies"
	# test 
	# options(icesSAG.use_token=T)
	resp1<-ices_get_jwt(linkD1,username="dubroca",verbose=T,quiet=F)  
	D1<-content(resp1,as="text")
	D1<-fromJSON(D1)
	resp2<-ices_get_jwt(linkD2,username="dubroca",verbose=T)  
	D2<-content(resp2,as="text")
	D2<-fromJSON(D2)
	resp3<-ices_get_jwt(linkD3,username="dubroca")  
	D3<-content(resp3,as="text")
	D3<-fromJSON(D3)
	resp4<-ices_get_jwt(linkD4,username="dubroca")  
	D4<-content(resp4,as="text")
	D4<-fromJSON(D4)
	resp5<-ices_get_jwt(linkD5,username="dubroca")  
	D5<-content(resp5,as="text")
	D5<-fromJSON(D5)

	write.csv(D1,file="../data/wgbycdb/D1.csv",row.names=F)
	write.csv(D2,file="../data/wgbycdb/D2.csv",row.names=F)
	write.csv(D3,file="../data/wgbycdb/D3.csv",row.names=F)
	write.csv(D4,file="../data/wgbycdb/D4.csv",row.names=F)
	write.csv(D5,file="../data/wgbycdb/D5.csv",row.names=F)
	

