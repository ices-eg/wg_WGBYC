	
#https://datsu.ices.dk/web/rptChk.aspx?Dataset=128

#tokenize data exchange




require(icesConnect)
icesConnect::set_username("laurent.dubroca")
icesConnect:::token_set_from_keyring("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJodHRwOi8vc2NoZW1hcy54bWxzb2FwLm9yZy93cy8yMDA1LzA1L2lkZW50aXR5L2NsYWltcy9uYW1lIjoibGF1cmVudC5kdWJyb2NhQGlmcmVtZXIuZnIiLCJqdGkiOiJlMjVmZjYyNi02OWM4LTRjY2YtYWM1MS05ZDYwMjY5MmVhNjEiLCJodHRwOi8vc2NoZW1hcy54bWxzb2FwLm9yZy93cy8yMDA1LzA1L2lkZW50aXR5L2NsYWltcy9lbWFpbGFkZHJlc3MiOiJsYXVyZW50LmR1YnJvY2FAaWZyZW1lci5mciIsIlVzZXJFbWFpbCI6ImxhdXJlbnQuZHVicm9jYUBpZnJlbWVyLmZyIiwiRW1haWwiOiJsYXVyZW50LmR1YnJvY2FAaWZyZW1lci5mciIsImV4cCI6MTc3ODY2NDI2MSwiaXNzIjoiaHR0cDovL3RhZi5pY2VzLmRrIiwiYXVkIjoiaHR0cDovL3RhZi5pY2VzLmRrIn0.71SWhBf6TZ4VugrjEXcplYbUzyALQaHb5LyHLr7Vk9k",username="laurent.dubroca")



# read the data from ices API
	library(httr)
	library(jsonlite)
	linkD1<-"https://bycatch.ices.dk/api/GetD1_Fishing_effort"
	linkD2<-"https://bycatch.ices.dk/api/GetD2_Bycatch_monitoring_effort"
	linkD3<-"https://bycatch.ices.dk/api/GetD3_BycatchEvent"
	linkD4<-"https://bycatch.ices.dk/api/GetOverviewSubmissionTable/2025"
	linkD5<-"https://bycatch.ices.dk/api/GetByCatchRoadMapListSpecies"
	# test 
	options(icesSAG.use_token=T)
	resp1<-ices_get_jwt(linkD1,username="laurent.dubroca",verbose=T,quiet=F)  
	D1<-content(resp1,as="text")
	D1<-fromJSON(D1)
	resp2<-ices_get_jwt(linkD2,username="laurent.dubroca",verbose=T)  
	D2<-content(resp2,as="text")
	D2<-fromJSON(D2)
	resp3<-ices_get_jwt(linkD3,username="laurent.dubroca")  
	D3<-content(resp3,as="text")
	D3<-fromJSON(D3)
	resp4<-ices_get_jwt(linkD4,username="laurent.dubroca")  
	D4<-content(resp4,as="text")
	D4<-fromJSON(D4)
	resp5<-ices_get_jwt(linkD5,username="laurent.dubroca")  
	D5<-content(resp5,as="text")
	D5<-fromJSON(D5)

	write.csv(D1,file="../data/wgbycdb/D1.csv",row.names=F)
	write.csv(D2,file="../data/wgbycdb/D2.csv",row.names=F)
	write.csv(D3,file="../data/wgbycdb/D3.csv",row.names=F)
	write.csv(D4,file="../data/wgbycdb/D4.csv",row.names=F)
	write.csv(D5,file="../data/wgbycdb/D5.csv",row.names=F)
	

