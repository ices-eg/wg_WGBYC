# wg_WGBYC

Working Group on Bycatch of Protected Species

The repository is organized by year.

## Access to WGBYC data

First ask ICES to get the right the access WGBYC data using the API.

Then, in R, use this :

	#tokenize data exchange
	library(icesConnect) #ask a token using  ices_token
	# read the data from ices API
	library(httr)
	library(jsonlite)
	linkD1<-"https://bycatch.ices.dk/api/GetD1_Fishing_effort"
	linkD2<-"https://bycatch.ices.dk/api/GetD2_Bycatch_monitoring_effort"
	linkD3<-"https://bycatch.ices.dk/api/GetD3_BycatchEvent"
	# download and parse 
	resp1<-ices_get_jwt(linkD1,username="yourusername")
	D1<-content(resp1,as="text")
	D1<-fromJSON(D1)
	resp2<-ices_get_jwt(linkD2,username="yourusername")
	D2<-content(resp2,as="text")
	D2<-fromJSON(D2)
	resp3<-ices_get_jwt(linkD3,username="yourusername")  
	D3<-content(resp3,as="text")
	D3<-fromJSON(D3)
