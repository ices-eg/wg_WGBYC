# wg_WGBYC

Working Group on Bycatch of Protected Species

The repository is organized by year.

## Access to WGBYC data

The WGBYC data format is here http://datsu.ices.dk/web/selRep.aspx?Dataset=128.

First ask ICES to get the right to access WGBYC data using the API.

Then, in R, use this :


    require(icesConnect)
    icesConnect::set_username("YOUR USERNAME")
    #go to https://bycatch.ices.dk/manage/jwt and generate a token
    icesConnect:::token_set_from_keyring("COPY PASTE YOUR TOKEN HERE","YOUR USERNAME")
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
	resp1<-ices_get_jwt(linkD1,username="YOUR USERNAME",verbose=T,quiet=F)  
	D1<-content(resp1,as="text")
	D1<-fromJSON(D1)
	resp2<-ices_get_jwt(linkD2,username="YOUR USERNAME",verbose=T)  
	D2<-content(resp2,as="text")
	D2<-fromJSON(D2)
	resp3<-ices_get_jwt(linkD3,username="YOUR USERNAME")  
	D3<-content(resp3,as="text")
	D3<-fromJSON(D3)
	resp4<-ices_get_jwt(linkD4,username="YOUR USERNAME")  
	D4<-content(resp4,as="text")
	D4<-fromJSON(D4)
	resp5<-ices_get_jwt(linkD5,username="YOUR USERNAME")  
	D5<-content(resp5,as="text")
	D5<-fromJSON(D5)
    #the dataframe D1 to D5 are now available in your working space
