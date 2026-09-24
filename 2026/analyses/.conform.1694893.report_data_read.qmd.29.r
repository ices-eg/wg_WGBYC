#|
#source keyring (token stuff)
keyringfile <- here("analyses", "keyring.R")
if (file.exists(keyringfile)) {
    source(keyringfile)
} else {
    stop("keyring file not found")
}
#keyring file is a short R code used to tokenize data exchange
#go to https://data.ices.dk/token to generate the token
#the sourced code is :
#require(icesConnect)
#icesConnect::set_username("john.doe")
#icesConnect:::token_set_from_keyring(
#"thelongcaracterchainofthetokenprovidedbyiceshastobypastedhere",
#    username = "john.doe"
#)

# data request

if (file.exists(here("data", "D1_wgbyc.csv"))) {
    #read dowloaded data
    D1 <- fread(here("data", "D1_wgbyc.csv"))
    D2 <- fread(here("data", "D2_wgbyc.csv"))
    D3 <- fread(here("data", "D3_wgbyc.csv"))
} else {
    # read the data from ices API
    linkD1 <- "https://bycatch.ices.dk/api/GetD1_Fishing_effort"
    linkD2 <- "https://bycatch.ices.dk/api/GetD2_Bycatch_monitoring_effort"
    linkD3 <- "https://bycatch.ices.dk/api/GetD3_BycatchEvent"
    linkD4 <- paste0(
        "https://bycatch.ices.dk/api/GetOverviewSubmissionTable/",
        currentyear + 1
    )
    linkD5 <- "https://bycatch.ices.dk/api/GetByCatchRoadMapListSpecies"
    options(icesSAG.use_token = T)
    resp1 <- ices_get_jwt(linkD1)
    D1 <- content(resp1, as = "text")
    D1 <- fromJSON(D1)
    resp2 <- ices_get_jwt(linkD2)
    D2 <- content(resp2, as = "text")
    D2 <- fromJSON(D2)
    #D3 request by year: API slow by construction
    D3 <- data.frame()
    for (idyear in 2017:2025) {
        print(idyear)
        resp3 <- ices_get_jwt(paste0(linkD3, "?year=", idyear))
        D3tmp <- content(resp3, as = "text")
        D3tmp <- fromJSON(D3tmp)
        D3 <- rbind(D3, D3tmp)
    }
    #add D4 and D5
    resp4 <- ices_get_jwt(linkD4)
    D4 <- content(resp4, as = "text")
    D4 <- fromJSON(D4)
    resp5 <- ices_get_jwt(linkD5)
    D5 <- content(resp5, as = "text")
    D5 <- fromJSON(D5)
    #filtering on year of interest
    D1 <- D1 %>% filter(year %in% 2017:2025)
    D2 <- D2 %>% filter(year %in% 2017:2025)
    D3 <- D3 %>% filter(year %in% 2017:2025)
    #save file in csv format into data dir
    write.csv(D1, file = here("data", "D1_wgbyc.csv"), row.names = F)
    write.csv(D2, file = here("data", "D2_wgbyc.csv"), row.names = F)
    write.csv(D3, file = here("data", "D3_wgbyc.csv"), row.names = F)
    write.csv(D4, file = here("data", "D4_wgbyc.csv"), row.names = F)
    write.csv(D5, file = here("data", "D5_wgbyc.csv"), row.names = F)
}

#get extra info to label stuff
if (file.exists(here("data", "vocab", "BYC_Mon812Type.csv"))) {
    #read dowloaded data
    listvocab <- c(
        "ISO_3166",
        "GearType",
        "GearGroup",
        "BYC_MonProgr",
        "BYC_SamplingProtocol",
        "BYC_MonMethod",
        "BYC_Mon812Type"
    )
    f1 <- function(vocabname) {
        tmp <- fread(here("data", "vocab", paste0(vocabname, ".csv")))
        assign(vocabname, tmp, pos = ".GlobalEnv")
    }
    lapply(listvocab, f1)
} else {
    #downloaded locally some ICES vocab
    listvocab <- c(
        "ISO_3166",
        "GearType",
        "GearGroup",
        "BYC_MonProgr",
        "BYC_SamplingProtocol",
        "BYC_MonMethod",
        "BYC_Mon812Type"
    )
    f1 <- function(vocabname) {
        pipo <- icesVocab::getCodeList(vocabname)
        write.csv(
            pipo,
            file = here("data/vocab", paste0(vocabname, ".csv")),
            row.names = F
        )
    }
    dir.create(here("data", "vocab"))
    lapply(listvocab, f1)
    #manual fix country name
    tmp <- fread(here("data", "vocab", paste0("ISO_3166", ".csv")))
    tmp <- tmp %>%
        mutate(Description = ifelse(Key == "PT-20", "Azores", Description)) %>%
        mutate(Description = ifelse(Key == "NL", "Netherlands", Description)) %>%
        mutate(Description = ifelse(Key == "GB", "United Kingdom", Description))
    write.csv(
        tmp,
        file = here("data/vocab", paste0("ISO_3166", ".csv")),
        row.names = F
    )
}
