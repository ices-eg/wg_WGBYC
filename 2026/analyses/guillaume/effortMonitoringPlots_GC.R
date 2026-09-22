#####################################################
#
#
#                     Plot WGBYC
#              Effort Monitoring plot
#
#
#
# R Version: 4.5.2
# Author : Guillaume Chero
# Date : September 2026
####################################################

rm(list = ls())
here::i_am("project.Rproj")

# 1. Install packages ==================================================================

lapply(c("tidyverse", 'httr', 'jsonlite', "icesConnect", "data.table", 
         "ggplot2", "grid", "gridExtra", "glue", "sf", "rnaturalearth", "rnaturalearthdata", 
         "lwgeom", "viridis", "stringr", "ggforce", "here"), library, character.only = TRUE)



# 2. Access D1, D2, D3  ==================================================================

# require(icesConnect)
# userName <- '.'
# key <- "."
# icesConnect::set_username(userName)
# icesConnect:::token_set_from_keyring(key, username = userName)

linkD1<-"https://bycatch.ices.dk/api/GetD1_Fishing_effort"
linkD2<-"https://bycatch.ices.dk/api/GetD2_Bycatch_monitoring_effort"
linkD3<-"https://bycatch.ices.dk/api/GetD3_BycatchEvent"


# 3. Download D1, D2, D3  ==================================================================

if(file.exists(here::here("data","D1_wgbyc.csv"))){
  #read dowloaded data
  D1<-fread(here::here("data","D1_wgbyc.csv"))
  D2<-fread(here::here("data","D2_wgbyc.csv"))
  D3<-fread(here::here("data","D3_wgbyc.csv"))
}else{
  options(icesSAG.use_token=T)
  resp1<-ices_get_jwt(linkD1)
  D1<-content(resp1,as="text")
  D1<-fromJSON(D1)
  resp2<-ices_get_jwt(linkD2)
  D2<-content(resp2,as="text")
  D2<-fromJSON(D2)
  #D3 request by year: API slow by construction
  D3<-data.frame()
  for(idyear in 2017:2025){
    print(idyear)
    resp3<-ices_get_jwt(paste0(linkD3,"?year=",idyear))
    D3tmp<-content(resp3,as="text")
    D3tmp<-fromJSON(D3tmp)
    D3<-rbind(D3,D3tmp)
  }
  #save file in csv format into data dir
  write.csv(D1,file=here("data","D1_wgbyc.csv"),row.names=F)
  write.csv(D2,file=here("data","D2_wgbyc.csv"),row.names=F)
  write.csv(D3,file=here("data","D3_wgbyc.csv"),row.names=F)
}

if(!exists("D1")){D1 <- data.table::fread(here("data","D1_wgbyc.csv"))}
if(!exists("D2")){D2 <- data.table::fread(here("data","D2_wgbyc.csv"))}
if(!exists("D3")){D3 <- data.table::fread(here("data","D3_wgbyc.csv"))}


# 2. Select parameters =======================================

## 2.1 year ====
analysis_year = 2025

## 2.2 path ====
path_export = "./2026/outputs/"

## 2.3 name ====
rename_vector <- c(
  # L4 level
  "DRB" = "DRB - Boat dredge", "FPN" = "FPN - Stationary uncovered pound nets", "FPO" = "FPO - Pots and traps",
  "FWR" = "FWR - Fixed installations for fences and weirs", "FYK" = "FYK - Fyke nets", "GEL" = "GEL - Glass eel fishing",
  "GN" = "GN - Gill nets", "GNC" = "GNC - Encircling gillnets", "GND" = "GND - Driftnet", "GNS" = "GNS - Set gillnet",
  "GTN" = "GTN - Gillnets-trammel nets", "GTR" = "GTR - Trammel nets", "HMD" = "HMD - Mechanised/suction dredge",
  "LA" = "LA - Lampara nets", "LH" = "LH - Hand and pole lines", "LHM" = "LHM - Hand and pole lines",
  "LHP" = "LHP - Hand and pole lines (hand-operated)", "LLD" = "LLD - Drifting longlines", "LLS" = "LLS - Set longlines",
  "LTL" = "LTL - Trolling lines", "LX" = "LX - Hooks and lines", "MIS" = "MIS - Gear nei.", "OTB" = "OTB - Bottom otter trawl",
  "OTM" = "OTM - Midwater otter trawl", "OTT" = "OTT - Milti-rig otter trawl", "PS" = "PS - Purse seine",
  "PTB" = "PTB - Bottom pair trawl", "PTM" = "PTM - Pelagic pair trawl", "SB" = "SB - Beach Seine",
  "SBV" = "SBV - Beach and boat seine", "SDN" = "SDN - Anchored seine", "SPR" = "SPR - Pair seine",
  "SSC" = "SSC - Fly shooting seine", "TBB" = "TBB - Beam trawl",
  # L3 level
  "L3DR" = "DR - Dredges", "L3FIX" = "Fix - Traps", "L3GN" = "GN - Nets",
  "L3LH" = "LH - Rods and Lines", "L3LL" = "LL - Longlines", "L3PS" = "PS - Surrounding Nets",
  "L3PT" = "PT - Pelagic Trawls", "L3SX" = "SX - Seines", "L3TB" = "TB - Bottom Trawls"
)

## 2.4 NAFO ====
include_NAFO_areas <- FALSE

## 2.5 map =====
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% st_make_valid() %>% st_transform(crs = 3035)

## 2.6 ICES area and GFCM ======
# The shape file come from Laurent Dubroca, we need to found a wya to source it correctly
ices_gfcm <- readRDS("./2026/data/ICES_areas_map/allgeo.rds") 

## 2.7 NAFO =========
if(include_NAFO_areas){
  NAFOareas <- st_read("../../ICES_areas_map/NAFO_Divisions.shp") %>% st_transform(crs = st_crs(ices_gfcm))
  combined_areas <- bind_rows(combined_areas, NAFOareas %>% select(geometry, icesdiv = Label) %>% mutate(Area_Full = paste0("21.", str_replace(as.character(Label), "([0-9])([A-Z])", "\\1.\\2"))))
} else {
  combined_areas <- ices_gfcm
}

## 2.8 Transform shapefile =====
ICESareas_laea <- st_transform(combined_areas, crs = 3035)



# 3. Data processing D1 =====================================================================

## 3.1 Data D1 =============================
D1_data <- D1 %>% 
  filter(!areaCode %in% c("27.1", "27.6", "27.1~27.2.a~27.2.b", "21.1")) %>%
  mutate(areaCode = recode(areaCode, "27.3.a" = "27.3.a.20", "27.5.a" = "27.5.a.2", "27.3.d.28" = "27.3.d.28.2", "11.1" = "111", "11.2" = "112", "21.3.l" = "21.3.L", "21.3.m" = "21.3.M", "21.3.n" = "21.3.N", "21.3.k" = "21.3.K", "21.3.o" = "21.3.O", "21.4.V.s" = "21.4.Vs"))
if (include_NAFO_areas) {
  D1_data <- D1_data %>% filter(areaType != "NAFOArea")
}


## 3.2 function to process data ==========================================
process_data <- function(data, group_var, rename_vector) {
  data %>% filter(year == analysis_year) %>%
    group_by(!!sym(group_var), areaCode) %>% 
    summarize(total_daysAtSeaF = sum(daysAtSeaF, na.rm = TRUE)) %>% 
    left_join(ICESareas_laea, by = c("areaCode" = "offname")) %>%
    mutate(daysAtSeaF_cat = cut(total_daysAtSeaF, 
                                breaks = c(-Inf, 100, 1000, 2000, 5000, 10000, 25000, Inf), 
                                labels = c("< 100", "100-1000", "1000-2000", "2000-5000", "5000-10000", "10000-25000", "> 25000")),
           !!sym(group_var) := recode(!!sym(group_var), !!!rename_vector)) %>%
    filter(!is.na(daysAtSeaF_cat) & !!sym(group_var) != "L3NK")
}


## 3.3 process L3 and L4 data ==========================================
daysAtSea_spatial <- process_data(D1_data, "metierL3", rename_vector)
daysAtSea_L4_spatial <- process_data(D1_data, "metierL4", rename_vector)

hist(daysAtSea_spatial$total_daysAtSeaF, breaks = seq(from = 0, to = 1000000, by = 100), xlim = c(0, 10000))
hist(daysAtSea_L4_spatial$total_daysAtSeaF, breaks = seq(from = 0, to = 1000000, by = 100), xlim = c(0, 10000))


# 3. Data processing D2 =====================================================================

## 3.1 Data D2 ==================
D2_data <- D2 %>% filter(!areaCode %in% c("27.1", "27.6", "27.1~27.2.a~27.2.b", "21.1")) %>%
  mutate(areaCode = recode(areaCode, "27.3.a" = "27.3.a.20", "27.5.a" = "27.5.a.2", "27.3.d.28" = "27.3.d.28.2", "11.1" = "111", "11.2" = "112", "21.3.l" = "21.3.L", "21.3.m" = "21.3.M", "21.3.n" = "21.3.N", "21.3.k" = "21.3.K", "21.3.o" = "21.3.O", "21.4.V.s" = "21.4.Vs"))

## exclude all log books (LB), port observers (PO), and all log books (except for Norway)
indRef <- which(D2_data$monitoringMethod == 'SO' | #all observer data
                  D2_data$monitoringMethod == 'VO' & D2_data$country != 'EE' | #include vessel crew except Estonia
                  D2_data$monitoringMethod == 'EM' | #all electronic monitoring data
                  D2_data$monitoringMethod == 'OTH' & D2_data$country == 'NO') #include other only for Norway

D2_data <- D2_data[indRef, ]
rm(indRef)

## 3.2 Filtert Data
daysAtSea_Ob <- D2_data %>% filter(year == analysis_year) %>%
  group_by(metierL3, areaCode) %>%
  summarize(daysAtSea_Ob = sum(daysAtSeaOb, na.rm = TRUE)) %>%
  left_join(ICESareas_laea, by = c("areaCode" = "offname")) 


## 3.2 assign breaks for plotting and sort data accordingly ====
daysAtSea_Ob <- daysAtSea_Ob %>% mutate(daysAtSea_Ob_cat = cut(daysAtSea_Ob, 
                                                               breaks = c(-Inf, 5, 10, 20, 50, 100, 500, Inf), 
                                                               labels = c("<5", "5-10", "10-20", "20-50", "50-100", "100-500", ">500")),
                                        metierL3 = recode(metierL3, !!!rename_vector)) %>%
  filter(!is.na(daysAtSea_Ob_cat) & metierL3 != "L3NK")


## 3.3 monitoring effort Metier L4 ==============
daysAtSea_L4_Ob <- D2_data %>% filter(year == analysis_year) %>%
  group_by(metierL4, areaCode) %>%
  summarize(daysAtSea_Ob = sum(daysAtSeaOb, na.rm = TRUE)) %>%
  left_join(ICESareas_laea, by = c("areaCode" = "offname")) %>%
  mutate(daysAtSea_L4_Ob_cat = cut(daysAtSea_Ob, 
                                   breaks = c(-Inf, 5, 10, 20, 50, 100, 500, Inf), 
                                   labels = c("<5", "5-10", "10-20", "20-50", "50-100", "100-500", ">500")),
         metierL4 = recode(metierL4, !!!rename_vector)) %>%
  filter(!is.na(daysAtSea_L4_Ob_cat) & metierL4 != "L4NK")



## 3.4 observer coverage L3 =========================
matchedDaysAtSea <- full_join(daysAtSea_spatial, daysAtSea_Ob, by = c("areaCode", "metierL3"))
hist((matchedDaysAtSea$daysAtSea_Ob/matchedDaysAtSea$total_daysAtSeaF)*100, breaks = seq(from = 0, to = 100, by = 0.1), xlim = c(0, 2))

spatialCoverage <- left_join(ICESareas_laea, matchedDaysAtSea, by = c("offname" = "areaCode")) %>%
  mutate(Obs_Coverage = (daysAtSea_Ob / total_daysAtSeaF)*100,
         Obs_Coverage_cat = cut(Obs_Coverage, 
                                breaks = c(-Inf, 0.25, 0.5, 0.75, 1, 2, 4, Inf),
                                labels = c("< 0.25", "0.25-0.5",  "0.5-0.75", "0.75-1.0", "1.0-2.0", "2.0-4.0", "> 4.0"))) %>%
  filter(!is.na(Obs_Coverage_cat) & metierL3 != "L3NK") %>%
  mutate(metierL3 = recode(metierL3, !!!rename_vector))



## 3.5 Observer coverage L4 ==================================
matchedDaysAtSea_L4 <- full_join(daysAtSea_L4_spatial, daysAtSea_L4_Ob, by = c("areaCode", "metierL4"))
spatialCoverage_L4 <- left_join(ICESareas_laea, matchedDaysAtSea_L4, by = c("offname" = "areaCode")) %>%
  mutate(Obs_Coverage = (daysAtSea_Ob / total_daysAtSeaF)*100,
         Obs_Coverage_cat = cut(Obs_Coverage, 
                                breaks = c(-Inf, 0.1, 0.25, 0.5, 0.75, 1, 2, Inf),
                                labels = c("< 0.1", "0.1-0.25", "0.25-0.5",  "0.5-0.75", "0.75-1.0", "1.0-2.0", "> 2.0"))) %>%
  filter(!is.na(Obs_Coverage_cat) & metierL4 != "L4NK") %>%
  mutate(metierL4 = recode(metierL4, !!!rename_vector))




# 4. PLOT ========================================================

## 4.1 Palette ==================================================

## color palette options to avoid a highly controversial continuous log scale
c("#FFCCCC", "#FF9999", "#FF6666", "#FF3333", "#FF0000", "#CC0000") # old and ugly

c("#440154FF", "#46337EFF", "#365C8DFF", "#277F8EFF", "#1FA187FF", "#4AC16DFF", "#9FDA3AFF", "#FDE725FF") # 8 levels

c("#440154FF", "#443A83FF", "#31688EFF", "#21908CFF", "#35B779FF", "#8FD744FF", "#FDE725FF") # 7 levels

c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF") # 6 levels

## define color palette for plot with shades of red
ownPalette <- list(
  fishingEffort = c("< 100" = "#440154FF", "100-1000" = "#443A83FF", "1000-2000" = "#31688EFF", 
                    "2000-5000" = "#21908CFF", "5000-10000" = "#35B779FF", "10000-25000" = "#8FD744FF", "> 25000" = "#FDE725FF"),
  observer = c("<5" = "#440154FF", "5-10" = "#443A83FF", "10-20" = "#31688EFF", 
               "20-50" = "#21908CFF", "50-100" = "#35B779FF", "100-500" = "#8FD744FF", ">500" = "#FDE725FF"),
  observerPerc = c("< 0.25" = "#440154FF", "0.25-0.5" = "#443A83FF", "0.5-0.75" = "#31688EFF",  
                   "0.75-1.0" = "#21908CFF", "1.0-2.0" = "#35B779FF", "2.0-4.0" = "#8FD744FF", "> 4.0" = "#FDE725FF")
)



## 4.2 function to create plots =================
create_plot <- function(data = XXXX, 
                        fill_var = "xxxx", 
                        plotTitle = 'xxxx', 
                        fileName = 'xxxx', 
                        paletteBreaksColours = ownPalette$xxxx, 
                        paletteTitle = 'xxxx', 
                        facet_var = "xxxx", facet_ncol = 3, facet_nrow = 3){
  total_pages <- ceiling(length(unique(data[[facet_var]])) / (facet_ncol * facet_nrow))
  for (page in 1:total_pages) {
    plot <- ggplot(data = data) +
      geom_sf(data = ICESareas_laea, fill = "white", color = "lightgray") +
      geom_sf(data = world, fill = "lightgray", color= "darkgrey", size=0.1) +
      geom_sf(aes_string(geometry = "geometry", fill = fill_var), color = "darkgrey") +
      # scale_fill_viridis(name = paletteBreaksTitle, direction = 1, option = "D", 
      #                     trans = "pseudo_log", breaks = paletteBreaks,
      #                    labels = c(paletteBreakLabels),
      #                    na.value = "white",
      #                     discrete = FALSE,
      #                    guide = guide_colourbar(title.position = "top", barwidth = 18, barheight = 1)) +      
      scale_fill_manual(name = paletteTitle, values = paletteBreaksColours, na.value = "white",
                        guide = guide_legend(title.position="top", label.position = "bottom", nrow = 1, byrow = TRUE)) +
      coord_sf(crs = st_crs(3035), xlim = c(-40000, 6800000), ylim = c(1100000, 7000000)) +
      theme_minimal() +
      labs(title = plotTitle) +
      facet_wrap_paginate(as.formula(paste("~", facet_var)), ncol = facet_ncol, nrow = facet_nrow, page = page, drop = TRUE) +
      theme(strip.text = element_text(size = 6),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom",
            legend.text = element_text(size = 8, angle = -67, vjust = 0.33, hjust = 0),
            legend.title = element_text(size = 10),
            panel.background = element_blank(),
            plot.background = element_blank())
    ggsave(filename = glue(path_export, fileName, ".jpg"), plot = plot, width = 6, height = 7.3)
    print("done")
  }
}



## 4.3 Plots for Metier Level 3 ===================

## days at sea
create_plot(data = daysAtSea_spatial, fill_var = "daysAtSeaF_cat", 
            plotTitle = 'Fishing effort',  fileName = 'fishingEffort_L3', 
            paletteBreaksColours = ownPalette$fishingEffort,  paletteTitle = 'Fishing effort (days at sea)', 
            facet_var = "metierL3", facet_ncol = 3, facet_nrow = 3) 

## monitoring effort days at sea
create_plot(data = daysAtSea_Ob, fill_var = "daysAtSea_Ob_cat", 
            plotTitle = 'Monitoring effort',  fileName = 'monitoringEffort_L3', 
            paletteBreaksColours = ownPalette$observer,  paletteTitle = 'Monitoring effort (days at sea)', 
            facet_var = "metierL3", facet_ncol = 3, facet_nrow = 3) 

## monitoring coverage (%)
create_plot(data = spatialCoverage, fill_var = "Obs_Coverage_cat", 
            plotTitle = 'Monitoring coverage',  fileName = 'monitoringCoverage_L3', 
            paletteBreaksColours = ownPalette$observerPerc,  paletteTitle = 'Monitoring coverage (%)', 
            facet_var = "metierL3", facet_ncol = 3, facet_nrow = 3) 



