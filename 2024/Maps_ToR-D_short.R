# Libraries
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(lwgeom)
library(viridis)
library(stringr)
library(ggforce)

#########################
#NOTE #### REMOVE THE LINE WHERE ICELANDIC DATA FROM 2022 IS USED FOR 2023!!!!
#Only EM, VO, and SO are presented AND OTH, but discus next year!
#Change to this if lb needed
  #DaysAtSea_Ob <- D2_data %>% filter(year == analysis_year, monitoringMethod %in% c("SO", "EM", "VO", "OTH", "LB", "PO")) %>%


######################

# Select values
analysis_year <- 2023
Include_NAFO_areas <- 0 #1 if want to use NAFO

# Define color palette for plot with shades of red
OwnPalette <- list(
  FishingEffort = c("Less than 1 000" = "#FFCCCC", "1 000 to 3 000" = "#FF9999", "3 000 to 10 000" = "#FF6666",
                    "10 000 to 25 000" = "#FF3333", "25 000 to 50 000" = "#FF0000", "50 000 or more" = "#CC0000"),
  Observer = c("Less than 10" = "#FFCCCC", "10 to 20" = "#FF9999", "20 to 50" = "#FF6666",
               "50 to 100" = "#FF3333", "100 to 500" = "#FF0000", "500 or more" = "#CC0000"),
  ObserverPerc = c("Less than 0.25" = "#FFCCCC", "0.25 to 0.50" = "#FF9999", "0.50 to 0.75" = "#FF6666",
                   "0.75 to 1.00" = "#FF3333", "1.00 to 2.00" = "#FF0000", "More than 2.00" = "#CC0000")
)

#USE OLD PALETTE
#OwnPalette <- list(
#  FishingEffort = c("Less than 1 000" = "#800080", "1 000 to 3 000" = "#00008B", "3 000 to 10 000" = "#ADD8E6",
#                    "10 000 to 25 000" = "#00FFFF", "25 000 to 50 000" = "#90EE90", "50 000 or more" = "#FFFF00"),
#  Observer = c("Less than 10" = "#800080", "10 to 20" = "#00008B", "20 to 50" = "#ADD8E6",
#               "50 to 100" = "#00FFFF", "100 to 500" = "#90EE90", "500 or more" = "#FFFF00"),
#  ObserverPerc = c("Less than 0.25" = "#800080", "0.25 to 0.50" = "#00008B", "0.50 to 0.75" = "#ADD8E6",
#                   "0.75 to 1.00" = "#00FFFF", "1.00 to 2.00" = "#90EE90", "More than 2.00" = "#FFFF00")
#)


# Rename metiers
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

# Load and transform data
world <- ne_countries(scale = "medium", returnclass = "sf") %>% st_make_valid() %>% st_transform(crs = 3035)
ICESareas <- st_read("2024/ICES_areas_map/ICES_Areas_20160601_cut_dense_3857.shp")
GSAareas <- st_read("2024/ICES_areas_map/GSAs_simplified.shp") %>% st_transform(crs = st_crs(ICESareas))
NAFOareas <- st_read("2024/ICES_areas_map/NAFO_Divisions.shp") %>% st_transform(crs = st_crs(ICESareas))

# Merge areas
combined_areas <- bind_rows(ICESareas, GSAareas %>% select(geometry, Area_Full = SMU_CODE) %>% mutate(Area_Full = as.character(Area_Full)))
if (Include_NAFO_areas == 1) {
  combined_areas <- bind_rows(combined_areas, NAFOareas %>% select(geometry, Area_Full = Label) %>% mutate(Area_Full = paste0("21.", str_replace(as.character(Label), "([0-9])([A-Z])", "\\1.\\2"))))
}
ICESareas_laea <- st_transform(combined_areas, crs = 3035)

# Data cleaning
D1_data <- D1 %>% filter(!areaCode %in% c("27.1", "27.6", "27.1~27.2.a~27.2.b", "21.1")) %>%
  mutate(areaCode = recode(areaCode, "27.3.a" = "27.3.a.20", "27.5.a" = "27.5.a.2", "27.3.d.28" = "27.3.d.28.2", "11.1" = "111", "11.2" = "112", "21.3.l" = "21.3.L", "21.3.m" = "21.3.M", "21.3.n" = "21.3.N", "21.3.k" = "21.3.K", "21.3.o" = "21.3.O", "21.4.V.s" = "21.4.Vs"))
if (Include_NAFO_areas == 0) {
  D1_data <- D1_data %>% filter(areaType != "NAFOArea")
}

##################################################################
####REMOVE THIS IN FUTURE YEARS!####

D1_data <- D1_data %>%
  filter(!(year == 2023 & country == "IS"))

new_rows <- D1_data %>%
  filter(year == 2022 & country == "IS") %>%
  mutate(year = 2023)

D1_data <- bind_rows(D1_data, new_rows)

#######################################################
###################################################

# Function to process data
process_data <- function(data, group_var, rename_vector) {
  data %>% filter(year == analysis_year) %>%
    group_by(!!sym(group_var), areaCode) %>%
    summarize(total_daysAtSeaF = sum(daysAtSeaF, na.rm = TRUE)) %>%
    left_join(ICESareas_laea, by = c("areaCode" = "Area_Full")) %>%
    mutate(daysAtSeaF_cat = cut(total_daysAtSeaF, breaks = c(-Inf, 1000, 3000, 10000, 25000, 50000, Inf), labels = c("Less than 1 000", "1 000 to 3 000", "3 000 to 10 000", "10 000 to 25 000", "25 000 to 50 000", "50 000 or more")),
           !!sym(group_var) := recode(!!sym(group_var), !!!rename_vector)) %>%
    filter(!is.na(daysAtSeaF_cat) & !!sym(group_var) != "L3NK")
}

# Process L3 and L4 data
DaysAtSea_spatial <- process_data(D1_data, "metierL3", rename_vector)
DaysAtSea_L4_spatial <- process_data(D1_data, "metierL4", rename_vector)

# Monitoring effort Metier L3
D2_data <- D2 %>% filter(!areaCode %in% c("27.1", "27.6", "27.1~27.2.a~27.2.b", "21.1")) %>%
  mutate(areaCode = recode(areaCode, "27.3.a" = "27.3.a.20", "27.5.a" = "27.5.a.2", "27.3.d.28" = "27.3.d.28.2", "11.1" = "111", "11.2" = "112", "21.3.l" = "21.3.L", "21.3.m" = "21.3.M", "21.3.n" = "21.3.N", "21.3.k" = "21.3.K", "21.3.o" = "21.3.O", "21.4.V.s" = "21.4.Vs"))
DaysAtSea_Ob <- D2_data %>% filter(year == analysis_year, monitoringMethod %in% c("SO", "EM", "VO", "OTH")) %>%
  group_by(metierL3, areaCode) %>%
  summarize(DaysAtSea_Ob = sum(daysAtSeaOb, na.rm = TRUE)) %>%
  left_join(ICESareas_laea, by = c("areaCode" = "Area_Full")) %>%
  mutate(DaysAtSea_Ob_cat = cut(DaysAtSea_Ob, breaks = c(-Inf, 10, 20, 50, 100, 500, Inf), labels = c("Less than 10", "10 to 20", "20 to 50", "50 to 100", "100 to 500", "500 or more")),
         metierL3 = recode(metierL3, !!!rename_vector)) %>%
  filter(!is.na(DaysAtSea_Ob_cat) & metierL3 != "L3NK")

# Monitoring effort Metier L4
DaysAtSea_L4_Ob <- D2_data %>% filter(year == analysis_year) %>%
  group_by(metierL4, areaCode) %>%
  summarize(DaysAtSea_Ob = sum(daysAtSeaOb, na.rm = TRUE)) %>%
  left_join(ICESareas_laea, by = c("areaCode" = "Area_Full")) %>%
  mutate(DaysAtSea_L4_Ob_cat = cut(DaysAtSea_Ob, breaks = c(-Inf, 10, 20, 50, 100, 500, Inf), labels = c("Less than 10", "10 to 20", "20 to 50", "50 to 100", "100 to 500", "500 or more")),
         metierL4 = recode(metierL4, !!!rename_vector)) %>%
  filter(!is.na(DaysAtSea_L4_Ob_cat) & metierL4 != "L4NK")

# Observer coverage L3
MatchedDaysAtSea <- full_join(DaysAtSea_spatial, DaysAtSea_Ob, by = c("areaCode", "metierL3"))
SpatialCoverage <- left_join(ICESareas_laea, MatchedDaysAtSea, by = c("Area_Full" = "areaCode")) %>%
  mutate(Obs_Coverage = DaysAtSea_Ob / total_daysAtSeaF,
         Obs_Coverage_cat = cut(Obs_Coverage, breaks = c(-Inf, 0.0025, 0.005, 0.0075, 0.01, 0.02, Inf),
                                labels = c("Less than 0.25", "0.25 to 0.50", "0.50 to 0.75", "0.75 to 1.00", "1.00 to 2.00", "More than 2.00"))) %>%
  filter(!is.na(Obs_Coverage_cat) & metierL3 != "L3NK") %>%
  mutate(metierL3 = recode(metierL3, !!!rename_vector))

# Observer coverage L4
MatchedDaysAtSea_L4 <- full_join(DaysAtSea_L4_spatial, DaysAtSea_L4_Ob, by = c("areaCode", "metierL4"))
SpatialCoverage_L4 <- left_join(ICESareas_laea, MatchedDaysAtSea_L4, by = c("Area_Full" = "areaCode")) %>%
  mutate(Obs_Coverage = DaysAtSea_Ob / total_daysAtSeaF,
         Obs_Coverage_cat = cut(Obs_Coverage, breaks = c(-Inf, 0.25, 0.5, 0.75, 1, 2, Inf),
                                labels = c("Less than 0.25", "0.25 to 0.50", "0.50 to 0.75", "0.75 to 1.00", "1.00 to 2.00", "More than 2.00"))) %>%
  filter(!is.na(Obs_Coverage_cat) & metierL4 != "L4NK") %>%
  mutate(metierL4 = recode(metierL4, !!!rename_vector))

# Function to create and save plots
create_plot <- function(data, fill_var, title, fill_label, filename_prefix, facet_var, ncol = 3, nrow = 3, palette) {
  total_pages <- ceiling(length(unique(data[[facet_var]])) / (ncol * nrow))
  for (page in 1:total_pages) {
    plot <- ggplot(data = data) +
      geom_sf(data = ICESareas_laea, fill = "white", color = "lightgray") +
      geom_sf(data = world, fill = "lightgray", color= "darkgrey", size=0.1) +
      geom_sf(aes_string(geometry = "geometry", fill = fill_var), color = "darkgrey") +
      scale_fill_manual(values = palette, na.value = "white", name = fill_label) +
      coord_sf(crs = st_crs(3035), xlim = c(-40000, 6800000), ylim = c(1100000, 7000000)) +
      theme_minimal() +
      labs(title = title, fill = fill_label) +
      facet_wrap_paginate(as.formula(paste("~", facet_var)), ncol = ncol, nrow = nrow, page = page, drop = TRUE) +
      theme(strip.text = element_text(size = 6),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom",
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 10),
            panel.background = element_blank(),
            plot.background = element_blank())
    ggsave(filename = paste0(filename_prefix, "_page_", page, ".jpg"), plot = plot, width = 6, height = 7.3)
  }
}

# Plot for Metier Level 3
create_plot(DaysAtSea_spatial, "daysAtSeaF_cat", "Fishing Effort", "Days at Sea - Metier Level 3", "FishingEffort_L3", "metierL3", palette = OwnPalette$FishingEffort)
# Plot for Metier Level 4
create_plot(DaysAtSea_L4_spatial, "daysAtSeaF_cat", "Fishing Effort", "Days at Sea - Metier Level 4", "FishingEffort_L4", "metierL4", palette = OwnPalette$FishingEffort)
# Plot for Monitoring Effort Metier Level 3
create_plot(DaysAtSea_Ob, "DaysAtSea_Ob_cat", "Monitoring Effort", "Days at Sea - Metier Level 3", "MonitoringEffort_L3", "metierL3", palette = OwnPalette$Observer)
# Plot for Monitoring Effort Metier Level 4
create_plot(DaysAtSea_L4_Ob, "DaysAtSea_L4_Ob_cat", "Monitoring Effort", "Days at Sea - Metier Level 4", "MonitoringEffort_L4", "metierL4", palette = OwnPalette$Observer)
# Plot for Monitoring Coverage Metier Level 3
create_plot(SpatialCoverage, "Obs_Coverage_cat", "Monitoring Coverage %", "Monitoring Coverage (%) - Metier Level 3", "MonitoringCoverage_L3", "metierL3", palette = OwnPalette$ObserverPerc)
# Plot for Monitoring Coverage Metier Level 4
create_plot(SpatialCoverage_L4, "Obs_Coverage_cat", "Monitoring Coverage %", "Monitoring Coverage (%) - Metier Level 4", "MonitoringCoverage_L4", "metierL4", palette = OwnPalette$ObserverPerc)
