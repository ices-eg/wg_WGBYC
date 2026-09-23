# Select year
analysis_year = 2025

# Rename vector
rename_vector <- c(
    # L4 level
    "DRB" = "DRB - Boat dredge",
    "FPN" = "FPN - Stationary uncovered pound nets",
    "FPO" = "FPO - Pots and traps",
    "FWR" = "FWR - Fixed installations for fences and weirs",
    "FYK" = "FYK - Fyke nets",
    "GEL" = "GEL - Glass eel fishing",
    "GN" = "GN - Gill nets",
    "GNC" = "GNC - Encircling gillnets",
    "GND" = "GND - Driftnet",
    "GNS" = "GNS - Set gillnet",
    "GTN" = "GTN - Gillnets-trammel nets",
    "GTR" = "GTR - Trammel nets",
    "HMD" = "HMD - Mechanised/suction dredge",
    "LA" = "LA - Lampara nets",
    "LH" = "LH - Hand and pole lines",
    "LHM" = "LHM - Hand and pole lines",
    "LHP" = "LHP - Hand and pole lines (hand-operated)",
    "LLD" = "LLD - Drifting longlines",
    "LLS" = "LLS - Set longlines",
    "LTL" = "LTL - Trolling lines",
    "LX" = "LX - Hooks and lines",
    "MIS" = "MIS - Gear nei.",
    "OTB" = "OTB - Bottom otter trawl",
    "OTM" = "OTM - Midwater otter trawl",
    "OTT" = "OTT - Milti-rig otter trawl",
    "PS" = "PS - Purse seine",
    "PTB" = "PTB - Bottom pair trawl",
    "PTM" = "PTM - Pelagic pair trawl",
    "SB" = "SB - Beach Seine",
    "SBV" = "SBV - Beach and boat seine",
    "SDN" = "SDN - Anchored seine",
    "SPR" = "SPR - Pair seine",
    "SSC" = "SSC - Fly shooting seine",
    "TBB" = "TBB - Beam trawl",
    # L3 level
    "L3DR" = "DR - Dredges",
    "L3FIX" = "Fix - Traps",
    "L3GN" = "GN - Nets",
    "L3LH" = "LH - Rods and Lines",
    "L3LL" = "LL - Longlines",
    "L3PS" = "PS - Surrounding Nets",
    "L3PT" = "PT - Pelagic Trawls",
    "L3SX" = "SX - Seines",
    "L3TB" = "TB - Bottom Trawls"
)

# Do we include NAFO or not
include_NAFO_areas <- FALSE

## Prepare Map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
    st_make_valid() %>%
    st_transform(crs = 3035)

# ICES area and GFCM
# The shape file come from Laurent Dubroca, we need to found a wya to source it correctly
ices_gfcm <- readRDS(here("data", "vocab", "allgeo.rds"))

# NAFO
if (include_NAFO_areas) {
    NAFOareas <- st_read("../../ICES_areas_map/NAFO_Divisions.shp") %>%
        st_transform(crs = st_crs(ices_gfcm))
    combined_areas <- bind_rows(
        combined_areas,
        NAFOareas %>%
            select(geometry, icesdiv = Label) %>%
            mutate(
                Area_Full = paste0(
                    "21.",
                    str_replace(as.character(Label), "([0-9])([A-Z])", "\\1.\\2")
                )
            )
    )
} else {
    combined_areas <- ices_gfcm
}

# ransform shapefile =====
ICESareas_laea <- st_transform(combined_areas, crs = 3035)
