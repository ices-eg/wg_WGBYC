# CREATE A LOOP TO GENERATE INDIVIDUAL COUNTRY REPORTS

# 0. Load libraries ----
library(rmarkdown)

# 1. Load data ----
D1 <- read.csv(file = "../data/D1_wgbyc26.csv", sep = ";")
D2 <- read.csv(file = "../data/D2_wgbyc26.csv", sep = ";")
D3 <- read.csv(file = "../data/D3_wgbyc26.csv", sep = ";")

# 2. Countries with D1 data in 2025 ----
countries <- sort(unique(D1$country[D1$year == 2025]))

# 3. Run the Rmarkdown with data checks per country ----
for(this_country in countries){
  print(this_country)
  # Subgroup per country all the data
  D1_country <- D1[D1$country %in% this_country & D1$year %in% 2025, ]
  D2_country <- D2[D2$country %in% this_country & D2$year %in% 2025, ]
  D3_country <- D3[D3$country %in% this_country & D3$year %in% 2025, ]
  
  # Generate the report
  render("WGBYC_DataChecks_PerCountry.rmd", 
         output_file = paste0("../results/WGBYC2026_DataCheckReport", this_country, ".html"),
         params = list(country = this_country),
         envir = new.env())
}
