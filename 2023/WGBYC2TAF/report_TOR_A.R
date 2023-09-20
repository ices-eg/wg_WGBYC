## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)

mkdir("report")

library(ggplot2)
library(ggalluvial)
library(dplyr)

#TOR A


#monitoring methods: overall view
#generate a docx with the figure
cat("
---
title: 'Monitoring method'
---

# Text

	TO BE DONE (in 2024...)
\\newpage

# Figures

## Figure 1

Proportion of monitored/observed days-at sea-reported per monitoring method from 2017 to 2022 (2023 data call).

![](output/monitoring_method_overview.png)

\\newpage

## Figure 2

Proportion of monitored/observed days-at sea-reported per monitoring method from 2017 to 2022 by countries (2023 data call).

![](output/monitoring_method_country.png)

",file="pipo.Rmd")	

rmarkdown::render("pipo.Rmd",output_format="word_document",output_file="./report/WGBYC_TOR_A_figures_table.docx")
unlink("pipo.Rmd")

 
#TOR A extra


