---
title: 'Quality checks'
author: ''
date: ''
output: 
  word_document:
    number_sections: yes
    reference_docx: './boot/data/reportTemplate.docx'
---



# Rationale

This report presents a quality check framework developped for the WGBYC
database. 

WGBYC data are accessed using the ICES API in order to minimize the
data manipultion time and to avoid the error associated with data formatting and
reading.

## Data and reproducibility

Data used in this report were accessed following the TAF guideline.




```
## [1] "TOR A"
```



# Table of responses by country – 2018 – 2023


ICES/WGBYC requested data from  countries through the 2024 data call. 
Bulgaria submitted updated data, providing data from 2019 to 2023. 
A data submission was considered achieved if 
at least one value of effort was reported in the fishing and monitoring effort tables. 
For the bycatch events, only the presence of data was considered, 
as zero values (e.g. absence of bycatch events) is not clearly defined in the datacall. 
The submission status and date are summarized in the table 
1^[https://github.com/ices-eg/wg_WGBYC]. 



Figure 1: Summary of the data submitted in 2024 in the WGBYC database.

# Observation methods evolution

The evolution of the monitoring methods reported in the WGBYC data call is
presented in the figure 
2. This figure is a variation of the figure 3.3
of the TOR a.

![](output_qc_files/figure-docx/newt33-1.png)

Figure 2: Annual proportion of monitoring methods in days at sea from
2017 to 2023.

# Stability checks

In this section, the temporal evolution of the fishing and observation effort
is analysed using basic xy plot. The rationale behind these exploratory analyses
is the hypotheses that observation program of bycatch should be linked in term
of effort to the fishing effort. Statistical summaries were computed by year,
ecoregion and countries to provided an overall view of the data.
Days at sea is the main effort analysed in this section.

# Fishing and observation effort by ecoregion





## Fishing effort 

The figure 3 presents the temporal evolution 
of the fishing effort by
gears (metier level 3) and ecoregion.

![](output_qc_files/figure-docx/plte1all-1.png)
*Figure 3: Temporal evolution of the fishing effort by ecoregion and metier level 3 from 2017 to 2023. In blue, linear model highlight the temporal trends over the 5 years.*

![](output_qc_files/figure-docx/plte1bisi1-1.png)
*Figure 3: Temporal evolution of the fishing effort by ecoregion and metier level 3 from 2017 to 2023. In blue, linear model highlight the temporal trends over the 5 years.*

![](output_qc_files/figure-docx/plte1bis-1.png)
*Figure 4: Month fishing effort by ecoregion and metier level 3 from 2017 to 2023. In blue, linear model highlight the temporal trends over the 5 years.*

## Observation effort 

The figure 5 presents the temporal evolution 
of the observation effort in days at sea by
gears (metier level 3) and ecoregion, and monitoring programs.

![](output_qc_files/figure-docx/plte2all-1.png)
*Figure 5: Temporal evolution of the fishing effort by ecoregion and metier level 3 from 2017 to 2023 by monitoring program. In blue, linear model highlight the temporal trends over the 5 years.*

## Fishing versus observation effort 

The figure 6 shows the total effort of
fishing versus the total effort of observation by year for each ecoregion and
gears.

![](output_qc_files/figure-docx/plte12all-1.png)
*Figure 6: Fishing effort (x-axis) versus observation effort (y-axis) total by years, ecoregion and gear.  In blue, linear model highlight the temporal trends over the 5 years.The red cross highlights the last year provided by the data call.*

# By catch events

Some test on D3 table in relation to D2 and D1.


```
## # A tibble: 316 × 296
##     year L3name       ecoregion `Abramis brama` Acanthocardia tuberc…¹ Acipenser
##    <int> <chr>        <chr>     <list>          <list>                 <list>   
##  1  2019 Traps        Baltic S… <dbl [3]>       <NULL>                 <NULL>   
##  2  2020 Traps        Baltic S… <dbl [2]>       <NULL>                 <NULL>   
##  3  2017 Bottom traw… Western … <NULL>          <dbl [3]>              <NULL>   
##  4  2018 Bottom traw… Western … <NULL>          <dbl [3]>              <NULL>   
##  5  2023 Nets         Baltic S… <NULL>          <NULL>                 <dbl [2]>
##  6  2021 Bottom traw… Ionian S… <NULL>          <NULL>                 <NULL>   
##  7  2021 Nets         Baltic S… <NULL>          <NULL>                 <NULL>   
##  8  2022 Nets         Baltic S… <NULL>          <NULL>                 <NULL>   
##  9  2019 Pelagic tra… Black Sea <NULL>          <NULL>                 <NULL>   
## 10  2023 Bottom traw… Black Sea <NULL>          <NULL>                 <NULL>   
## # ℹ 306 more rows
## # ℹ abbreviated name: ¹​`Acanthocardia tuberculata`
## # ℹ 290 more variables: `Acipenser naccarii` <list>,
## #   `Acipenser oxyrinchus` <list>, `Acipenser stellatus` <list>,
## #   `Aetomylaeus bovinus` <list>, `Alburnus alburnus` <list>,
## #   `Alca torda` <list>, Alcidae <list>, `Alepisaurus ferox` <list>,
## #   `Alepocephalus bairdii` <list>, `Alepocephalus rostratus` <list>, …
```

# Fishing and observation effort by ecoregion and countries

This section reproduces the graphical analyses provided by the previous section,
by country.





## Belgium





### Fishing effort

![](output_qc_files/figure-docx/plte1Belgium-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Belgium-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Belgium-1.png)

\newpage## Bulgaria





### Fishing effort

![](output_qc_files/figure-docx/plte1Bulgaria-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Bulgaria-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Bulgaria-1.png)

\newpage## Croatia





### Fishing effort

![](output_qc_files/figure-docx/plte1Croatia-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Croatia-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Croatia-1.png)

\newpage## Cyprus





### Fishing effort

![](output_qc_files/figure-docx/plte1Cyprus-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Cyprus-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Cyprus-1.png)

\newpage## Denmark





### Fishing effort

![](output_qc_files/figure-docx/plte1Denmark-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Denmark-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Denmark-1.png)

\newpage## Estonia





### Fishing effort

![](output_qc_files/figure-docx/plte1Estonia-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Estonia-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Estonia-1.png)

\newpage## Finland





### Fishing effort

![](output_qc_files/figure-docx/plte1Finland-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Finland-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Finland-1.png)

\newpage## France





### Fishing effort

![](output_qc_files/figure-docx/plte1France-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2France-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12France-1.png)

\newpage## Germany





### Fishing effort

![](output_qc_files/figure-docx/plte1Germany-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Germany-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Germany-1.png)

\newpage## Greece





### Fishing effort

![](output_qc_files/figure-docx/plte1Greece-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Greece-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Greece-1.png)

\newpage## Iceland





### Fishing effort

![](output_qc_files/figure-docx/plte1Iceland-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Iceland-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Iceland-1.png)

\newpage## Ireland





### Fishing effort

![](output_qc_files/figure-docx/plte1Ireland-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Ireland-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Ireland-1.png)

\newpage## Italy





### Fishing effort

![](output_qc_files/figure-docx/plte1Italy-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Italy-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Italy-1.png)

\newpage## Latvia





### Fishing effort

![](output_qc_files/figure-docx/plte1Latvia-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Latvia-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Latvia-1.png)

\newpage## Lithuania





### Fishing effort

![](output_qc_files/figure-docx/plte1Lithuania-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Lithuania-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Lithuania-1.png)

\newpage## Malta





### Fishing effort

![](output_qc_files/figure-docx/plte1Malta-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Malta-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Malta-1.png)

\newpage## Netherlands





### Fishing effort

![](output_qc_files/figure-docx/plte1Netherlands-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Netherlands-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Netherlands-1.png)

\newpage## Norway





### Fishing effort

![](output_qc_files/figure-docx/plte1Norway-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Norway-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Norway-1.png)

\newpage## Poland





### Fishing effort

![](output_qc_files/figure-docx/plte1Poland-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Poland-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Poland-1.png)

\newpage## Portugal





### Fishing effort

![](output_qc_files/figure-docx/plte1Portugal-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Portugal-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Portugal-1.png)

\newpage## Slovenia





### Fishing effort

![](output_qc_files/figure-docx/plte1Slovenia-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Slovenia-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Slovenia-1.png)

\newpage## Spain





### Fishing effort

![](output_qc_files/figure-docx/plte1Spain-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Spain-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Spain-1.png)

\newpage## Sweden





### Fishing effort

![](output_qc_files/figure-docx/plte1Sweden-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2Sweden-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12Sweden-1.png)

\newpage## United Kingdom





### Fishing effort

![](output_qc_files/figure-docx/plte1United Kingdom-1.png)

\newpage

### Observation effort

![](output_qc_files/figure-docx/plte2United Kingdom-1.png)

\newpage

### Fishing vs observation effort

![](output_qc_files/figure-docx/plte12United Kingdom-1.png)

\newpage




# Fishing effort

This section describes the effort reported in the fishing effort table.

Seven effort indicators can be reported to WGBYC, 2 of them being mandatory,
only one is used here.

\newpage

# References
