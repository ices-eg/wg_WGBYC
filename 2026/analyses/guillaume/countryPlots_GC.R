#####################################################
#
#
#                     Plot WGBYC
#   Number of bycatch per species/ecoregion/year
#
#
#
# R Version: 4.5.2
# Author : Guillaume Chero
# Date : September 2026
####################################################

rm(list = ls())


# 1. Install packages ==================================================================

lapply(c("tidyverse", 'httr', 'jsonlite', "icesConnect", "data.table", 
         "ggplot2", "grid", "gridExtra", "glue", "here"), library, character.only = TRUE)



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

if(file.exists(here("data","D1_wgbyc.csv"))){
  #read dowloaded data
  D1<-fread(here("data","D1_wgbyc.csv"))
  D2<-fread(here("data","D2_wgbyc.csv"))
  D3<-fread(here("data","D3_wgbyc.csv"))
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

# 4. Check data ===========================================

range(D3$individualsWithPingers, na.rm =TRUE)
range(D3$individualsWithoutPingers, na.rm =TRUE)

D3$individualsWithPingers[which(D3$individualsWithPingers < 0)]
D3$individualsWithoutPingers[which(D3$individualsWithoutPingers < 0)]


# 5. Select parameters =======================================

ecoregions_unique <- unique(D3$ecoregion)
years_unique <- c(2017:2025)
path_export = "./2026/outputs/"


# 6. Create base dataset ========================================


## 6.1 Clean D3 ===============================================

D3 <- 
  D3 %>% 
  mutate(classname = case_when(superClass == "Reptilia" & is.na(D3$classname) ~ 'Reptilia', 
                               .default = classname))


## 6.2 Summary dataset ===============================

### 6.2.1 Create a summary data empty ==============================
summaryData <- expand.grid(years_unique, ecoregions_unique) %>% 
  rename(year = Var1, ecoregion = Var2) %>% 
  data.frame() %>% 
  mutate(Aves = NA, 
         Elasmobranchii = NA, 
         Holocephali = NA, 
         Mammalia = NA, 
         Reptilia = NA, 
         Teleostei = NA, 
         Chondrostei = NA, 
         Petromyzonti = NA) 


### 6.2.2 Count the number of bycatch by Year / Ecoregion / Order =======
# The loop goes row by row to fill the summurized information

for(II in 1:nrow(summaryData)){
  dataRef <- D3[which(D3$ecoregion == summaryData$ecoregion[II] & D3$year == summaryData$year[II]), ]
  
  summaryData$Aves[II] <- sum(dataRef$individualsWithPingers[which(dataRef$classname == "Aves")], na.rm = TRUE) + 
    sum(dataRef$individualsWithoutPingers[which(dataRef$classname == "Aves")], na.rm = TRUE)
  
  summaryData$Elasmobranchii[II] <- sum(dataRef$individualsWithPingers[which(dataRef$classname == "Elasmobranchii")], na.rm = TRUE) + 
    sum(dataRef$individualsWithoutPingers[which(dataRef$classname == "Elasmobranchii")], na.rm = TRUE)
  
  summaryData$Holocephali[II] <- sum(dataRef$individualsWithPingers[which(dataRef$classname == "Holocephali")], na.rm = TRUE) + 
    sum(dataRef$individualsWithoutPingers[which(dataRef$classname == "Holocephali")], na.rm = TRUE)
  
  summaryData$Mammalia[II] <- sum(dataRef$individualsWithPingers[which(dataRef$classname == "Mammalia")], na.rm = TRUE) + 
    sum(dataRef$individualsWithoutPingers[which(dataRef$classname == "Mammalia")], na.rm = TRUE)
  
  summaryData$Reptilia[II] <- sum(dataRef$individualsWithPingers[which(dataRef$classname == "Reptilia")], na.rm = TRUE) + 
    sum(dataRef$individualsWithoutPingers[which(dataRef$classname == "Reptilia")], na.rm = TRUE)
  
  summaryData$Teleostei[II] <- sum(dataRef$individualsWithPingers[which(dataRef$classname == "Teleostei")], na.rm = TRUE) + 
    sum(dataRef$individualsWithoutPingers[which(dataRef$classname == "Teleostei")], na.rm = TRUE)
  
  summaryData$Chondrostei[II] <- sum(dataRef$individualsWithPingers[which(dataRef$classname == "Chondrostei")], na.rm = TRUE) + 
    sum(dataRef$individualsWithoutPingers[which(dataRef$classname == "Chondrostei")], na.rm = TRUE)
  
  summaryData$Petromyzonti[II] <- sum(dataRef$individualsWithPingers[which(dataRef$classname == "Petromyzonti")], na.rm = TRUE) + 
    sum(dataRef$individualsWithoutPingers[which(dataRef$classname == "Petromyzonti")], na.rm = TRUE)
  
  rm(dataRef)
}


### 6.2.3 Restructure data to plot =================================================
# transform the data to have classname as a column
data_toPlot <- data.frame(ecoregion = rep(summaryData$ecoregion, 8),
                          year = rep(summaryData$year, 8),
                          className = c(rep('Aves', nrow(summaryData)),
                                        rep('Elasmobranchii', nrow(summaryData)),
                                        rep('Holocephali', nrow(summaryData)),
                                        rep('Mammalia', nrow(summaryData)),
                                        rep('Reptilia', nrow(summaryData)),
                                        rep('Teleostei', nrow(summaryData)),
                                        rep('Chondrostei', nrow(summaryData)),
                                        rep('Petromyzonti', nrow(summaryData))),
                          bycatch_numInd = c(summaryData$Aves, 
                                             summaryData$Elasmobranchii, 
                                             summaryData$Holocephali, 
                                             summaryData$Mammalia,
                                             summaryData$Reptilia, 
                                             summaryData$Teleostei,
                                             summaryData$Chondrostei, 
                                             summaryData$Petromyzonti))

### 6.2.4 Shorten some names =========================================
data_toPlot <- 
  data_toPlot %>% 
  mutate(ecoregion = case_when(
    ecoregion == 'Aegean-Levantine Sea' ~ 'Aegean-Levantine Sea' , 
    ecoregion == 'Bay of Biscay and the Iberian Coast' ~ 'Bay Biscay & Iberian Coast', 
    ecoregion == 'Ionian Sea and the Central Mediterranean Sea' ~ 'Ionian & Central Mediterranean' , 
    ecoregion == 'Western Mediterranean Sea' ~ 'Western Mediterranean' , 
    ecoregion == 'Oceanic Northeast Atlantic' ~ 'Oceanic NE Atlantic' , 
    .default = ecoregion
  ))


### 6.2.5 From this table, create sub-dataset ===========================

#### birds, mammals, turtles ====
data_toPlot_noFish <- data_toPlot[which(data_toPlot$className == 'Aves' |
                                          data_toPlot$className == 'Mammalia' |
                                          data_toPlot$className == 'Reptilia'), ]

#### sharks and shit ========
data_toPlot_elasmo <- data_toPlot[which(data_toPlot$className == 'Elasmobranchii'), ]


#### All pets ===========

data_toPlot_allPETS <- data_toPlot[which(data_toPlot$className == 'Aves' |
                                           data_toPlot$className == 'Mammalia' |
                                           data_toPlot$className == 'Reptilia' |
                                           data_toPlot$className == 'Elasmobranchii'), ]

## split data in two
data_toPlot_allPETS_a <- data_toPlot_allPETS[which(data_toPlot_allPETS$ecoregion == 'Adriatic Sea' |
                                                     data_toPlot_allPETS$ecoregion == 'Aegean-Levantine Sea' |
                                                     data_toPlot_allPETS$ecoregion == 'Azores' |
                                                     data_toPlot_allPETS$ecoregion == 'Baltic Sea' |
                                                     data_toPlot_allPETS$ecoregion == 'Barents Sea'), ] 

data_toPlot_allPETS_b <- data_toPlot_allPETS[which(data_toPlot_allPETS$ecoregion == 'Bay Biscay & Iberian Coast' |
                                                     data_toPlot_allPETS$ecoregion == 'Black Sea' |
                                                     data_toPlot_allPETS$ecoregion == 'Celtic Seas' |
                                                     data_toPlot_allPETS$ecoregion == 'Faroes' |
                                                     data_toPlot_allPETS$ecoregion == 'Greater North Sea' |
                                                     data_toPlot_allPETS$ecoregion == 'Greenland Sea'), ]
data_toPlot_allPETS_b$bycatch_numInd[which(data_toPlot_allPETS_b$bycatch_numInd > 10000)] <- 10000


data_toPlot_allPETS_c <- data_toPlot_allPETS[which(data_toPlot_allPETS$ecoregion == 'Icelandic Waters' |
                                                     data_toPlot_allPETS$ecoregion == 'Ionian & Central Mediterranean' |
                                                     data_toPlot_allPETS$ecoregion == 'North West Atlantic' |
                                                     data_toPlot_allPETS$ecoregion == 'Norwegian Sea' |
                                                     data_toPlot_allPETS$ecoregion == 'Oceanic NE Atlantic' |
                                                     data_toPlot_allPETS$ecoregion == 'Western Mediterranean'), ]

data_toPlot_allPETS_list = list("data_toPlot_allPETS_a" = data_toPlot_allPETS_a,
                                "data_toPlot_allPETS_b" = data_toPlot_allPETS_b,
                                "data_toPlot_allPETS_c" = data_toPlot_allPETS_c)


# 7. Plot =========================================================================================

## 7.1 number bycatch birds / mammals / turtles =============

plotOut_bmt <- ggplot(data_toPlot_noFish,aes(x = year,y = bycatch_numInd,
                                             fill = className,
                                             label = className)) +
  geom_col(stat = 'identity', position = 'dodge', width = 0.75) +
  scale_fill_manual(name = '', values = c('Aves' = '#F8766D', 
                                          'Mammalia' = '#00BA38',
                                          'Reptilia' = '#619CFF')) +
  
  facet_wrap(~ecoregion, ncol = 3, nrow = 6) +
  
  labs(fill = " ") +
  ylab("Number reported bycatch") +
  xlab(" ") +
  
  scale_x_continuous(breaks = years_unique) +

  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank()) +
  guides(fill = guide_legend(nrow = 1))

ggsave(filename = glue(path_export, 'numBycatch_birdsMammalTurtle.jpg'), 
       plot = plotOut_bmt, width = 6, height = 8)



## 7.2 Elasmo ==================================================================

plotOut_elasmo <- ggplot(data_toPlot_elasmo,aes(x = year,y = bycatch_numInd,
                                                fill = className,
                                                label = className)) +
  geom_col(stat = 'identity', position = 'dodge', width = 0.5) +
  scale_fill_manual(name = '', values = c('Elasmobranchii' = "#E76BF3")) +
  
  facet_wrap(~ecoregion, ncol = 3, nrow = 6) +
  
  labs(fill = " ") +
  ylab("Number reported bycatch") +
  xlab(" ") +
  
  scale_x_continuous(breaks = years_unique) +
  # scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0, 100, 1000, 10000, 60000)) +
  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank()) +
  guides(fill = guide_legend(nrow = 1))

ggsave(filename = glue(path_export, 'numBycatch_elasmo.jpg'), plot = plotOut_elasmo, width = 6, height = 8)



## 7.3 All pets ==================================================================

### 7.3.1 fix y-axis ====================

range(data_toPlot_allPETS$bycatch_numInd[which(data_toPlot_allPETS$className == 'Aves')])
range(data_toPlot_allPETS$bycatch_numInd[which(data_toPlot_allPETS$className == 'Mammalia')])
range(data_toPlot_allPETS$bycatch_numInd[which(data_toPlot_allPETS$className == 'Reptilia')])
range(data_toPlot_allPETS$bycatch_numInd[which(data_toPlot_allPETS$className == 'Elasmobranchii')])

dummyData_yaxis_a <- data.frame(ecoregion = rep(c('Adriatic Sea', 'Aegean-Levantine Sea', 'Azores', 'Baltic Sea', 'Barents Sea'), 4),
                                year = rep(2017, (5*4)),
                                className = c(rep('Aves', 5), rep('Mammalia', 5), rep('Reptilia', 5), rep('Elasmobranchii', 5)),
                                bycatch_numInd = c(rep(1050, 5), rep(1000, 5), rep(170, 5), rep(10000, 5))) #set y-axis max for each species

dummyData_yaxis_b <- data.frame(ecoregion = rep(c('Bay Biscay & Iberian Coast', 'Black Sea', 'Celtic Seas', 'Faroes', 'Greater North Sea', 'Greenland Sea'), 4),
                                year = rep(2017, (6*4)),
                                className = c(rep('Aves', 6), rep('Mammalia', 6), rep('Reptilia', 6), rep('Elasmobranchii', 6)),
                                bycatch_numInd = c(rep(1050, 6), rep(1000, 6), rep(170, 6), rep(10000, 6))) #set y-axis max for each species

dummyData_yaxis_c <- data.frame(ecoregion = rep(c('Icelandic Waters', 'Ionian & Central Mediterranean', 'North West Atlantic', 'Norwegian Sea', 'Oceanic NE Atlantic', 'Western Mediterranean'), 4),
                                year = rep(2017, (6*4)),
                                className = c(rep('Aves', 6), rep('Mammalia', 6), rep('Reptilia', 6), rep('Elasmobranchii', 6)),
                                bycatch_numInd = c(rep(1050, 6), rep(1000, 6), rep(170, 6), rep(10000, 6))) #set y-axis max for each species

dummyData_yaxis_list = list("dummyData_yaxis_a" = dummyData_yaxis_a,
                            "dummyData_yaxis_b" = dummyData_yaxis_b,
                            "dummyData_yaxis_c" = dummyData_yaxis_c)


### 7.3.2 Make plot =================
# Loop to produce the figure a/b/c

lapply(1:3, function(x){
  
  plotOut_all_a <- ggplot(data_toPlot_allPETS_list[[x]], 
                          aes(x = year,
                              y = bycatch_numInd,
                              fill = className,
                              label = className)) +
    geom_col(stat = 'identity', position = 'dodge', width = 0.75) +
    scale_fill_manual(name = '', values = c('Aves' = '#F8766D', 
                                            'Mammalia' = '#00BA38',
                                            'Reptilia' = '#619CFF',
                                            'Elasmobranchii' = '#E76BF3'),
                      labels = c('Aves' = 'Aves', 
                                 'Mammalia' = 'Mammalia',
                                 'Reptilia' = 'Reptilia',
                                 'Elasmobranchii' = 'Elasmobranchii')) +
    
    facet_grid(className ~ ecoregion, scales = "free") +
    geom_point(data = dummyData_yaxis_list[[x]], col = 'white', alpha = 0) +
    
    labs(fill = " ") +
    ylab(" ") +
    xlab(" ") +
    
    scale_x_continuous(breaks = years_unique) +
    
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "right",
          panel.grid.minor.x = element_blank(),
          panel.background = element_blank()) +
    guides(fill = guide_legend(nrow = 4))
  
}) -> data_toPlot_allPETS_plot




### 7.3.3 Combined and save =================

png(filename = glue(path_export, "numBycatch_allPETSplots.png"), width = 30, height = 38, res = 300, units = "cm")
plotLayout = rbind(c(1),
                   c(2),
                   c(3))
grid.arrange(data_toPlot_allPETS_plot[[1]],
             data_toPlot_allPETS_plot[[2]],
             data_toPlot_allPETS_plot[[3]],
             ncol = 1, layout_matrix = plotLayout)
dev.off()


# 8. Clear workspace ====================
rm(data_toPlot_allPETS, dummyData_yaxis_list, dummyData_yaxis_a, dummyData_yaxis_b, dummyData_yaxis_c, data_toPlot_allPETS_plot, dummyData_yaxis_a, dummyData_yaxis_b, dummyData_yaxis_c, dummyData_yaxis_list, data_toPlot_allPETS_plot)
