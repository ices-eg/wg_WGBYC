# Select parameters =======================================

ecoregions_unique <- unique(D3$ecoregion)
years_unique <- c(2017:2025)

# Clean D3 ===============================================

D3 <- D3 %>%
    mutate(
        classname = case_when(
            superClass == "Reptilia" & is.na(D3$classname) ~ 'Reptilia',
            .default = classname
        )
    )

# Create a summary data empty ==============================
summaryData <- expand.grid(years_unique, ecoregions_unique) %>%
    rename(year = Var1, ecoregion = Var2) %>%
    data.frame() %>%
    mutate(
        Aves = NA,
        Elasmobranchii = NA,
        Holocephali = NA,
        Mammalia = NA,
        Reptilia = NA,
        Teleostei = NA,
        Chondrostei = NA,
        Petromyzonti = NA
    )

# Count the number of bycatch by Year / Ecoregion / Order
# The loop goes row by row to fill the summurized information

for (II in 1:nrow(summaryData)) {
    dataRef <- D3[
        which(
            D3$ecoregion == summaryData$ecoregion[II] & D3$year == summaryData$year[II]
        ),
    ]

    summaryData$Aves[II] <- sum(
        dataRef$individualsWithPingers[which(dataRef$classname == "Aves")],
        na.rm = TRUE
    ) +
        sum(
            dataRef$individualsWithoutPingers[which(dataRef$classname == "Aves")],
            na.rm = TRUE
        )

    summaryData$Elasmobranchii[II] <- sum(
        dataRef$individualsWithPingers[which(dataRef$classname == "Elasmobranchii")],
        na.rm = TRUE
    ) +
        sum(
            dataRef$individualsWithoutPingers[which(
                dataRef$classname == "Elasmobranchii"
            )],
            na.rm = TRUE
        )

    summaryData$Holocephali[II] <- sum(
        dataRef$individualsWithPingers[which(dataRef$classname == "Holocephali")],
        na.rm = TRUE
    ) +
        sum(
            dataRef$individualsWithoutPingers[which(dataRef$classname == "Holocephali")],
            na.rm = TRUE
        )

    summaryData$Mammalia[II] <- sum(
        dataRef$individualsWithPingers[which(dataRef$classname == "Mammalia")],
        na.rm = TRUE
    ) +
        sum(
            dataRef$individualsWithoutPingers[which(dataRef$classname == "Mammalia")],
            na.rm = TRUE
        )

    summaryData$Reptilia[II] <- sum(
        dataRef$individualsWithPingers[which(dataRef$classname == "Reptilia")],
        na.rm = TRUE
    ) +
        sum(
            dataRef$individualsWithoutPingers[which(dataRef$classname == "Reptilia")],
            na.rm = TRUE
        )

    summaryData$Teleostei[II] <- sum(
        dataRef$individualsWithPingers[which(dataRef$classname == "Teleostei")],
        na.rm = TRUE
    ) +
        sum(
            dataRef$individualsWithoutPingers[which(dataRef$classname == "Teleostei")],
            na.rm = TRUE
        )

    summaryData$Chondrostei[II] <- sum(
        dataRef$individualsWithPingers[which(dataRef$classname == "Chondrostei")],
        na.rm = TRUE
    ) +
        sum(
            dataRef$individualsWithoutPingers[which(dataRef$classname == "Chondrostei")],
            na.rm = TRUE
        )

    summaryData$Petromyzonti[II] <- sum(
        dataRef$individualsWithPingers[which(dataRef$classname == "Petromyzonti")],
        na.rm = TRUE
    ) +
        sum(
            dataRef$individualsWithoutPingers[which(dataRef$classname == "Petromyzonti")],
            na.rm = TRUE
        )

    rm(dataRef)
}


# Restructure data to plot
# transform the data to have classname as a column
data_toPlot <- data.frame(
    ecoregion = rep(summaryData$ecoregion, 8),
    year = rep(summaryData$year, 8),
    className = c(
        rep('Aves', nrow(summaryData)),
        rep('Elasmobranchii', nrow(summaryData)),
        rep('Holocephali', nrow(summaryData)),
        rep('Mammalia', nrow(summaryData)),
        rep('Reptilia', nrow(summaryData)),
        rep('Teleostei', nrow(summaryData)),
        rep('Chondrostei', nrow(summaryData)),
        rep('Petromyzonti', nrow(summaryData))
    ),
    bycatch_numInd = c(
        summaryData$Aves,
        summaryData$Elasmobranchii,
        summaryData$Holocephali,
        summaryData$Mammalia,
        summaryData$Reptilia,
        summaryData$Teleostei,
        summaryData$Chondrostei,
        summaryData$Petromyzonti
    )
)

# shorten some names =========================================
data_toPlot <-
    data_toPlot %>%
    mutate(
        ecoregion = case_when(
            ecoregion == 'Aegean-Levantine Sea' ~ 'Aegean-Levantine Sea',
            ecoregion ==
                'Bay of Biscay and the Iberian Coast' ~ 'Bay Biscay & Iberian Coast',
            ecoregion ==
                'Ionian Sea and the Central Mediterranean Sea' ~ 'Ionian & Central Mediterranean',
            ecoregion == 'Western Mediterranean Sea' ~ 'Western Mediterranean',
            ecoregion == 'Oceanic Northeast Atlantic' ~ 'Oceanic NE Atlantic',
            .default = ecoregion
        )
    )

# From this table, create sub-dataset
# birds, mammals, turtles
data_toPlot_noFish <- data_toPlot[
    which(
        data_toPlot$className == 'Aves' |
            data_toPlot$className == 'Mammalia' |
            data_toPlot$className == 'Reptilia'
    ),
]
# sharks
data_toPlot_elasmo <- data_toPlot[which(data_toPlot$className == 'Elasmobranchii'), ]
# All pets
data_toPlot_allPETS <- data_toPlot[
    which(
        data_toPlot$className == 'Aves' |
            data_toPlot$className == 'Mammalia' |
            data_toPlot$className == 'Reptilia' |
            data_toPlot$className == 'Elasmobranchii'
    ),
]
## split data in two
data_toPlot_allPETS_a <- data_toPlot_allPETS[
    which(
        data_toPlot_allPETS$ecoregion == 'Adriatic Sea' |
            data_toPlot_allPETS$ecoregion == 'Aegean-Levantine Sea' |
            data_toPlot_allPETS$ecoregion == 'Azores' |
            data_toPlot_allPETS$ecoregion == 'Baltic Sea' |
            data_toPlot_allPETS$ecoregion == 'Barents Sea'
    ),
]

data_toPlot_allPETS_b <- data_toPlot_allPETS[
    which(
        data_toPlot_allPETS$ecoregion == 'Bay Biscay & Iberian Coast' |
            data_toPlot_allPETS$ecoregion == 'Black Sea' |
            data_toPlot_allPETS$ecoregion == 'Celtic Seas' |
            data_toPlot_allPETS$ecoregion == 'Faroes' |
            data_toPlot_allPETS$ecoregion == 'Greater North Sea' |
            data_toPlot_allPETS$ecoregion == 'Greenland Sea'
    ),
]
data_toPlot_allPETS_b$bycatch_numInd[which(
    data_toPlot_allPETS_b$bycatch_numInd > 10000
)] <- 10000


data_toPlot_allPETS_c <- data_toPlot_allPETS[
    which(
        data_toPlot_allPETS$ecoregion == 'Icelandic Waters' |
            data_toPlot_allPETS$ecoregion == 'Ionian & Central Mediterranean' |
            data_toPlot_allPETS$ecoregion == 'North West Atlantic' |
            data_toPlot_allPETS$ecoregion == 'Norwegian Sea' |
            data_toPlot_allPETS$ecoregion == 'Oceanic NE Atlantic' |
            data_toPlot_allPETS$ecoregion == 'Western Mediterranean'
    ),
]

data_toPlot_allPETS_list = list(
    "data_toPlot_allPETS_a" = data_toPlot_allPETS_a,
    "data_toPlot_allPETS_b" = data_toPlot_allPETS_b,
    "data_toPlot_allPETS_c" = data_toPlot_allPETS_c
)
