country_unique <- unique(D3$country)
years_unique <- c(2017:2025)

D3 <-
    D3 %>%
    mutate(
        classname = case_when(
            superClass == "Reptilia" & is.na(D3$classname) ~ 'Reptilia',
            .default = classname
        )
    )
# Create a summary data empty
summaryData <- expand.grid(years_unique, country_unique) %>%
    rename(year = Var1, country = Var2) %>%
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

# Count the number of bycatch by Year / Ecoregion / Order =======
# The loop goes row by row to fill the summurized information

for (II in 1:nrow(summaryData)) {
    dataRef <- D3[
        which(D3$country == summaryData$country[II] & D3$year == summaryData$year[II]),
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


# Restructure data to plot =================================================
# transform the data to have classname as a column
data_toPlot <- data.frame(
    country = rep(summaryData$country, 8),
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

# From this table, create sub-dataset ===========================
#birds, mammals, turtles ====
data_toPlot_noFish <- data_toPlot[
    which(
        data_toPlot$className == 'Aves' |
            data_toPlot$className == 'Mammalia' |
            data_toPlot$className == 'Reptilia'
    ),
]
# sharks and shit ========
data_toPlot_elasmo <- data_toPlot[which(data_toPlot$className == 'Elasmobranchii'), ]
# All pets ===========
data_toPlot_allPETS <- data_toPlot[
    which(
        data_toPlot$className == 'Aves' |
            data_toPlot$className == 'Mammalia' |
            data_toPlot$className == 'Reptilia' |
            data_toPlot$className == 'Elasmobranchii'
    ),
]
