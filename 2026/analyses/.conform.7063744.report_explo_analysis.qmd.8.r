# 3. Data processing D1 =====================================================================

## 3.1 Data D1 =============================
D1_data <- D1 %>%
    filter(!areaCode %in% c("27.1", "27.6", "27.1~27.2.a~27.2.b", "21.1")) %>%
    mutate(
        areaCode = recode(
            areaCode,
            "27.3.a" = "27.3.a.20",
            "27.5.a" = "27.5.a.2",
            "27.3.d.28" = "27.3.d.28.2",
            "11.1" = "111",
            "11.2" = "112",
            "21.3.l" = "21.3.L",
            "21.3.m" = "21.3.M",
            "21.3.n" = "21.3.N",
            "21.3.k" = "21.3.K",
            "21.3.o" = "21.3.O",
            "21.4.V.s" = "21.4.Vs"
        )
    )
if (include_NAFO_areas) {
    D1_data <- D1_data %>% filter(areaType != "NAFOArea")
}


## 3.2 function to process data ==========================================
process_data <- function(data, group_var, rename_vector) {
    data %>%
        filter(year == analysis_year) %>%
        group_by(!!sym(group_var), areaCode) %>%
        summarize(total_daysAtSeaF = sum(daysAtSeaF, na.rm = TRUE)) %>%
        left_join(ICESareas_laea, by = c("areaCode" = "offname")) %>%
        mutate(
            daysAtSeaF_cat = cut(
                total_daysAtSeaF,
                breaks = c(-Inf, 100, 1000, 2000, 5000, 10000, 25000, Inf),
                labels = c(
                    "< 100",
                    "100-1000",
                    "1000-2000",
                    "2000-5000",
                    "5000-10000",
                    "10000-25000",
                    "> 25000"
                )
            ),
            !!sym(group_var) := recode(!!sym(group_var), !!!rename_vector)
        ) %>%
        filter(!is.na(daysAtSeaF_cat) & !!sym(group_var) != "L3NK")
}


## 3.3 process L3 and L4 data ==========================================
daysAtSea_spatial <- process_data(D1_data, "metierL3", rename_vector)
daysAtSea_L4_spatial <- process_data(D1_data, "metierL4", rename_vector)

hist(
    daysAtSea_spatial$total_daysAtSeaF,
    breaks = seq(from = 0, to = 1000000, by = 100),
    xlim = c(0, 10000)
)
hist(
    daysAtSea_L4_spatial$total_daysAtSeaF,
    breaks = seq(from = 0, to = 1000000, by = 100),
    xlim = c(0, 10000)
)


# 3. Data processing D2 =====================================================================

## 3.1 Data D2 ==================
D2_data <- D2 %>%
    filter(!areaCode %in% c("27.1", "27.6", "27.1~27.2.a~27.2.b", "21.1")) %>%
    mutate(
        areaCode = recode(
            areaCode,
            "27.3.a" = "27.3.a.20",
            "27.5.a" = "27.5.a.2",
            "27.3.d.28" = "27.3.d.28.2",
            "11.1" = "111",
            "11.2" = "112",
            "21.3.l" = "21.3.L",
            "21.3.m" = "21.3.M",
            "21.3.n" = "21.3.N",
            "21.3.k" = "21.3.K",
            "21.3.o" = "21.3.O",
            "21.4.V.s" = "21.4.Vs"
        )
    )

## exclude all log books (LB), port observers (PO), and all log books (except for Norway)
indRef <- which(
    D2_data$monitoringMethod == 'SO' | #all observer data
        D2_data$monitoringMethod == 'VO' & D2_data$country != 'EE' | #include vessel crew except Estonia
        D2_data$monitoringMethod == 'EM' | #all electronic monitoring data
        D2_data$monitoringMethod == 'OTH' & D2_data$country == 'NO'
) #include other only for Norway

D2_data <- D2_data[indRef, ]
rm(indRef)

## 3.2 Filtert Data
daysAtSea_Ob <- D2_data %>%
    filter(year == analysis_year) %>%
    group_by(metierL3, areaCode) %>%
    summarize(daysAtSea_Ob = sum(daysAtSeaOb, na.rm = TRUE)) %>%
    left_join(ICESareas_laea, by = c("areaCode" = "offname"))


## 3.2 assign breaks for plotting and sort data accordingly ====
daysAtSea_Ob <- daysAtSea_Ob %>%
    mutate(
        daysAtSea_Ob_cat = cut(
            daysAtSea_Ob,
            breaks = c(-Inf, 5, 10, 20, 50, 100, 500, Inf),
            labels = c("<5", "5-10", "10-20", "20-50", "50-100", "100-500", ">500")
        ),
        metierL3 = recode(metierL3, !!!rename_vector)
    ) %>%
    filter(!is.na(daysAtSea_Ob_cat) & metierL3 != "L3NK")


## 3.3 monitoring effort Metier L4 ==============
daysAtSea_L4_Ob <- D2_data %>%
    filter(year == analysis_year) %>%
    group_by(metierL4, areaCode) %>%
    summarize(daysAtSea_Ob = sum(daysAtSeaOb, na.rm = TRUE)) %>%
    left_join(ICESareas_laea, by = c("areaCode" = "offname")) %>%
    mutate(
        daysAtSea_L4_Ob_cat = cut(
            daysAtSea_Ob,
            breaks = c(-Inf, 5, 10, 20, 50, 100, 500, Inf),
            labels = c("<5", "5-10", "10-20", "20-50", "50-100", "100-500", ">500")
        ),
        metierL4 = recode(metierL4, !!!rename_vector)
    ) %>%
    filter(!is.na(daysAtSea_L4_Ob_cat) & metierL4 != "L4NK")


## 3.4 observer coverage L3 =========================
matchedDaysAtSea <- full_join(
    daysAtSea_spatial,
    daysAtSea_Ob,
    by = c("areaCode", "metierL3")
)
hist(
    (matchedDaysAtSea$daysAtSea_Ob / matchedDaysAtSea$total_daysAtSeaF) * 100,
    breaks = seq(from = 0, to = 100, by = 0.1),
    xlim = c(0, 2)
)

spatialCoverage <- left_join(
    ICESareas_laea,
    matchedDaysAtSea,
    by = c("offname" = "areaCode")
) %>%
    mutate(
        Obs_Coverage = (daysAtSea_Ob / total_daysAtSeaF) * 100,
        Obs_Coverage_cat = cut(
            Obs_Coverage,
            breaks = c(-Inf, 0.25, 0.5, 0.75, 1, 2, 4, Inf),
            labels = c(
                "< 0.25",
                "0.25-0.5",
                "0.5-0.75",
                "0.75-1.0",
                "1.0-2.0",
                "2.0-4.0",
                "> 4.0"
            )
        )
    ) %>%
    filter(!is.na(Obs_Coverage_cat) & metierL3 != "L3NK") %>%
    mutate(metierL3 = recode(metierL3, !!!rename_vector))


## 3.5 Observer coverage L4 ==================================
matchedDaysAtSea_L4 <- full_join(
    daysAtSea_L4_spatial,
    daysAtSea_L4_Ob,
    by = c("areaCode", "metierL4")
)
spatialCoverage_L4 <- left_join(
    ICESareas_laea,
    matchedDaysAtSea_L4,
    by = c("offname" = "areaCode")
) %>%
    mutate(
        Obs_Coverage = (daysAtSea_Ob / total_daysAtSeaF) * 100,
        Obs_Coverage_cat = cut(
            Obs_Coverage,
            breaks = c(-Inf, 0.1, 0.25, 0.5, 0.75, 1, 2, Inf),
            labels = c(
                "< 0.1",
                "0.1-0.25",
                "0.25-0.5",
                "0.5-0.75",
                "0.75-1.0",
                "1.0-2.0",
                "> 2.0"
            )
        )
    ) %>%
    filter(!is.na(Obs_Coverage_cat) & metierL4 != "L4NK") %>%
    mutate(metierL4 = recode(metierL4, !!!rename_vector))
