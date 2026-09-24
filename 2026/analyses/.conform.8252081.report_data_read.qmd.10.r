#long-table-container style="max-height: 500px; overflow-y: auto;"

#big long useless table
#Annex_table
tab2 <- D2 %>%
    filter(year == currentyear) %>%
    group_by(ecoregion, areaCode, metierL3, monitoringMethod) %>%
    summarise(dasobs = sum(daysAtSeaOb, na.rm = T)) %>%
    ungroup()
tab1 <- D1 %>%
    filter(year == currentyear) %>%
    group_by(ecoregion, metierL3, areaCode) %>%
    summarise(dastot = sum(daysAtSeaF, na.rm = T)) %>%
    ungroup()
tab3 <- D3 %>%
    filter(year == currentyear) %>%
    group_by(ecoregion, areaCode, metierL3, monitoringMethod, species, classname) %>%
    summarise(
        nb = sum(individualsWithPingers, individualsWithoutPingers, na.rm = T),
        inc = sum(incidentsWithPingers, incidentsWithoutPingers, na.rm = T)
    ) %>%
    ungroup()

tab123 <- left_join(left_join(tab2, tab1), tab3) %>%
    filter(!is.na(species)) %>%
    transmute(
        ecoregion,
        areaCode,
        metierL3,
        dastot,
        monitoringMethod,
        dasobs,
        moncov = 100 * (dasobs / dastot),
        classname,
        species,
        nb,
        inc
    ) %>%
    mutate(dasobs = ifelse(dasobs == 0, NA, dasobs)) %>%
    mutate(moncov = ifelse(dasobs == 0, NA, moncov)) %>%
    transmute(
        Ecoregion = ecoregion,
        AreaCode = areaCode,
        MetierL3 = metierL3,
        `Fishing Effort (das)` = dastot,
        MonitoringMethod = monitoringMethod,
        `Total Observed Effort (das)` = dasobs,
        `Monitoring Coverage (%)` = moncov,
        classname,
        Species = species,
        Total_No_Specimens = nb,
        Incidents = inc
    )

head(tab123)

#write excel file
wb <- createWorkbook()
addWorksheet(wb, 1)
writeData(wb, 1, tab123)
saveWorkbook(
    wb,
    file = here("outputs", "TOR_A_long_table_bycatch_only.xlsx"),
    overwrite = T
)

#big long useless table with all info
#Annex_table
tab2 <- D2 %>%
    filter(year == currentyear) %>%
    group_by(ecoregion, areaCode, metierL3, monitoringMethod) %>%
    summarise(dasobs = sum(daysAtSeaOb, na.rm = T)) %>%
    ungroup()
tab1 <- D1 %>%
    filter(year == currentyear) %>%
    group_by(ecoregion, metierL3, areaCode) %>%
    summarise(dastot = sum(daysAtSeaF, na.rm = T)) %>%
    ungroup()
tab3 <- D3 %>%
    filter(year == currentyear) %>%
    group_by(ecoregion, areaCode, metierL3, monitoringMethod, species, classname) %>%
    summarise(
        nb = sum(individualsWithPingers, individualsWithoutPingers, na.rm = T),
        inc = sum(incidentsWithPingers, incidentsWithoutPingers, na.rm = T)
    ) %>%
    ungroup()

tab123 <- full_join(full_join(tab2, tab1), tab3) %>% #filter(!is.na(species))%>%
    transmute(
        ecoregion,
        areaCode,
        metierL3,
        dastot,
        monitoringMethod,
        dasobs,
        moncov = 100 * (dasobs / dastot),
        classname,
        species,
        nb,
        inc
    ) %>%
    mutate(dasobs = ifelse(dasobs == 0, NA, dasobs)) %>%
    mutate(moncov = ifelse(dasobs == 0, NA, moncov)) %>%
    transmute(
        Ecoregion = ecoregion,
        AreaCode = areaCode,
        MetierL3 = metierL3,
        `Fishing Effort (das)` = dastot,
        MonitoringMethod = monitoringMethod,
        `Total Observed Effort (das)` = dasobs,
        `Monitoring Coverage (%)` = moncov,
        classname,
        Species = species,
        Total_No_Specimens = nb,
        Incidents = inc
    )

#write excel file
wb <- createWorkbook()
addWorksheet(wb, 1)
writeData(wb, 1, tab123)
saveWorkbook(wb, file = here("outputs", "TOR_A_long_table.xlsx"), overwrite = T)

#short table
tab2 <- D2 %>%
    filter(year == currentyear) %>%
    group_by(ecoregion, metierL3, monitoringMethod) %>%
    summarise(dasobs = sum(daysAtSeaOb, na.rm = T)) %>%
    ungroup()
tab1 <- D1 %>%
    filter(year == currentyear) %>%
    group_by(ecoregion, metierL3) %>%
    summarise(dastot = sum(daysAtSeaF, na.rm = T)) %>%
    ungroup()
tab3 <- D3 %>%
    filter(year == currentyear) %>%
    group_by(ecoregion, metierL3, monitoringMethod, species, classname) %>%
    summarise(
        nb = sum(individualsWithPingers, individualsWithoutPingers, na.rm = T),
        inc = sum(incidentsWithPingers, incidentsWithoutPingers, na.rm = T)
    ) %>%
    ungroup()

tab123 <- left_join(left_join(tab2, tab1), tab3) %>%
    filter(!is.na(species)) %>%
    transmute(
        ecoregion,
        metierL3,
        dastot,
        monitoringMethod,
        dasobs,
        moncov = 100 * (dasobs / dastot),
        classname,
        species,
        nb,
        inc
    ) %>%
    mutate(dasobs = ifelse(dasobs == 0, NA, dasobs)) %>%
    mutate(moncov = ifelse(dasobs == 0, NA, moncov)) %>%
    transmute(
        Ecoregion = ecoregion,
        MetierL3 = metierL3,
        `Fishing Effort (das)` = dastot,
        MonitoringMethod = monitoringMethod,
        `Total Observed Effort (das)` = dasobs,
        `Monitoring Coverage (%)` = moncov,
        classname,
        Species = species,
        Total_No_Specimens = nb,
        Incidents = inc
    )

#write excel file
wb <- createWorkbook()
addWorksheet(wb, 1)
writeData(wb, 1, tab123)
saveWorkbook(wb, file = here("outputs", "TOR_A_short_table.xlsx"), overwrite = T)


#shorter table
tab2 <- D2 %>%
    filter(year == currentyear) %>%
    group_by(ecoregion) %>%
    summarise(dasobs = sum(daysAtSeaOb, na.rm = T)) %>%
    ungroup()
tab1 <- D1 %>%
    filter(year == currentyear) %>%
    group_by(ecoregion) %>%
    summarise(dastot = sum(daysAtSeaF, na.rm = T)) %>%
    ungroup()
tab3 <- D3 %>%
    filter(year == currentyear) %>%
    mutate(classname = ifelse(is.na(classname), "Reptiles", classname)) %>%
    group_by(Ecoregion = ecoregion, classname) %>%
    summarise(
        nbspp = n_distinct(species),
        nb = sum(individualsWithPingers, individualsWithoutPingers, na.rm = T),
        inc = sum(incidentsWithPingers, incidentsWithoutPingers, na.rm = T)
    ) %>%
    ungroup() %>%
    tidyr::pivot_longer(nbspp:inc, names_to = "variable") %>%
    tidyr::pivot_wider(values_from = value, names_from = classname)
tab3$variable[tab3$variable == "nbspp"] <- "Species"
tab3$variable[tab3$variable == "nb"] <- "Individuals"
tab3$variable[tab3$variable == "inc"] <- "Incidents"


tab123 <- left_join(tab1, tab2) %>%
    filter(!is.na(ecoregion)) %>%
    transmute(
        Ecoregion = ecoregion,
        `Fishing Effort (das)` = dastot,
        `Total Observed Effort (das)` = dasobs,
        `Monitoring Coverage (%)` = 100 * (dasobs / dastot)
    ) %>%
    left_join(tab3) %>%
    filter(Ecoregion != "")


#write excel file
wb <- createWorkbook()
addWorksheet(wb, 1)
writeData(wb, 1, tab123)
saveWorkbook(wb, file = here("outputs", "TOR_A_shorter_table.xlsx"), overwrite = T)

pipo <- flextable(tab123) %>%
    merge_v(
        j = ~ Ecoregion +
            `Fishing Effort (das)` +
            `Total Observed Effort (das)` +
            `Monitoring Coverage (%)`
    ) %>%
    hline(i = seq(3, nrow(tab123), 3)) %>%
    valign(j = 1:4, valign = "top") %>%
    colformat_double(j = 4, digits = 2) %>%
    colformat_double(j = c(2:3, 5:ncol(tab123)), digits = 0) %>%
    bg(part = "header", bg = "grey30") %>%
    color(part = "header", color = "ghostwhite") %>%
    italic(j = 1) %>%
    fit_to_width(max_width = 11.7)
pipo

#save as docx object
save_as_docx(
    pipo,
    path = here("outputs", "TOR_A_shorter_table.docx"),
    pr_section = prop_section(
        page_size = page_size(orient = "landscape"),
        page_margins = page_mar(
            bottom = 0,
            top = 0,
            right = 0,
            left = 0,
            header = 0,
            footer = 0,
            gutter = 0
        )
    )
)


#shorter table detailed effort
tab2 <- D2 %>%
    filter(year == currentyear) %>%
    group_by(ecoregion, metierL3_name) %>%
    summarise(dasobs = sum(daysAtSeaOb, na.rm = T)) %>%
    ungroup()
tab1 <- D1 %>%
    filter(year == currentyear) %>%
    group_by(ecoregion, metierL3_name) %>%
    summarise(dastot = sum(daysAtSeaF, na.rm = T)) %>%
    ungroup()
tab12 <- full_join(tab1, tab2) %>%
    mutate(moncov = 100 * (dasobs / dastot)) %>%
    mutate(moncov = ifelse(is.finite(moncov), moncov, 0)) %>%
    mutate(info = paste0(round(dastot, 0), " das (", round(moncov, 3), "% cov.)")) %>%
    select(ecoregion, metierL3_name, info)
tab3 <- D3 %>%
    filter(year == currentyear) %>%
    group_by(ecoregion, metierL3_name) %>%
    summarise(inc = sum(incidentsWithPingers, incidentsWithoutPingers, na.rm = T)) %>%
    mutate(inc = ifelse(is.finite(inc), inc, 0)) %>%
    ungroup()
tab123 <- full_join(tab12, tab3) %>%
    mutate(inc = ifelse(is.finite(inc), inc, 0)) %>%
    mutate(info = paste0(info, " das/ ", inc, " inc.")) %>%
    select(ecoregion, metierL3_name, info) %>%
    pivot_wider(values_from = info, names_from = metierL3_name) %>%
    filter(!is.na(ecoregion))


flextable(tab123) %>%
    autofit() %>%
    set_table_properties(
        opts_html = list(
            scroll = list(
                height = "400px", # Limits height and enables vertical scroll
                freeze_first_column = TRUE # Keeps first column visible when scrolling right
            )
        )
    )
#write excel file
wb <- createWorkbook()
addWorksheet(wb, 1)
writeData(wb, 1, tab123)
saveWorkbook(
    wb,
    file = here("output", "TOR_A_cool_effort_shorter_table.xlsx"),
    overwrite = T
)
