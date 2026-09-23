# prepare data for fig 3.3 (reported monitoring method)
#all
monmethall <- D2 %>%
    group_by(monitoringMethod_name, year) %>%
    summarise(das = sum(daysAtSeaOb, na.rm = T), .groups = "drop") %>%
    group_by(year) %>%
    mutate(tot = sum(das)) %>%
    ungroup() %>%
    mutate(reldas = das / tot * 100) %>%
    mutate(type = "WGBYC database")
#BEAM filtering only
monmethbeam <- D2 %>%
    filter(!is.na(ecoregion) & !(ecoregion %in% c("North West Atlantic"))) %>%
    filter(!(monitoringMethod == "LB" & country != "PT")) %>%
    filter(!(monitoringMethod == "LB" & country != "PT-20")) %>%
    filter(!(monitoringMethod == "OTH" & country != "NO")) %>%
    filter(monitoringMethod != "PO") %>%
    filter(!(monitoringMethod == "VO" & country == "EE")) %>%
    group_by(monitoringMethod_name, year) %>%
    summarise(das = sum(daysAtSeaOb, na.rm = T), .groups = "drop") %>%
    group_by(year) %>%
    mutate(tot = sum(das)) %>%
    ungroup() %>%
    mutate(reldas = das / tot * 100) %>%
    mutate(type = "WGBYC databse with BEAM filtering")
monmethall <- rbind(monmethall, monmethbeam)

#plt output here
#monitoring methods overview global + BEAM filtering
plt <- ggplot(
    monmethall,
    aes(
        x = year,
        y = reldas,
        stratum = monitoringMethod_name,
        alluvium = monitoringMethod_name,
        fill = monitoringMethod_name,
        label = monitoringMethod_name
    )
) +
    geom_flow(stat = "alluvium") +
    geom_stratum() +
    facet_grid(~type) +
    theme_bw() +
    labs(fill = "Monitoring method") +
    ylab("Proportion of annual DAS by monitoring method (%)")


#by ctry
monmethctry <- D2 %>%
    group_by(monitoringMethod_name, country_name, year) %>%
    summarise(das = sum(daysAtSeaOb, na.rm = T), .groups = "drop") %>%
    group_by(year, country_name) %>%
    mutate(tot = sum(das)) %>%
    ungroup() %>%
    mutate(reldas = das / tot * 100)


print(plt)
textcaption <- "Total monitored (observed) days at sea reported in the WGBYC database and in the data filtered to compute BEAM estimatess per monitoring method (2017-2025) at-sea-observers, electronic monitoring, port observers, and vessel crew observers, logbooks, other. The BEAM filtered for use in ToR C (see specific section, BEAM): at-sea-observers, electronic monitoring, vessel crew observers except for Estonia, and other only for Norway."
print(textcaption)
filename <- here("outputs", "monitoring_method_overview.png")
png(filename, res = 300, width = 18, height = 18, unit = "cm")
print(plt)
dev.off()
#save as docx object
read_docx() |>
    body_add_img(
        src = filename,
        height = 18,
        width = 18,
        unit = "cm",
        style = "centered"
    ) |>
    body_add_par(value = textcaption, style = "centered") |>
    print(target = sub(".png", ".docx", filename))
