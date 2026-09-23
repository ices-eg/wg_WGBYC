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
textcaption <- "Total monitored (observed) days at sea reported in the WGBYC database and in the data filtered to compute BEAM estimatess per monitoring method (2017-2025) at-sea-observers,
electronic monitoring, port observers, and vessel crew observers, logbooks, other. The BEAM 
filtered for use in ToR C (see specific section, BEAM): at-sea-observ-
ers, electronic monitoring, vessel crew observers except for Estonia, and
other only for Norway."
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

#by country
plt <- ggplot(
    monmethctry,
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
    facet_wrap(~country_name) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(fill = "Monitoring method") +
    ylab("Proportion of annual DAS by monitoring method (%)")
#keep 1.8 fix country
print(plt)
png(
    here("outputs", "monitoring_method_country.png"),
    res = 300,
    width = 1600,
    height = 1200,
    unit = "px"
)
print(plt)
dev.off()

#moniroting by program
monitoringProgramType_namectry <- D2 %>%
    group_by(monitoringProgramType_name, country_name, year) %>%
    summarise(das = sum(daysAtSeaOb, na.rm = T), .groups = "drop") %>%
    group_by(year, country_name) %>%
    mutate(tot = sum(das)) %>%
    ungroup() %>%
    mutate(reldas = das / tot * 100)
#a graph
plt <- ggplot(
    monitoringProgramType_namectry,
    aes(
        x = year,
        y = reldas,
        stratum = monitoringProgramType_name,
        alluvium = monitoringProgramType_name,
        fill = monitoringProgramType_name,
        label = monitoringProgramType_name
    )
) +
    geom_flow(stat = "alluvium") +
    geom_stratum() +
    facet_wrap(~country_name) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(fill = "Monitoring program") +
    ylab("Proportion of annual DAS by monitoring program (%)")

#keep 1.9
print(plt)
png(
    here("outputs", "monitoring_program_country.png"),
    res = 300,
    width = 1600,
    height = 1200,
    unit = "px"
)
print(plt)
dev.off()

#
# incidents by montoring program and species class
#
pipo <- D3
pipo$nbinc <- apply(
    D3 %>% select(incidentsWithPingers, incidentsWithoutPingers),
    1,
    sum,
    na.rm = T
)
pipo <- pipo %>%
    mutate(classname = ifelse(is.na(classname), "Reptiles", classname)) %>%
    mutate(
        newclass = ifelse(grepl("Aves|Elasmo|Mamma|Rept", classname), classname, "Other")
    )

incprog <- pipo %>%
    group_by(monitoringMethod_name, year, newclass) %>%
    summarise(inc = sum(nbinc, na.rm = T), .groups = "drop") %>%
    ungroup()
#a graph
plt <- ggplot(
    incprog,
    aes(
        x = as.factor(year),
        y = inc,
        stratum = monitoringMethod_name,
        alluvium = monitoringMethod_name,
        fill = monitoringMethod_name,
        label = monitoringMethod_name
    )
) +
    geom_flow(stat = "alluvium") +
    geom_stratum() +
    facet_wrap(~newclass, scale = "free") +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "inside",
        legend.position.inside = c(.85, 0.25)
    ) +
    labs(fill = "Monitoring method") +
    ylab("Number of reported incidents") +
    xlab("")

#keep 1.10 keep
print(plt)
png(
    here("outputs", "incidents_by_monitoringProgram.png"),
    res = 300,
    width = 1600,
    height = 1200,
    unit = "px"
)
print(plt)
dev.off()


#prepare data for sample coverage by monitoring effort by gear by georegions
monmethgear <- D2 %>%
    group_by(monitoringMethod_name, ecoregion, metierL3_name, year) %>%
    summarise(das = sum(daysAtSeaOb, na.rm = T), .groups = "drop")

#monitoring methods: ecoregion/gear view
monmethgear1 <- D2 %>%
    group_by(monitoringMethod_name, ecoregion, metierL3_name, year) %>%
    summarise(das = sum(daysAtSeaOb, na.rm = T), .groups = "drop") %>%
    filter(!monitoringMethod_name %in% c("Logbook", "Port observers"))
monmethgear2 <- monmethgear1 %>%
    mutate(metierL3_name = "All metier") %>%
    group_by(monitoringMethod_name, ecoregion, metierL3_name, year) %>%
    summarise(das = sum(das, na.rm = T), .groups = "drop")
monmethgear <- rbind(monmethgear1, monmethgear2) %>%
    mutate(monitoringMethod_name = as.factor(monitoringMethod_name))


listecoregion <- na.omit(unique(monmethgear$ecoregion))
for (i in listecoregion) {
    tmp <- monmethgear[monmethgear$ecoregion == i, ] %>%
        filter(!is.na(metierL3_name))
    plt <- ggplot(
        tmp,
        aes(
            x = year,
            y = das,
            stratum = monitoringMethod_name,
            alluvium = monitoringMethod_name,
            fill = monitoringMethod_name,
            label = monitoringMethod_name
        )
    ) +
        geom_flow(stat = "alluvium") +
        geom_stratum() +
        facet_wrap(~metierL3_name, scale = "free") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        labs(fill = "Monitoring method") +
        ggtitle(i) +
        ylab("Sum of annual DAS by monitoring method")
    print(plt)
    filename <- here("outputs", paste0("monitoring_method_", gsub(" ", "", i), ".png"))
    png(filename, res = 300, width = 1600, height = 1200, unit = "px")
    print(plt)
    dev.off()
}

#prepare data for sample coverage by monitoring effort by georegions
monmethecoregion <- D2 %>%
    group_by(monitoringMethod_name, ecoregion, year) %>%
    summarise(das = sum(daysAtSeaOb, na.rm = T), .groups = "drop") %>%
    filter(!is.na(ecoregion))

plt <- ggplot(
    monmethecoregion %>% mutate(ecoregion = sub(" ", "\n", ecoregion)),
    aes(
        x = year,
        y = das,
        stratum = monitoringMethod_name,
        alluvium = monitoringMethod_name,
        fill = monitoringMethod_name,
        label = monitoringMethod_name
    )
) +
    geom_flow(stat = "alluvium") +
    geom_stratum() +
    facet_wrap(~ecoregion, scale = "free") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(fill = "Monitoring method") +
    ggtitle("Monitoring program by ecoregion") +
    ylab("Sum of annual DAS by monitoring method")
filename <- here("outputs", "monitoring_method_ecoregion.png")
print(plt)
png(filename, res = 300, width = 1600, height = 1200, unit = "px")
print(plt)
dev.off()
