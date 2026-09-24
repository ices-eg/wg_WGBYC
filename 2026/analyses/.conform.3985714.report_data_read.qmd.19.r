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
textcaption <- "test"
print(textcaption)
filename <- here("outputs", "monitoring_method_ecoregion.png")
print(plt)
png(filename, res = 300, width = 18, height = 14, unit = "cm")
print(plt)
dev.off()
#save as docx object
read_docx() |>
    body_add_img(
        src = filename,
        height = 14,
        width = 16,
        unit = "cm",
        style = "centered"
    ) |>
    body_add_par(value = textcaption, style = "centered") |>
    print(target = sub(".png", ".docx", filename))
