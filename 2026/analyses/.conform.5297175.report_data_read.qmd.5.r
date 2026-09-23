# prepare data for fig 3.3 (reported monitoring method)
#all
monmethall <- D2 %>%
    group_by(monitoringMethod_name, year) %>%
    summarise(trips = sum(tripsOb, na.rm = T), .groups = "drop") %>%
    group_by(year) %>%
    mutate(tot = sum(trips)) %>%
    ungroup() %>%
    mutate(reltrips = trips / tot * 100)
#by ctry
monmethctry <- D2 %>%
    group_by(monitoringMethod_name, country_name, year) %>%
    summarise(trips = sum(tripsOb, na.rm = T), .groups = "drop") %>%
    group_by(year, country_name) %>%
    mutate(tot = sum(trips)) %>%
    ungroup() %>%
    mutate(reltrips = trips / tot * 100)
#plt output here
#monitoring methods overview
plt <- ggplot(
    monmethall,
    aes(
        x = year,
        y = reltrips,
        stratum = monitoringMethod_name,
        alluvium = monitoringMethod_name,
        fill = monitoringMethod_name,
        label = monitoringMethod_name
    )
) +
    geom_flow(stat = "alluvium") +
    geom_stratum() +
    theme_bw() +
    labs(fill = "Monitoring method") +
    ylab("Proportion of annual trips by monitoring method (%)")
print(plt)
filename <- here("outputs", "monitoring_method_overview_trips.png")
png(filename, res = 300, width = 1600, height = 1200, unit = "px")
print(plt)
dev.off()

#clean
plt <- ggplot(
    monmethctry,
    aes(
        x = year,
        y = reltrips,
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
    ylab("Proportion of annual trips by monitoring method (%)")
print(plt)
filename <- here("outputs", "monitoring_method_country_trips.png")
png(filename, res = 300, width = 1600, height = 1200, unit = "px")
print(plt)
dev.off()

#moniroting by program
monprogctry <- D2 %>%
    group_by(monitoringProgramType_name, country_name, year) %>%
    summarise(trips = sum(tripsOb, na.rm = T), .groups = "drop") %>%
    group_by(year, country_name) %>%
    mutate(tot = sum(trips)) %>%
    ungroup() %>%
    mutate(reltrips = trips / tot * 100)
#a graph
plt <- ggplot(
    monprogctry,
    aes(
        x = year,
        y = reltrips,
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
    ylab("Proportion of annual trips by monitoring program (%)")

print(plt)
filename <- here("outputs", "monitoring_program_country_trips.png")
png(filename, res = 300, width = 1600, height = 1200, unit = "px")
print(plt)
dev.off()

#prepare data for sample coverage by monitoring effort by gear by georegions
monmethgear <- D2 %>%
    group_by(monitoringMethod_name, ecoregion, metierL3_name, year) %>%
    summarise(trips = sum(daysAtSeaOb, na.rm = T), .groups = "drop")

#monitoring methods: ecoregion/gear view
monmethgear1 <- D2 %>%
    group_by(monitoringMethod_name, ecoregion, metierL3_name, year) %>%
    summarise(trips = sum(tripsOb, na.rm = T), .groups = "drop") %>%
    filter(!monitoringMethod_name %in% c("Logbook", "Port observers"))
monmethgear2 <- monmethgear1 %>%
    mutate(metierL3_name = "All metier") %>%
    group_by(monitoringMethod_name, ecoregion, metierL3_name, year) %>%
    summarise(trips = sum(trips, na.rm = T), .groups = "drop")
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
            y = trips,
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
        ylab("Sum of annual trips by monitoring method")
    print(plt)
    filename <- here(
        "outputs",
        paste0("monitoring_method_", gsub(" ", "", i), "_trips.png")
    )
    png(filename, res = 300, width = 1600, height = 1200, unit = "px")
    print(plt)
    dev.off()
}

#prepare data for sample coverage by monitoring effort by georegions
monmethecoregion <- D2 %>%
    group_by(monitoringMethod_name, ecoregion, year) %>%
    summarise(trips = sum(tripsOb, na.rm = T), .groups = "drop") %>%
    filter(!is.na(ecoregion))

plt <- ggplot(
    monmethecoregion %>% mutate(ecoregion = sub(" ", "\n", ecoregion)),
    aes(
        x = year,
        y = trips,
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
    ggtitle(i) +
    ylab("Sum of annual trips by monitoring method")
print(plt)
filename <- here("outputs", "monitoring_method_ecoregion_trips.png")
png(filename, res = 300, width = 1600, height = 1200, unit = "px")
print(plt)
dev.off()
