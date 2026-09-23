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
textcaption <- ""
print(textcaption)
filename <- here("outputs", "monitoring_method_overview_trips.png")
png(filename, res = 300, width = 1600, height = 1200, unit = "px")
print(plt)
dev.off()
#save as docx object
read_docx() |>
    body_add_img(
        src = filename,
        width = 16,
        height = 12,
        unit = "cm",
        style = "centered"
    ) |>
    body_add_par(value = textcaption, style = "centered") |>
    print(target = sub(".png", ".docx", filename))
