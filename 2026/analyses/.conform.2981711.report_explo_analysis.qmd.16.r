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
textcaption <- "test"
print(textcaption)
png(filename, res = 300, width = 30, height = 20, unit = "cm")
print(plt)
dev.off()
#save as docx object
read_docx() |>
    body_add_img(
        src = filename,
        width = 12,
        height = 10,
        unit = "cm",
        style = "centered"
    ) |>
    body_add_par(value = textcaption, style = "centered") |>
    print(target = sub(".png", ".docx", filename))
