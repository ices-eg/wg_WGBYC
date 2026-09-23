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
textcaption <- "Total monitored (observed) days at sea reported by each country for each monitoring program (2017-2025)"
print(textcaption)
filename <- here("outputs", "monitoring_program_country.png")
#keep 1.9
print(plt)
png(filename, res = 300, width = 21, height = 16, unit = "cm")
print(plt)
dev.off()
#save as docx object
read_docx() |>
    body_add_img(
        src = filename,
        height = 16,
        width = 21,
        unit = "cm",
        style = "centered"
    ) |>
    body_add_par(value = textcaption, style = "centered", ) |>
    print(target = sub(".png", ".docx", filename))
