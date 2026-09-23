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
textcaption <- "Total monitored (observed) days at sea reported by each country for each monitoring method (2017-2025); at-sea-observers, electronic monitoring, port observers, vessel crew observers, logbooks, other."
print(textcaption)
filename <- here("outputs", "monitoring_method_country_trips.png")
png(filename, res = 300, width = 30, height = 20, unit = "cm")
print(plt)
dev.off()
#save as docx object
read_docx() |>
    body_add_img(
        src = filename,
        width = 10,
        height = 8,
        unit = "cm",
        style = "centered"
    ) |>
    body_add_par(value = textcaption, style = "centered") |>
    print(target = sub(".png", ".docx", filename))
