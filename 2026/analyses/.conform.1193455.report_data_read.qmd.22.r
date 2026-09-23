#by country
plt_monitoring_method_country <- ggplot(
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
print(plt_monitoring_method_country)
textcaption <- "test"
print(textcaption)
filename <- here(
    "outputs",
    glue(list_table$plt_monitoring_method_country[1], "_monitoring_method_country.png")
)
png(filename, res = 300, width = 22, height = 16, unit = "cm")
print(plt_monitoring_method_country)
dev.off()
#save as docx object
read_docx() |>
    body_add_img(
        src = filename,
        height = 16,
        width = 18,
        unit = "cm",
        style = "centered"
    ) |>
    body_add_par(value = textcaption, style = "centered") |>
    print(target = sub(".png", ".docx", filename))
