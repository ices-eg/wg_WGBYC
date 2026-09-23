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
textcaption <- "Total number of bycatch incidents for each taxa (birds, elasmobranchs, mammals, other fish species, and rep-tiles) reported by each monitoring method (2017-2025): at-sea-observers, electronic monitoring, port observers, and vessel crew observers, logbooks, other."
print(textcaption)
filename <- here("outputs", "incidents_by_monitoringProgram.png")
#keep 1.10 keep
print(plt)
png(
    filename,
    res = 300,
    width = 1600,
    height = 1200,
    unit = "px"
)
print(plt)
dev.off()
#save as docx object
read_docx() |>
    body_add_img(
        src = filename,
        height = 12,
        width = 15,
        unit = "cm",
        style = "centered"
    ) |>
    body_add_par(value = textcaption, style = "centered") |>
    print(target = sub(".png", ".docx", filename))
