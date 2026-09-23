EM_taxa_bycatch <- D3 %>%
    filter(monitoringMethod == "EM")

# Drop na values and drop teleostei (Because only 3 incidents! Check this later if needed!)
EM_taxa_bycatch <- EM_taxa_bycatch %>%
    filter(!is.na(classname) & classname != "Teleostei") %>%
    filter(!classname == "Chondrostei")

## Pivot the info
EM_Taxa_Grouped <- EM_taxa_bycatch %>%
    group_by(year, country, classname) %>%
    summarize(
        sum_incidents = sum(incidentsWithPingers, na.rm = TRUE) +
            sum(incidentsWithoutPingers, na.rm = TRUE),
        sum_individuals = sum(individualsWithPingers, na.rm = TRUE) +
            sum(individualsWithoutPingers, na.rm = TRUE)
    )


#Plot incidents per taxa
plt <- ggplot(EM_Taxa_Grouped, aes(x = year, y = sum_incidents, fill = country)) +
    geom_bar(stat = "identity", position = "stack", colour = "black") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_x_continuous(breaks = unique(EM_Taxa_Grouped$year)) +
    labs(fill = "Country") +
    ylab("Total number of incidents") +
    xlab('') +
    facet_wrap(~classname, scales = "free", nrow = 1)

textcaption <- "Total number of bycatch incidents reported by each country through the WGBYC data calls, observed with electronic monitoring. Sweden did not report elasmobranch bycatch with EM in 2022-2025 due to difficulties in taxo-nomic identification. France did not report bycatch of seabirds and elasmobranchs with EM because data collection is not authorized for these taxa unless specified by the crew. Spain has ongoing EM research programs but did not submit EM data through the WGBYC data calls. "
print(textcaption)
filename <- here("outputs", "em_plt_incidents.png")
png(filename, res = 300, width = 30, height = 20, unit = "cm")
print(plt)
dev.off()
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
