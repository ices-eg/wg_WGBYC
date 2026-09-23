#Use only Electronic Monitoring
EM_monitoringeffort <- D2 %>%
    filter(monitoringMethod == "EM")
## Pivot the info
EM_Grouped <- EM_monitoringeffort %>%
    group_by(year, country) %>%
    summarize(
        total_tripsOb = sum(tripsOb, na.rm = TRUE),
        total_daysAtSeaOb = sum(daysAtSeaOb, na.rm = TRUE)
    )

## 4.4 PLOT EM total trips observed =======
plt_A <- ggplot(EM_Grouped, aes(x = year, y = total_daysAtSeaOb, fill = country)) +
    geom_bar(stat = "identity", position = "stack", colour = "black") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_x_continuous(breaks = unique(EM_Grouped$year)) +
    labs(fill = "Country") +
    xlab('') +
    ylab("Total DAS observed using EM")
print(plt_A)
textcaption <- "Total monitored (observed) number of days at sea reported by each country with electronic monitoring."
print(textcaption)
filename <- here("outputs", "em_total_das.png")
png(filename, res = 300, width = 30, height = 20, unit = "cm")
print(plt_A)
dev.off()
read_docx() |>
    body_add_img(
        src = filename,
        width = 8,
        height = 10,
        unit = "cm",
        style = "centered"
    ) |>
    body_add_par(value = textcaption, style = "centered") |>
    print(target = sub(".png", ".docx", filename))

plt_B <- ggplot(EM_Grouped, aes(x = year, y = total_tripsOb, fill = country)) +
    geom_bar(stat = "identity", position = "stack", colour = "black") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_x_continuous(breaks = unique(EM_Grouped$year)) +
    labs(fill = "Country") +
    xlab('') +
    ylab("Total trips observed using EM")
print(plt_B)
textcaption <- "Total monitored (observed) number of trip reported by each country with electronic monitoring."
print(textcaption)
filename <- here("outputs", "em_total_das_trip.png")
png(filename, res = 300, width = 30, height = 20, unit = "cm")
print(plt_B)
dev.off()
read_docx() |>
    body_add_img(
        src = filename,
        width = 8,
        height = 10,
        unit = "cm",
        style = "centered"
    ) |>
    body_add_par(value = textcaption, style = "centered") |>
    print(target = sub(".png", ".docx", filename))

AB <- plot_grid(
    A + theme(legend.position = "none"),
    B + theme(legend.position = "right"),
    rel_widths = c(1, 1.2),
    nrow = 1
)
textcaption <- "Total monitored (observed) number of trip reported by each country with electronic monitoring."
print(textcaption)
filename <- here("outputs", "em_total_das_and_trip.png")
png(filename, res = 300, width = 30, height = 20, unit = "cm")
print(AB)
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
