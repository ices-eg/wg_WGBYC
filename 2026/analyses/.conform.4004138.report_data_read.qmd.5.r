## color palette options to avoid a highly controversial continuous log scale
c("#FFCCCC", "#FF9999", "#FF6666", "#FF3333", "#FF0000", "#CC0000") # old and ugly

c(
    "#440154FF",
    "#46337EFF",
    "#365C8DFF",
    "#277F8EFF",
    "#1FA187FF",
    "#4AC16DFF",
    "#9FDA3AFF",
    "#FDE725FF"
) # 8 levels

c(
    "#440154FF",
    "#443A83FF",
    "#31688EFF",
    "#21908CFF",
    "#35B779FF",
    "#8FD744FF",
    "#FDE725FF"
) # 7 levels

c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF") # 6 levels

## define color palette for plot with shades of red
ownPalette <- list(
    fishingEffort = c(
        "< 100" = "#440154FF",
        "100-1000" = "#443A83FF",
        "1000-2000" = "#31688EFF",
        "2000-5000" = "#21908CFF",
        "5000-10000" = "#35B779FF",
        "10000-25000" = "#8FD744FF",
        "> 25000" = "#FDE725FF"
    ),
    observer = c(
        "<5" = "#440154FF",
        "5-10" = "#443A83FF",
        "10-20" = "#31688EFF",
        "20-50" = "#21908CFF",
        "50-100" = "#35B779FF",
        "100-500" = "#8FD744FF",
        ">500" = "#FDE725FF"
    ),
    observerPerc = c(
        "< 0.25" = "#440154FF",
        "0.25-0.5" = "#443A83FF",
        "0.5-0.75" = "#31688EFF",
        "0.75-1.0" = "#21908CFF",
        "1.0-2.0" = "#35B779FF",
        "2.0-4.0" = "#8FD744FF",
        "> 4.0" = "#FDE725FF"
    )
)


# Function to create plots =================
create_plot <- function(
    data = XXXX,
    fill_var = "xxxx",
    plotTitle = 'xxxx',
    paletteBreaksColours = ownPalette$xxxx,
    paletteTitle = 'xxxx',
    facet_var = "xxxx",
    facet_ncol = 3,
    facet_nrow = 3
) {
    total_pages <- ceiling(length(unique(data[[facet_var]])) / (facet_ncol * facet_nrow))
    for (page in 1:total_pages) {
        plot <- ggplot(data = data) +
            geom_sf(data = ICESareas_laea, fill = "white", color = "lightgray") +
            geom_sf(data = world, fill = "lightgray", color = "darkgrey", size = 0.1) +
            geom_sf(
                aes_string(geometry = "geometry", fill = fill_var),
                color = "darkgrey"
            ) +
            # scale_fill_viridis(name = paletteBreaksTitle, direction = 1, option = "D",
            #                     trans = "pseudo_log", breaks = paletteBreaks,
            #                    labels = c(paletteBreakLabels),
            #                    na.value = "white",
            #                     discrete = FALSE,
            #                    guide = guide_colourbar(title.position = "top", barwidth = 18, barheight = 1)) +
            scale_fill_manual(
                name = paletteTitle,
                values = paletteBreaksColours,
                na.value = "white",
                guide = guide_legend(
                    title.position = "top",
                    label.position = "bottom",
                    nrow = 1,
                    byrow = TRUE
                )
            ) +
            coord_sf(
                crs = st_crs(3035),
                xlim = c(-40000, 6800000),
                ylim = c(1100000, 7000000)
            ) +
            theme_minimal() +
            labs(title = plotTitle) +
            facet_wrap_paginate(
                as.formula(paste("~", facet_var)),
                ncol = facet_ncol,
                nrow = facet_nrow,
                page = page,
                drop = TRUE
            ) +
            theme(
                strip.text = element_text(size = 6),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                axis.title = element_blank(),
                legend.position = "bottom",
                legend.text = element_text(
                    size = 8,
                    angle = -67,
                    vjust = 0.33,
                    hjust = 0
                ),
                legend.title = element_text(size = 10),
                panel.background = element_blank(),
                plot.background = element_blank()
            )
        return(plot)
    }
}


## 4.3 Plots for Metier Level 3 ===================

## days at sea
plt <- create_plot(
    data = daysAtSea_spatial,
    fill_var = "daysAtSeaF_cat",
    plotTitle = 'Fishing effort',
    paletteBreaksColours = ownPalette$fishingEffort,
    paletteTitle = 'Fishing effort (days at sea)',
    facet_var = "metierL3",
    facet_ncol = 3,
    facet_nrow = 3
)
print(plt)
textcaption <- "Metier level 3 fishing effort for 2025 (days at sea) submitted to the WGBYC database for monitoring methods retained for use under ToR C for the calculation of bycatch estimates for 2024. This includes all scientific observer data, all electronic mon-itoring data, and all vessel crew data (except for Estonia) but excludes reports from logbooks and port observers. Transparent = no data or zero."
print(textcaption)
filename <- here("outputs", "fishingEffort_L3.png")
png(filename, res = 300, width = 30, height = 20, unit = "cm")
print(plt)
dev.off()
read_docx() |>
    body_add_img(
        src = filename,
        width = 18,
        height = 14,
        unit = "cm",
        style = "centered"
    ) |>
    body_add_par(value = textcaption, style = "centered") |>
    print(target = sub(".png", ".docx", filename))


## monitoring effort days at sea
plt <- create_plot(
    data = daysAtSea_Ob,
    fill_var = "daysAtSea_Ob_cat",
    plotTitle = 'Monitoring effort',
    paletteBreaksColours = ownPalette$observer,
    paletteTitle = 'Monitoring effort (days at sea)',
    facet_var = "metierL3",
    facet_ncol = 3,
    facet_nrow = 3
)
print(plt)
textcaption <- "Metier level 3 monitoring effort for 2025 (days at sea) submitted to the WGBYC database for monitoring methods retained for use under ToR C for the calculation of bycatch estimates for 2024. This includes all scientific observer data, all elec-tronic monitoring data, and all vessel crew data (except for Estonia) but excludes reports from logbooks and port observers. Transparent = no data or zero."
print(textcaption)
filename <- here("outputs", "monitoringEffort_L3.png")
png(filename, res = 300, width = 30, height = 20, unit = "cm")
print(plt)
dev.off()
read_docx() |>
    body_add_img(
        src = filename,
        width = 18,
        height = 14,
        unit = "cm",
        style = "centered"
    ) |>
    body_add_par(value = textcaption, style = "centered") |>
    print(target = sub(".png", ".docx", filename))


## monitoring coverage (%)
plt <- create_plot(
    data = spatialCoverage,
    fill_var = "Obs_Coverage_cat",
    plotTitle = 'Monitoring coverage',
    paletteBreaksColours = ownPalette$observerPerc,
    paletteTitle = 'Monitoring coverage (%)',
    facet_var = "metierL3",
    facet_ncol = 3,
    facet_nrow = 3
)
print(plt)
textcaption <- "Metier level 3 monitoring coverage of 2025 data submitted to the WGBYC database for monitoring methods retained for use under ToR C for the calculation of bycatch estimates for 2024. This includes all scientific observer data, all electronic mon-itoring data, and all vessel crew data (except for Estonia) but excludes reports from logbooks and port observers. Transparent = no data or zero. Coverage calculated as monitoring effort as a percentage of fishing effort (using numbers presented in Figures X3 & X4 above). Transparent = no data or zero."
print(textcaption)
filename <- here("outputs", "monitoringCoverage_L3.png")
png(filename, res = 300, width = 30, height = 20, unit = "cm")
print(plt)
dev.off()
read_docx() |>
    body_add_img(
        src = filename,
        width = 18,
        height = 14,
        unit = "cm",
        style = "centered"
    ) |>
    body_add_par(value = textcaption, style = "centered") |>
    print(target = sub(".png", ".docx", filename))
