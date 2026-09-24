## Number bycatch birds / mammals / turtles

plt <- ggplot(
    data_toPlot_noFish,
    aes(x = year, y = bycatch_numInd, fill = className, label = className)
) +
    geom_col(stat = 'identity', position = 'dodge', width = 0.75) +
    scale_fill_manual(
        name = '',
        values = c('Aves' = '#F8766D', 'Mammalia' = '#00BA38', 'Reptilia' = '#619CFF')
    ) +

    facet_wrap(~ecoregion, ncol = 3, nrow = 6) +

    labs(fill = " ") +
    ylab("Number reported bycatch") +
    xlab(" ") +

    scale_x_continuous(breaks = years_unique) +

    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank()
    ) +
    guides(fill = guide_legend(nrow = 1))
print(plt)
textcaption <- ""
print(textcaption)
filename <- here("outputs", "numBycatch_birdsMammalTurtle.png")
png(filename, res = 300, width = 14, height = 18, unit = "cm")
print(plt)
dev.off()
read_docx() |>
    body_add_img(
        src = filename,
        width = 14,
        height = 18,
        unit = "cm",
        style = "centered"
    ) |>
    body_add_par(value = textcaption, style = "centered") |>
    print(target = sub(".png", ".docx", filename))


# Elasmo ==================================================================

plt <- ggplot(
    data_toPlot_elasmo,
    aes(x = year, y = bycatch_numInd, fill = className, label = className)
) +
    geom_col(stat = 'identity', position = 'dodge', width = 0.5) +
    scale_fill_manual(name = '', values = c('Elasmobranchii' = "#E76BF3")) +

    facet_wrap(~ecoregion, ncol = 3, nrow = 6) +

    labs(fill = " ") +
    ylab("Number reported bycatch") +
    xlab(" ") +

    scale_x_continuous(breaks = years_unique) +
    # scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0, 100, 1000, 10000, 60000)) +

    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank()
    ) +
    guides(fill = guide_legend(nrow = 1))
print(plt)
textcaption <- ""
print(textcaption)
filename <- here("outputs", "numBycatch_elasmo.png")
png(filename, res = 300, width = 14, height = 18, unit = "cm")
print(plt)
dev.off()
read_docx() |>
    body_add_img(
        src = filename,
        width = 14,
        height = 18,
        unit = "cm",
        style = "centered"
    ) |>
    body_add_par(value = textcaption, style = "centered") |>
    print(target = sub(".png", ".docx", filename))


# All pets ==================================================================

# fix y-axis

range(data_toPlot_allPETS$bycatch_numInd[which(data_toPlot_allPETS$className == 'Aves')])
range(data_toPlot_allPETS$bycatch_numInd[which(
    data_toPlot_allPETS$className == 'Mammalia'
)])
range(data_toPlot_allPETS$bycatch_numInd[which(
    data_toPlot_allPETS$className == 'Reptilia'
)])
range(data_toPlot_allPETS$bycatch_numInd[which(
    data_toPlot_allPETS$className == 'Elasmobranchii'
)])

dummyData_yaxis_a <- data.frame(
    ecoregion = rep(
        c('Adriatic Sea', 'Aegean-Levantine Sea', 'Azores', 'Baltic Sea', 'Barents Sea'),
        4
    ),
    year = rep(2017, (5 * 4)),
    className = c(
        rep('Aves', 5),
        rep('Mammalia', 5),
        rep('Reptilia', 5),
        rep('Elasmobranchii', 5)
    ),
    bycatch_numInd = c(rep(1050, 5), rep(1000, 5), rep(170, 5), rep(10000, 5))
) #set y-axis max for each species

dummyData_yaxis_b <- data.frame(
    ecoregion = rep(
        c(
            'Bay Biscay & Iberian Coast',
            'Black Sea',
            'Celtic Seas',
            'Faroes',
            'Greater North Sea',
            'Greenland Sea'
        ),
        4
    ),
    year = rep(2017, (6 * 4)),
    className = c(
        rep('Aves', 6),
        rep('Mammalia', 6),
        rep('Reptilia', 6),
        rep('Elasmobranchii', 6)
    ),
    bycatch_numInd = c(rep(1050, 6), rep(1000, 6), rep(170, 6), rep(10000, 6))
) #set y-axis max for each species

dummyData_yaxis_c <- data.frame(
    ecoregion = rep(
        c(
            'Icelandic Waters',
            'Ionian & Central Mediterranean',
            'North West Atlantic',
            'Norwegian Sea',
            'Oceanic NE Atlantic',
            'Western Mediterranean'
        ),
        4
    ),
    year = rep(2017, (6 * 4)),
    className = c(
        rep('Aves', 6),
        rep('Mammalia', 6),
        rep('Reptilia', 6),
        rep('Elasmobranchii', 6)
    ),
    bycatch_numInd = c(rep(1050, 6), rep(1000, 6), rep(170, 6), rep(10000, 6))
) #set y-axis max for each species

dummyData_yaxis_list = list(
    "dummyData_yaxis_a" = dummyData_yaxis_a,
    "dummyData_yaxis_b" = dummyData_yaxis_b,
    "dummyData_yaxis_c" = dummyData_yaxis_c
)


# Make plot =================
# Loop to produce the figure a/b/c

lapply(1:3, function(x) {
    plt <- ggplot(
        data_toPlot_allPETS_list[[x]],
        aes(x = year, y = bycatch_numInd, fill = className, label = className)
    ) +
        geom_col(stat = 'identity', position = 'dodge', width = 0.75) +
        scale_fill_manual(
            name = '',
            values = c(
                'Aves' = '#F8766D',
                'Mammalia' = '#00BA38',
                'Reptilia' = '#619CFF',
                'Elasmobranchii' = '#E76BF3'
            ),
            labels = c(
                'Aves' = 'Aves',
                'Mammalia' = 'Mammalia',
                'Reptilia' = 'Reptilia',
                'Elasmobranchii' = 'Elasmobranchii'
            )
        ) +

        facet_grid(className ~ ecoregion, scales = "free") +
        geom_point(data = dummyData_yaxis_list[[x]], col = 'white', alpha = 0) +

        labs(fill = " ") +
        ylab(" ") +
        xlab(" ") +

        scale_x_continuous(breaks = years_unique) +

        theme_bw() +
        theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "right",
            panel.grid.minor.x = element_blank(),
            panel.background = element_blank()
        ) +
        guides(fill = guide_legend(nrow = 4))
}) -> data_toPlot_allPETS_plot

textcaption <- "Bycatch reporting (individual numbers) by ecoregion from 2017 to 2024, across all metiers. Note that in 2017 and 2018 numbers are capped at 14,000 from 66,524 and 19,206 respectively to improve visualization"
print(textcaption)
filename <- here(
    "outputs",
    glue(list_table$data_toPlot_allPETS_plot[1], "_numBycatch_allPETSplots.png")
)
png(filename, res = 300, width = 35, height = 37, unit = "cm")
plotLayout = rbind(c(1), c(2), c(3))
grid.arrange(
    data_toPlot_allPETS_plot[[1]],
    data_toPlot_allPETS_plot[[2]],
    data_toPlot_allPETS_plot[[3]],
    ncol = 1,
    layout_matrix = plotLayout
)
dev.off()
read_docx() |>
    body_add_img(
        src = filename,
        width = 15,
        height = 15,
        unit = "cm",
        style = "centered"
    ) |>
    body_add_par(value = textcaption, style = "centered") |>
    print(target = sub(".png", ".docx", filename))
