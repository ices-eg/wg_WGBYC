## number bycatch birds / mammals / turtles =============

plotOut_bmt <- ggplot(
    data_toPlot_noFish,
    aes(x = year, y = bycatch_numInd, fill = className, label = className)
) +
    geom_col(stat = 'identity', position = 'dodge', width = 0.75) +
    scale_fill_manual(
        name = '',
        values = c('Aves' = '#F8766D', 'Mammalia' = '#00BA38', 'Reptilia' = '#619CFF')
    ) +
    facet_wrap(~country) +
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
filename <- here("outputs", "numBycatch_birdsMammalTurtle_country.png")
png(filename = filename, width = 75, height = 38, res = 300, units = "cm")
plotOut_bmt
dev.off()


# Elasmo ==================================================================
plotOut_elasmo <- ggplot(
    data_toPlot_elasmo,
    aes(x = year, y = bycatch_numInd, fill = className, label = className)
) +
    geom_col(stat = 'identity', position = 'dodge', width = 0.5) +
    scale_fill_manual(name = '', values = c('Elasmobranchii' = "#E76BF3")) +
    facet_wrap(~country) +
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
filename <- here("outputs", "numBycatch_elasmo_country.png")
png(filename = filename, width = 75, height = 38, res = 300, units = "cm")
plotOut_elasmo
dev.off()

# All pets ==================================================================

plotOut_all_a <- ggplot(
    data_toPlot_allPETS,
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

    facet_grid(className ~ country, scales = "free") +
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
filename <- here("outputs", "numBycatch_allPETSplots_country.png")
png(filename = filename, width = 75, height = 38, res = 300, units = "cm")
plotOut_all_a
dev.off()
