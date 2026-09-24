#{{{
t33 <- D2 %>%
    group_by(year, monmethname) %>%
    summarise(das = sum(daysAtSeaOb, na.rm = T)) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(tot = sum(das)) %>%
    ungroup() %>%
    mutate(reldas = das / tot * 100) %>%
    mutate(monmethname = as.factor(monmethname))
pltt33 <- ggplot(
    t33,
    aes(
        x = year,
        y = reldas,
        stratum = monmethname,
        alluvium = monmethname,
        fill = monmethname,
        label = monmethname
    )
) +
    geom_flow(stat = "alluvium") +
    geom_stratum() +
    #geom_text(stat="stratum")+
    theme_bw() +
    ylab("Proportion of the annual days at sea by monitoring methods (%)") +
    labs(fill = "Monitoring method")
print(pltt33)
#}}}
