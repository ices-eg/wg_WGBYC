#{{{
ef1 <- D1 %>%
    group_by(ctryname, year, L3name, ecoregion) %>%
    summarise(das = sum(daysAtSeaF, na.rm = T)) %>%
    ungroup()
ef2 <- D2 %>%
    group_by(ctryname, year, L3name, ecoregion, monmethname) %>%
    summarise(das = sum(daysAtSeaOb, na.rm = T)) %>%
    ungroup()
ef2bis <- D2 %>%
    group_by(ctryname, year, L3name, ecoregion) %>%
    summarise(dasobs = sum(daysAtSeaOb, na.rm = T)) %>%
    ungroup()
ef12 <- full_join(ef1, ef2bis)
#}}}
