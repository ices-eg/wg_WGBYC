ef1 <- D1 %>%
    group_by(year, L3name, ecoregion) %>%
    summarise(das = sum(daysAtSeaF, na.rm = T)) %>%
    ungroup()
ef2 <- D2 %>%
    group_by(year, L3name, ecoregion, monmethname) %>%
    summarise(das = sum(daysAtSeaOb, na.rm = T)) %>%
    ungroup()
b3 <- D3 %>%
    mutate(
        n1 = ifelse(is.na(incidentsWithPingers), 0, 1),
        n2 = ifelse(is.na(incidentsWithPingers), 0, 1)
    ) %>%
    group_by(species, year, L3name, ecoregion) %>%
    summarise(nb = n1 + n2) %>%
    ungroup()

b3 %>% tidyr::pivot_wider(values_from = nb, names_from = species)

marmamspp <- D3 %>% filter(cetaceanFlag == "Y") %>% select(species) %>% distinct()

b123 <- full_join(full_join(ef1, ef2), b3)
