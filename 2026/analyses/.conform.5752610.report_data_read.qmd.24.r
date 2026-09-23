#add human readable label with some basic test in case ICES vocab being not consistent
#D1
tmp <- D1 %>%
    left_join(
        ISO_3166 %>% transmute(country = Key, country_name = Description),
        by = c("country")
    ) %>%
    left_join(
        GearGroup %>% transmute(metierL3 = Key, metierL3_name = Description),
        by = c("metierL3")
    )
if (nrow(tmp) != nrow(D1)) {
    stop("pb D1 explo1")
} else {
    D1 <- tmp
    rm(tmp)
}
#D2
tmp <- D2 %>%
    left_join(
        ISO_3166 %>% transmute(country = Key, country_name = Description),
        by = c("country")
    ) %>%
    left_join(
        GearGroup %>% transmute(metierL3 = Key, metierL3_name = Description),
        by = c("metierL3")
    ) %>%
    left_join(
        BYC_MonMethod %>%
            transmute(monitoringMethod = Key, monitoringMethod_name = Description),
        by = c("monitoringMethod")
    ) %>%
    left_join(
        BYC_MonProgr %>%
            transmute(
                monitoringProgramType = Key,
                monitoringProgramType_name = Description
            ),
        by = c("monitoringProgramType")
    )
if (nrow(tmp) != nrow(D2)) {
    stop("pb D2 explo1")
} else {
    D2 <- tmp
    rm(tmp)
}
#D3
tmp <- D3 %>%
    left_join(
        ISO_3166 %>% transmute(country = Key, country_name = Description),
        by = c("country")
    ) %>%
    left_join(
        GearGroup %>% transmute(metierL3 = Key, metierL3_name = Description),
        by = c("metierL3")
    ) %>%
    left_join(
        BYC_MonMethod %>%
            transmute(monitoringMethod = Key, monitoringMethod_name = Description),
        by = c("monitoringMethod")
    ) %>%
    left_join(
        BYC_MonProgr %>%
            transmute(
                monitoringProgramType = Key,
                monitoringProgramType_name = Description
            ),
        by = c("monitoringProgramType")
    )
if (nrow(tmp) != nrow(D3)) {
    stop("pb D3 explo1")
} else {
    D3 <- tmp
    rm(tmp)
}
