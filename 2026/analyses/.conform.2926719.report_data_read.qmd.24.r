nbcountryD1 <- D1 %>%
    filter(year == 2025) %>%
    select(country_name) %>%
    distinct() %>%
    nrow()
nbcountryD2 <- D1 %>%
    filter(year == 2025) %>%
    select(country_name) %>%
    distinct() %>%
    nrow()
nbcountryD3 <- D1 %>%
    filter(year == 2025) %>%
    select(country_name) %>%
    distinct() %>%
    nrow()
#compute

deadline <- as.Date("2026-07-03")
startworkshop <- as.Date("2026-09-21")
tmp <- D4 %>%
    mutate(diffdeadline = deadline - as.Date(dateUpload)) %>%
    mutate(diffworkshop = startworkshop - as.Date(dateUpload)) %>%
    mutate(
        transmissionStatus = ifelse(
            diffdeadline >= 0,
            "Before the deadline",
            "After the deadline"
        )
    ) %>%
    mutate(
        transmissionStatus = ifelse(
            diffdeadline < 0 & diffworkshop >= 0,
            "After the deadline and before the WG",
            transmissionStatus
        )
    ) %>%
    mutate(
        transmissionStatus = ifelse(
            diffworkshop < 0,
            "After the deadline and during the WG",
            transmissionStatus
        )
    )

mean(tmp$diffdeadline)
median(tmp$diffdeadline)
table(tmp$transmissionStatus)
