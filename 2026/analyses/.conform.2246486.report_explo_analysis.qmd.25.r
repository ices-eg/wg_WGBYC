D1 <- D1 %>%
    mutate(country_name = ifelse(country_name == "Azores", "Portugal", country_name)) %>%
    mutate(
        country_name = ifelse(
            country_name == "Northern Ireland",
            "United Kingdom",
            country_name
        )
    )
D2 <- D2 %>%
    mutate(country_name = ifelse(country_name == "Azores", "Portugal", country_name)) %>%
    mutate(
        country_name = ifelse(
            country_name == "Northern Ireland",
            "United Kingdom",
            country_name
        )
    )
D3 <- D3 %>%
    mutate(country_name = ifelse(country_name == "Azores", "Portugal", country_name)) %>%
    mutate(
        country_name = ifelse(
            country_name == "Northern Ireland",
            "United Kingdom",
            country_name
        )
    )
