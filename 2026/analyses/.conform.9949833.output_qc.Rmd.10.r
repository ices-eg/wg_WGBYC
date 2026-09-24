#{{{
idyear <- 2023
plte1 <- ggplot(ef1, aes(x = year - 2000, y = das)) +
    geom_point() +
    facet_grid(fctshoreco(ecoregion) ~ sub(" ", "\n", L3name), scale = "free") +
    scale_y_log10() +
    geom_smooth(method = "lm") +
    theme_bw() +
    theme(strip.text.y.right = element_text(angle = 0, size = 8)) +
    theme(strip.text.x.top = element_text(size = 8)) +
    ylab("Days at sea (log 10 scale)") +
    xlab("year")
plte1bis1 <- ggplot(
    ef1bis %>%
        group_by(year, month = as.factor(month), ecoregion) %>%
        summarise(das = sum(das))
) +
    aes(x = month, y = das, fill = month) +
    geom_bar(stat = "identity") +
    scale_y_log10() +
    facet_grid(fctshoreco(ecoregion) ~ year, scale = "free") +
    theme_bw() +
    theme(strip.text.y.right = element_text(angle = 0, size = 8)) +
    ylab("Days at sea (log 10 scale)") +
    xlab("Month")

plte1bis2 <- ggplot(ef1bis) +
    aes(x = month, y = das, color = year, group = year) +
    geom_line() +
    #geom_point(data= ef1bis%>%filter(is.na(month))%>%mutate(month=1),
    #	   aes(x=month,y=das,color=year,group=year,shape=20))%>%
    facet_grid(fctshoreco(ecoregion) ~ sub(" ", "\n", L3name), scale = "free") +
    scale_y_log10() +
    theme_bw() +
    theme(strip.text.y.right = element_text(angle = 0, size = 8)) +
    theme(strip.text.x.top = element_text(size = 8)) +
    ylab("Days at sea (log 10 scale)") +
    xlab("year")


plte2 <- ggplot(ef2, aes(x = year - 2000, y = dasobs, color = monmethname)) +
    facet_grid(fctshoreco(ecoregion) ~ sub(" ", "\n", L3name), scale = "free") +
    scale_y_log10() +
    geom_smooth(method = "lm") +
    theme_bw() +
    theme(strip.text.y.right = element_text(angle = 0, size = 8)) +
    theme(strip.text.x.top = element_text(size = 8)) +
    theme(legend.position = "bottom") +
    ylab("Days at sea (log 10 scale)") +
    xlab("year")
plte12 <- ggplot(
    ef12 %>% filter(is.finite(das) & is.finite(dasobs)),
    aes(x = das, y = dasobs, color = (year))
) +
    geom_point() +
    facet_grid(fctshoreco(ecoregion) ~ sub(" ", "\n", L3name), scale = "free") +
    scale_y_log10() +
    scale_x_log10() +
    geom_smooth(method = "lm") +
    geom_point(
        data = ef12 %>% filter(is.finite(das) & is.finite(dasobs), year == idyear),
        color = "red",
        shape = "+",
        size = 2
    ) +
    theme_bw() +
    theme(strip.text.y.right = element_text(angle = 0, size = 8)) +
    theme(strip.text.x.top = element_text(size = 8)) +
    ylab("Observation effort (days at sea, lo10 scale)") +
    xlab("Fishing effort (days at sea, log10 scale)")
#}}}
