#Plot incidents per taxa
plt <- ggplot(EM_Taxa_Grouped, aes(x = year, y = sum_incidents, fill = country_name)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(fill = "Country") +
    ylab("Total number of incidents") +
    facet_wrap(~classname, scales = "free") +
    scale_x_continuous(limits = c(2018.5, max(EM_Taxa_Grouped$year + 0.5))) #this is just to make all x-axes the same
plot(plt)
print(plt)
textcaption <- "test"
print(textcaption)
filename <- here("outputs", "EM_incidents_country.png")
png(filename, res = 300, width = 1600, height = 1200, unit = "px")
print(plt)
dev.off()
#save as docx object
read_docx() |>
    body_add_img(
        src = filename,
        width = 12,
        height = 10,
        unit = "cm",
        style = "centered"
    ) |>
    body_add_par(value = textcaption, style = "centered") |>
    print(target = sub(".png", ".docx", filename))
