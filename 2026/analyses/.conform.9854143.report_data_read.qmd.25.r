#| column: page
## green table output for WGBYC report

#green table
greentableD1 <- D1 %>%
    group_by(country_name, year) %>%
    summarise(n = n()) %>%
    mutate(year = paste0("D1_", year)) %>%
    tidyr::pivot_wider(values_from = n, names_from = year, names_sort = T)
greentableD2 <- D2 %>%
    group_by(country_name, year) %>%
    summarise(n = n()) %>%
    mutate(year = paste0("D2_", year)) %>%
    tidyr::pivot_wider(values_from = n, names_from = year, names_sort = T)
greentableD3 <- D3 %>%
    group_by(country_name, year) %>%
    summarise(n = n()) %>%
    mutate(year = paste0("D3_", year)) %>%
    tidyr::pivot_wider(values_from = n, names_from = year, names_sort = TRUE)
greentable <- full_join(full_join(greentableD1, greentableD2), greentableD3) %>%
    ungroup() %>%
    data.frame()

#build up a submission table
subtableD1 <- D1 %>%
    group_by(country_name, year) %>%
    summarise(n = paste0(sort(unique(submissionYear)), collapse = ",")) %>%
    mutate(year = paste0("D1_", year)) %>%
    tidyr::pivot_wider(values_from = n, names_from = year, names_sort = T)
subtableD2 <- D2 %>%
    group_by(country_name, year) %>%
    summarise(n = paste0(sort(unique(submissionYear)), collapse = ",")) %>%
    mutate(year = paste0("D2_", year)) %>%
    tidyr::pivot_wider(values_from = n, names_from = year, names_sort = T)
subtableD3 <- D3 %>%
    group_by(country_name, year) %>%
    summarise(n = paste0(sort(unique(submissionYear)), collapse = ",")) %>%
    mutate(year = paste0("D3_", year)) %>%
    tidyr::pivot_wider(values_from = n, names_from = year, names_sort = T)
subtable <- full_join(full_join(subtableD1, subtableD2), subtableD3) %>%
    ungroup() %>%
    data.frame()
fulltable <- subtable
#add the number of record in subtable with a good old double loop du seigneur
for (i in 1:nrow(subtable)) {
    for (j in 2:ncol(subtable)) {
        fulltable[i, j] <- paste(
            subtable[i, j],
            paste0("(", greentable[i, j], ")"),
            sep = "\n"
        )
    }
}
#cosmetic
nametab <- names(greentable)[-1]
nametab <- paste0(nametab, "='", substr(nametab, 4, 7), "'", collapse = ",")
nametab <- eval(parse(text = paste0("list(", nametab, ")")))
nametab <- c(list(country_name = "Year of data"), nametab)
bgcolormat <- ifelse(greentable[, -1] < 0.00001, "white", "darkgreen")
charcolormat <- matrix("white", nrow = nrow(subtable), ncol = ncol(subtable) - 1)
charcolormat[(currentyear + 1) == subtable[, -1]] <- "black"
#charcolormat["2021,2023"==subtable[,-1]]<-"yellow"
big_border <- fp_border(color = "black", width = 2)
#remove number
#greentable[is.na(greentable)]<-0

#param to fix column formatting
nbcolyear <- (ncol(subtable) - 1) / 3
posbigline <- c(1, nbcolyear + 1, 1 + nbcolyear * 2)

textcaption <- "Summary table of countries providing data submissions to ICES WGBYC with data on fishing effort, observer effort (either days at sea or other measurement, e.g. effort per haul or set), and bycatch records. Green = Data submission received, White = no data received, Black = data submitted this year. The year of submission is also provided with the number of record (in bracket) for each table and country."

ftgreentable <- flextable(fulltable) %>%
    bg(j = 2:ncol(greentable), bg = bgcolormat) %>%
    color(j = 2:ncol(greentable), color = charcolormat) %>%
    set_header_labels(values = nametab) %>%
    add_header_row(
        values = c(
            "",
            "Fishing Effort (D1 table)",
            "Monitoring Effort (D2 table)",
            "Bycatch Events (D3 table)"
        ),
        colwidths = c(1, nbcolyear, nbcolyear, nbcolyear)
    ) %>%
    theme_box() %>%
    align(align = "center", part = "header") %>%
    align(align = "center", part = "body") %>%
    vline(j = posbigline, border = big_border) %>%
    fontsize(part = "all", size = 5) %>%
    autofit(add_w = 0, add_h = 0) %>%
    set_caption(caption = textcaption)
ftgreentable


save_as_docx(
    ftgreentable,
    path = here("outputs", "greentable.docx"),
    pr_section = prop_section(
        page_size = page_size(orient = "landscape"),
        page_margins = page_mar(
            bottom = 0.1,
            top = .1,
            right = .1,
            left = .1,
            header = 0,
            footer = 0,
            gutter = 0
        )
    )
)
