#| cache: true
#| echo: false
#|
#describe dir contains in the readme.md
if (F) {
    library(here)
    here::i_am("project.Rproj")
    source(here("analyses", "printdir.R"))
    printdir(here("data"), here("data", "README.md"))
    printdir(here("outputs"), here("outputs", "README.md"))
    quarto::quarto_render("report_data_read.qmd")
}
