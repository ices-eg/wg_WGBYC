#  bookdown::word_document2: default
rmarkdown::render("output_qc.Rmd", clean = T) #,output_dir="./output",intermediates_dir="../../../tmp",knit_root_dir="../../../tmp")
unlink("output_qc_cache", recursive = T)
unlink("output_qc_files", recursive = T)
#officedown::rdocx_document: default
#bibliography: '/Users/moi/Zotero/exportdb/My Library.bib'
#csl: './ices.csl'
