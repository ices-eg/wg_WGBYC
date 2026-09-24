rendered <- knitr::knit(text = rmd, quiet = TRUE)
cat(rendered, sep = "\n")
