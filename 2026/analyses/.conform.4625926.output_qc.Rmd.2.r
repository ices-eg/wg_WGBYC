#{{{
listidcountry <- D1 %>% transmute(id = ctryname) %>% distinct() %>% arrange(id)
rmd1 <- knitr::knit_expand("expand_ef1.rmd", id = listidcountry$id[1], idyear = 2023)
rmd <- rmd1
for (i in 2:nrow(listidcountry)) {
    #for(i in 2:2){
    rmdtmp <- knitr::knit_expand(
        "expand_ef1.Rmd",
        id = listidcountry$id[i],
        idyear = 2023
    )
    rmd <- paste0(rmd, rmdtmp, collapse = "\n")
}
#}}}
