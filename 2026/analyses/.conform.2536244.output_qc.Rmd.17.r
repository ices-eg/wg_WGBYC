#knitr option
operationnel <- TRUE
knitr::opts_chunk$set(
    echo = FALSE,
    warning = !operationnel,
    message = !operationnel,
    fig.height = 6,
    fig.width = 6,
    progress = !operationnel,
    verbose = !operationnel,
    include = TRUE,
    cache = T,
    fig.align = "center"
)
#include=TRUE,dev='png',autodep=FALSE)
library(dplyr)
library(icesVocab)
library(httr)
library(icesConnect) #ask a token using  ices_token
library(jsonlite)
library(flextable)
library(ftExtra)
library(officer)
library(sf)
library(ggplot2)
sf_use_s2(FALSE)
library(captioner)
library(ggalluvial)
#initialise caption fct for tab and fig
tabcap <- captioner(prefix = "Tableau ", infix = ".", levels = 1, auto_space = FALSE)
figcap <- captioner(prefix = "Figure ", infix = ".", levels = 1, auto_space = FALSE)
figcapannex <- captioner(prefix = "Figure A", infix = ".", levels = 1, auto_space = FALSE)
