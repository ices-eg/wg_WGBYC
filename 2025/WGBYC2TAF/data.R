## Preprocess data, write TAF data tables

##recipes
#library(icesTAF)
#library(icesConnect)
#set_username("myusername")
#taf.boot()
#source.all()


## Before: the data
## After: load the data 

#some packages
library(icesTAF)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)
library(flextable)
library(ftExtra)
library(officer)
require(openxlsx)

## data read 
source("data1_dataread.R")
