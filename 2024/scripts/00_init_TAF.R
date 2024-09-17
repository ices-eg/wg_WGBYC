# initialize icesTAF structure and co
# the (usual) problem of "classified" information from fishery led to
# hack the transparent way this TAF should work
#
# directory skeleton
#
library(icesTAF)
taf.skeleton("../WGBYC2TAF")
setwd("../WGBYC2TAF")
#
# initiate the bib file with a templace
#
draft.data(
  data.files = "reportTemplate.docx",
  data.scripts = NULL,
  originator = "ICES",
  title = "ICES TAF word template for automation",
  year = 2024,
  source = "https://github.com/ices-taf/doc/raw/master/reportTemplate.docx",
  file = TRUE,
  append = FALSE 
)
#test
taf.boot()

#wgbyc data
#use the getbyc.R scripts




# document the wgbyc data
