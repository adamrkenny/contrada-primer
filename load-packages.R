## check for missing packages and install, from:
## https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
## https://gist.github.com/stevenworthington/3178163

## list of packages required
packages <- c(
    "knitr", # dynamic documents
    "tidyverse", # data manipulation
    "readr", # data wrangling       
    "stringr", # string manipulation
    "scales", #  formatting numbers
    "kableExtra", # making tables
    "broom", # tidying objects into tibbles
    "broman", # to round numbers preserving zeros
    "lmerTest", # linear mixed-effects modelling
    "emmeans", # linear mixed-effects modelling
    "readODS", # reading ods
    "rgdal", # reading spatial data
    "patchwork", # combining plots
    "ggpubr", # combining plots # FIXME possibly remove
    "ggspatial", # manipulating spatial data with ggplot2
    "ggrepel", # repel labels in plots
    "ggalluvial", # create sankey diagrams
    "osmdata", # retrieve osm data
    "XML", #  wrangle XML data
    "lubridate", # for cleaning time data
    "geosphere", # for calculating distance in spatial data
    "english", # for custom function to print numbers    
    "tidygeocoder", # for location of geographical locations    
    "ggtext", # for markdown elements in ggplot
    "sf", # for mapping
    "fpc", # for clusters
    "dbscan", # for clusters
    "factoextra", # for clusters
    "EnvStats", # for plotting summaries FIXME to remove
    "sp", # reading spatial data
    "rgeos" # reading spatial data
    )

## create list of packages that have not been installed
new_packages <-
    packages[!(packages %in% installed.packages()[,"Package"])]
## install packages that have not been installed
if (length(new_packages)) 
    install.packages(new_packages, dependencies = TRUE)
## load all packages
sapply(packages, library, character.only = TRUE)

## ## create bib file with all packages
## ## https://tex.stackexchange.com/a/493435
## knitr::write_bib(packages,
##                  prefix = "R_pkg_",
##                  file = "references-R-packages.bib",
##                  tweak = FALSE) # TRUE removes inverted commas 
## Rcite = citation(); Rcite$key = "R_core-team" # Add citation for R
## write(toBibtex(Rcite),
##       file = "references-R-packages.bib",
##       append = TRUE)

## ## replace printing of "and and other contributors" in readODS pkg
## ## which generates warning
## ref_file <-
##     readLines("references-R-packages.bib")
## ref_file_updated <-
##     gsub(pattern = "and and other contributors",
##          replace = "and {other contributors}",
##          x = ref_file)
## writeLines(ref_file_updated, con = "references-R-packages.bib")
