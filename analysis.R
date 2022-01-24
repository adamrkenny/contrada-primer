## files to load
list_files <-

    c("./load-packages.R",
      "./custom-functions.R",
      "./load-datafiles.R")

## load packages, functions, and data
sapply(list_files, source)

##################################################
## each file can be opened separately and run
## once packages, functions, and data have been loaded

## contrada information
source("./scripts/contrada-information.R")

## contrada size
source("./scripts/contrada-size.R")

## survey
source("./scripts/survey-concatenated.R")

## attitudes
source("./scripts/attitudes.R")

## giro study
source("./scripts/giro-study-concatenated.R")
## warning: slow because of cluster analysis

