# README #

Directory contains the data, code, and materials associated with:

> Kenny & Fortunato "The *palio* of Siena and the contemporary *contrada* system" ([doi:10.31235/osf.io/hxyvc](https://doi.org/10.31235/osf.io/hxyvc))

Preprint is hosted on
[SocArXiv](https://doi.org/10.31235/osf.io/hxyvc). Supplementary
information can be found alongside the preprint.

Different licences apply to different files, see
[LICENSE](./LICENSE.md).

## About the [data](./data) ##

Data were either collected or collated by A.R.K. Data have been
processed to remove identifying information. Information about the
data and redactions can be found in the
[codebook](./data/codebook.pdf).

Data files are in different folders in [data](./data):

* [attitudes](./data/attitudes): measurement of attitudes among *contrada*
  members

* [contrada descriptives](./data/contrada-descriptives): general
  information about the *contrada*

* [contrada size](./data/contrada-size): estimates of population size

* [geographical](./data/geographical): geographical information
  relating to Siena

* [giro study](./data/giro-study): information relating to the
  procession

* [GPS data](./data/gps): raw data from GPS devices distributed as
  part of procession

* [survey](./data/survey): survey of *contrada* members

## About the [code](./code) ##

All code written in `R`. Code was originally written across multiple
`.Rnw` files that have been
[purled](https://bookdown.org/yihui/rmarkdown-cookbook/purl.html).

`.R` files are in [scripts](./scripts), and contain code for the *contrada*
primer ([contrada information](./scripts/contrada-information.R)) and
the four research elements ([contrada
size](./scripts/contrada-size.R),
[survey](./scripts/survey-concatenated.R),
[attitudes](./scripts/attitudes.R), and [giro
study](./scripts/giro-study-concatenated.R)).

In some instances, code relating to a particular set of analyses
(e.g. survey) was written across multiple `.Rnw` files, so the
resultant `.R` script has been labelled "concatenated".

All code has been shared, except chunks that linked *contrada* names
to the anonymized naming system.

### How to run the code ###

To run the analysis, open [`analysis.R`](./analysis.R) and load the
packages, custom functions, and data. Files in [scripts](./scripts)
can then be opened and compiled.

## About the figures ##

All figures were generated in `R` and saved as pdfs. The figures
included in the manuscript have been deposited in [the figures
folder](./figures).