# EURLex
An R shiny application to search and reference legislation from the EURLex database

## Prerequesites
The app uses the following packages:
* shiny
* DT
* readr
* pdftools
* rvest
* dplyr
* lubridate
* janitor
* stringr

It also requires a CSV metadata file from EURLex to run. This can be obtained by exporting the relevant metadata from [the legislation index](https://eur-lex.europa.eu/browse/directories/legislation.html).

Note that a login is required to return all entries, a csv obtained without a login will be limited in returns.

## Usage notes

* The current iteration of the code has a proxy setting to access the web, necessary for the original location of the app. This can be deleted as required.
* The metadata files can be unpredictable with read.csv and read_csv due to encoding isues. It may be necessary to swap one for the other. A manual change for colnames is included to avoid needing to rename variables.
