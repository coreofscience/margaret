# margaret: Extracts data from Minciencias web pages

# Overview
This package extracts data from Minciencias web pages about research groups and researchers, merge information with quality of articles from [Scimago](https://www.scimagojr.com/), [Publindex](https://scienti.minciencias.gov.co/publindex/#/revistasPublindex/clasificacion) and [Google scholar](https://scholar.google.es/).
And export the data in a xlsx file.

# Installation

## From CRAN

```r
install.packages("margaret")
```

## From GitHub

```r
install.packages('devtools')
devtools::install_github('coreofscience/margaret')
```
## Load margaret
```r
library(margaret)
```

# Examples
```r
library(margaret)

# Load data in a dataframe of r
groups <- read.csv(".../groups_information.csv", header=T, sep=",")

margaret_data <- getting_data(groups)

#or just
margaret::getting_data(groups)
```

# Packages
For installing this package also installs a selection of other packages that you’re likely to use frequently, but probably not in every analysis.

[rvest](https://CRAN.R-project.org/package=rvest), For make it easy to download, then manipulate, HTML and XML.

[scholar](https://CRAN.R-project.org/package=scholar), For extract citation data from Google Scholar.

[stringi](https://CRAN.R-project.org/package=stringi), For processing tools for pattern.

[tidyverse](https://CRAN.R-project.org/package=tidyverse), for load core packages from tidyverse.

[writexl](https://CRAN.R-project.org/package=writexl), For Zero-dependency data frame to xlsx export.
