# Info
In order to download the predictAUDPsyCoLaus package, please follow these instructions.

## Instructions
1. If not yet installed, install the R package `remotes`, so that the `predictAUDPsyCoLaus` package can be downloaded from GitHub.
```R
install.packages("devtools")
```
2. The `predictAUDPsyCoLaus` package can now be installed from GitHub, which automatically will prompt to install (or update) other R packages.
```R
remotes::install_github(repo="https://github.com/mmiche/predictAUDPsyCoLaus",
                      dependencies = "Imports", build_vignettes = TRUE)
```

## First use of predictAUDPsyCoLaus
If the download, i.e., the installation, of the package was successful, load the package in R and make the help pages (the documentation of the package) appear:
```R
library(predictAUDPsyCoLaus)
help(package="predictAUDPsyCoLaus")
```
On the main page of the help pages, please note the detailed package vignette, by clicking on the `User guides, package vignettes and other documentation` link.
