# Info
In order to download the predictAUDPsyCoLaus package, please follow these instructions.

## Instructions
1. If not yet installed, install the R package `devtools`, so that the `predictAUDPsyCoLaus` package can be downloaded from GitHub (see step 5 below).
```R
install.packages("devtools")
```
2. The `predictAUDPsyCoLaus` package requires 15 other packages to be installed, of which three are only available on GitHub, whereas the others are available on CRAN.
3. First, install the three packages from GitHub:
```R
# https://github.com/mmiche/mysml
devtools::install_github(repo="https://github.com/mmiche/mysml",
                         dependencies = "Imports")
# https://github.com/stephenrho/pminternal
devtools::install_github(repo="https://github.com/stephenrho/pminternal",
                         dependencies = "Imports")
# https://github.com/mpavlou/samplesizedev
devtools::install_github(repo="https://github.com/mpavlou/samplesizedev",
                         dependencies = "Imports")
```
4. Second, install the remaining 12 packages from CRAN:
```R
install.packages(pkgs = c("dplyr", "magrittr", "tidyr", "ranger", "ggplot2",
                          "cowplot", "gridExtra", "pmcalibration", "ClinicalUtilityRecal", "rms",
                          "precrec", "modgo"), dependencies = TRUE)
```
5. Lastly, use the `devtools` package to install the `predictAUDPsyCoLaus` package:
```R
devtools::install_github(repo="https://github.com/mmiche/predictAUDPsyCoLaus",
                      dependencies = "Imports", build_vignettes = TRUE)
```

## First use of predictAUDPsyCoLaus
If the download, i.e., the installation, of the package was successful, load the package in R and make the help pages (the documentation of the package) appear:
```R
library(predictAUDPsyCoLaus)
help(package="predictAUDPsyCoLaus")
```
On the main page of the help pages, please note the detailed package vignette, by clicking on the `User guides, package vignettes and other documentation` link.