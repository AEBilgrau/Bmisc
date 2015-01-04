
Biostatistical miscellaneous functions and utilities
----------------------------------------------------
[![Build Status](https://api.travis-ci.org/AEBilgrau/Bmisc.svg?branch=master)](https://travis-ci.org/AEBilgrau/Bmisc)

My frequently used biostatistical miscellaneous R functions and utilities. While many functions are useful in general, the aim of package is to be particularly useful for general microarray analysis, gene network analysis, and other (bio)statistical applications.

While most functions are for build for convenience some are optimized for speed through the **Rcpp** and **RcppArmadillio** packages. **Bmisc** uses **roxygen2** for documentation of the package.

## Installation
To install the latest version of Bmisc directly from the master branch at GitHub, run 

```R
#install.packages("devtools")  # Uncomment if devtools is not installed
devtools::install_github("AEBilgrau/Bmisc")
```

The package is still under development and may be considered unstable. Be sure that you have the [package development prerequisites](http://www.rstudio.com/ide/docs/packages/prerequisites) if you wish to install the package from the source.

---
