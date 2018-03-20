#!/usr/bin/env Rscript

install_package <- function(package) {
  if (!require(package,character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
    install.packages(as.character(package), repos = "http://cran.us.r-project.org")
    library(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }
}

packages <- c("devtools", "roxygen2", "testthat", "knitr")

lapply(packages, install_package);

devtools::install_github("hadley/devtools")

devtools::create_package("route/here/predictivadbr")
