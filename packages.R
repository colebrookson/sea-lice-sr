## packages.R
## Source this file at the top of any script to load all project dependencies.
## Packages are declared in DESCRIPTION; add new ones there, not here.

d <- read.dcf("DESCRIPTION", c("Depends", "Imports"))
pkgs <- unlist(strsplit(paste(d[1, ], collapse = ","), ","))
pkgs <- trimws(gsub("\\(.*\\)", "", pkgs)) # strip version specs like (>= 4.4)
pkgs <- setdiff(pkgs, "R") # drop the R pseudo-package

invisible(lapply(pkgs, function(p) {
    library(p, character.only = TRUE)
}))
