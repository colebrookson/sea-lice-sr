## packages.R
## Source this file at the top of any script to load all project dependencies.
## Packages are declared in DESCRIPTION, add new ones there pls!
## Dependency resolution goes through pak::local_deps(), not hand-parsed
## DESCRIPTION text

deps <- pak::local_deps(root = ".")
pkgs <- unique(deps$package[deps$package != "R"])

invisible(lapply(pkgs, function(p) {
  library(p, character.only = TRUE)
}))
