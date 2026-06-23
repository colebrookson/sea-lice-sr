## .Rprofile
## Runs automatically at the start of every R session in this project.

# use here::here() to anchor all file paths to the project root
if (requireNamespace("here", quietly = TRUE)) here::i_am("DESCRIPTION")

# surface package conflicts explicitly rather than silently masking
if (requireNamespace("conflicted", quietly = TRUE)) {
  library(conflicted)
  # set project-wide conflict preferences here, e.g.:
  # conflict_prefer("filter", "dplyr")
  # conflict_prefer("select", "dplyr")
}

# keep the workspace clean between sessions
options(
  warnPartialMatchArgs  = TRUE,
  warnPartialMatchDollar = TRUE,
  warnPartialMatchAttr  = TRUE
)
