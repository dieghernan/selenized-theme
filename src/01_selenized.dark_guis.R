# Create vscode (json) and RStudio (rstheme) variants using tmTheme as base

library(tidyverse)
library(jsonlite)
tminput <- "./dist/tmtheme/Selenized Dark.tmTheme"

# VScode -----
output <- basename(tminput) %>%
  str_replace_all(".tmTheme", "-color-theme.json") %>%
  str_replace_all(" ", "-") %>%
  file.path("dist", "vscode", "themes", .) |>
  tolower()

output

rstudiothemes::convert_tm_to_vs_theme(tminput, output)


# Modify name for store
js <- read_json(output)
js$name <- paste0(js$name, " Theme")

js |>
  write_json(path = output, auto_unbox = TRUE, pretty = TRUE)

# And get type of theme here
them_type <- read_json(output)$type

message(basename(tminput), " is ", them_type)
