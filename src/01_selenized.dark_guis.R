# Create vscode (json) and RStudio (rstheme) variants using tmTheme as base

library(tidyverse)
library(jsonlite)
tminput <- "./extras/textmate/Selenized Dark.tmTheme"

# Beautify tmTheme
xml2::read_xml(tminput) %>%
  xml2::write_xml(tminput)

source("src/functions.R")

# VScode -----
output <- basename(tminput) %>%
  str_replace_all(".tmTheme", "-color-theme.json") %>%
  str_replace_all(" ", "-") %>%
  file.path("themes", .) |>
  tolower()

output

tmtheme2vscode(tminput, output)

# Modify name for store
js <- read_json(output)
js$name <- paste0(js$name, " Theme")

js |>
  write_json(path = output, auto_unbox = TRUE, pretty = TRUE)

# And get type of theme here
them_type <- read_json(output)$type

message(basename(tminput), " is ", them_type)

# RStudio Theme ----

# Create a first compilation
outdir <- "./extras/rstudio"
rtheme_out <- tools::file_path_sans_ext(tminput) |>
  basename() |>
  paste0(".rstheme") %>%
  file.path(outdir, .)


tmtheme2rstheme(tminput, rtheme_out)


# Apply the new theme
rstudioapi::addTheme(rtheme_out, apply = TRUE, force = TRUE)
