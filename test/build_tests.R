# source("src/build_themes.R")

theme <- "selenized.dark"

paste0("_", theme, "_hljs.scss") %>%
  file.path("src", "themes", .) |>
  readLines() |>
  sass::sass(cache = FALSE, output = "test/test_hljs.css")


paste0("_", theme, "_prismjs.scss") %>%
  file.path("src", "themes", .) |>
  readLines() |>
  sass::sass(cache = FALSE, output = "test/test_prismjs.css")

paste0("_", theme, "_pygments.scss") %>%
  file.path("src", "themes", .) |>
  readLines() |>
  sass::sass(cache = FALSE, output = "test/test_pygments.css")
