source("src/build_themes.R")

theme <- "selenized.dark.scss"

file.path("dist", "hljs", theme) |>
  readLines() |>
  sass::sass(cache = FALSE, output = "test/test_hljs.css")

file.path("dist", "prismjs", theme) |>
  readLines() |>
  sass::sass(cache = FALSE, output = "test/test_prismjs.css")

file.path("dist", "pygments", theme) |>
  readLines() |>
  sass::sass(cache = FALSE, output = "test/test_pygments.css")
