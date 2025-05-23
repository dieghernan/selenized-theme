read_tmtheme <- function(input) {
  options(dplyr.summarise.inform = FALSE)


  require(xml2)
  require(tidyverse)



  tmtheme <- read_xml(input)

  tmclean <- tmtheme |>
    as_list() |>
    rapply(function(x) {
      x <- trimws(x)

      # Guess if color and convert
      res <- try(col2rgb(x), silent = TRUE)

      res
      if (!inherits(res, "try-error")) {
        x <- rgb(t(res), maxColorValue = 255) |> toupper()
      }


      x <- gsub("  ", " ", x)
      x <- gsub("  ", " ", x)
      x <- gsub("  ", " ", x)
      x <- gsub("  ", " ", x)
      x <- gsub("  ", " ", x)
      x <- trimws(x)
      x
    }, how = "list")

  # Redo and clean XML
  xml2::as_xml_document(tmclean) |>
    xml2::write_xml(input)

  # Read back
  tm_lst <- read_xml(input) |> as_list()


  # Metadata
  meta_l <- tm_lst$plist$dict

  meta_n <- meta_l[names(meta_l) == "key"] |> unlist()
  meta_n <- meta_n[meta_n != "settings"]
  meta_val <- meta_l[names(meta_l) == "string"] |> unlist()

  meta_df <- tibble(
    section = "tmTheme Metadata",
    name = meta_n,
    value = meta_val
  )

  array_lst <- tmtheme |>
    xml_find_first("//array") |>
    xml_find_all(".//dict") |>
    as_list()


  # Base preproc
  array <- tm_lst$plist$dict$array

  # Top level vars
  topl <- lapply(array, function(x) {
    "string" %in% names(x)
  }) |>
    unlist() |>
    unname()


  # Top level
  topv <- array[!topl]$dict$dict
  nms_top <- names(topv)
  then <- topv[nms_top == "key"] |> unlist()
  vl <- topv[nms_top == "string"] |> unlist()

  top_df <- tibble(
    section = "Top-level config",
    name = then,
    value = vl
  )

  specs <- lapply(array[topl], function(x) {
    # No spec
    thisloop <- x
    nms_no <- names(thisloop)

    # Don't want dict
    no_spec <- thisloop[nms_no == "key"] |> unlist()
    no_spec <- no_spec[no_spec != "settings"]
    vals_no_spec_init <- thisloop[nms_no == "string"]
    vals_no_spec <- lapply(vals_no_spec_init, function(y) {
      if (length(y) == 0) {
        return("")
      }

      return(unlist(y))
    }) |> unlist()

    names(vals_no_spec) <- no_spec
    df_top <- as_tibble_row(vals_no_spec)





    # Specification
    specs <- thisloop$dict |> lapply(function(x) {
      if (length(x) == 0) {
        return("NULL")
      }
      x
    })

    # Spec df
    if (length(specs) == 0) {
      df_spec <- tibble(foreground = "")
    } else {
      # Spec df
      nm <- names(specs)
      val <- specs[nm == "string"] |> unlist()
      names(val) <- specs[nm == "key"] |> unlist()

      df_spec <- as_tibble_row(val)
    }


    bind_cols(df_top, df_spec)
  }) |>
    bind_rows()

  specs[specs == "NULL"] <- NA
  # Do unnesting
  scopes <- specs$scope

  newsc <- lapply(scopes, function(x) {
    this <- str_split(x, ",", simplify = TRUE) |> unlist()
    this[this != ""]
  })
  specs$scope <- newsc

  specs_end <- specs |>
    unnest(cols = scope) %>%
    distinct()

  specs_end$section <- "Scopes"

  # Final df
  template <- tibble(
    section = "Delete",
    name = "Delete",
    scope = NA,
    value = NA,
    foreground = NA,
    background = NA,
    fontStyle = NA
  )

  end <- bind_rows(template, meta_df, top_df, specs_end)
  end <- end[end$name != "Delete", ]
  alln <- names(end)
  end <- end %>%
    mutate(across(all_of(alln), str_squish)) |>
    distinct()
  end$foreground <- end$foreground
  end$background <- end$background


  # Now re-create the theme with clean interface
  # Settings
  set <- end |>
    filter(section == "Top-level config") |>
    select(name, value)

  ll <- NULL

  for (i in seq_len(nrow(set))) {
    this <- set[i, ]
    tm <- this$name |> as.character()
    col <- this$value |> as.character()
    ll <- c(ll, list(key = list(tm), string = list(col)))
  }

  # Fill the template
  setting <- list(
    dict = list(
      key = list("settings"),
      dict = ll
    )
  )

  # Now get the rest of params (token colors)
  tok <- end |>
    filter(section == "Scopes") |>
    select(-section, -value)

  tok$order <- seq_len(nrow(tok))

  tok_g <- tok |>
    group_by(name, foreground, background, fontStyle) |>
    summarise(minr = min(order), scope = paste0(scope, collapse = ", ")) |>
    arrange(minr) %>%
    select(-minr)

  ntok <- seq_len(nrow(tok_g))
  i <- 1
  for (i in ntok) {
    this <- tok_g[i, ]
    name <- unlist(this$name)
    if (length(name) == 0) {
      name <- ""
    }
    scope <- this$scope |>
      strsplit(",") |>
      unlist() |>
      trimws() |>
      sort() |>
      paste0(collapse = ", ")
    # message(i, " is ", scope)
    # Settings are ok

    onl <- list(
      dict = list(
        key = list("name"),
        string = list(name),
        key = list("scope"),
        string = list(scope),
        key = list("settings"),
        dict = list()
      )
    )

    dictt <- NULL
    settts <- this[, c("foreground", "background", "fontStyle")] |>
      unlist() |>
      as.list()

    settts <- settts[!is.na(settts)]



    if ("foreground" %in% names(settts)) {
      dictt <- c(dictt, list(
        key = list("foreground"),
        string = settts["foreground"] |> unname() |> paste0(collapse = " ") |> list()
      ))
    }
    if ("background" %in% names(settts)) {
      dictt <- c(dictt, list(
        key = list("background"),
        string = settts["background"] |> unname() |> paste0(collapse = " ") |> list()
      ))
    }
    if ("fontStyle" %in% names(settts)) {
      dictt <- c(dictt, list(
        key = list("fontStyle"),
        string = settts["fontStyle"] |> unname() |> paste0(collapse = " ") |> list()
      ))
    }

    if (!is.null(dictt)) {
      onl$dict$dict <- dictt

      setting <- c(setting, onl)
    }
  }

  tm_lst$plist$dict$array <- setting

  # Redo and clean XML
  xml2::as_xml_document(tm_lst) |>
    xml2::write_xml(input)


  # Output
  end
}


tmtheme2vscode <- function(tminput, output) {
  require(xml2)
  require(tidyverse)
  require(jsonlite)
  options(dplyr.summarise.inform = FALSE)

  # Based in https://github.com/microsoft/vscode-generator-code/blob/6e3f05ab46b6186e588094517764fdf42f21d094/generators/app/generate-colortheme.js#L237C18-L261C2
  mapping <- read_csv("src/mapping_themes.csv", show_col_types = FALSE)

  get_tmTheme <- read_tmtheme(tminput)

  # Dark?
  ss <- get_tmTheme |>
    filter(name == "semanticClass") |>
    pull(value)

  typ <- ifelse(grepl("dark", ss), "dark", "light")
  # message("This is ", typ)
  nm <- get_tmTheme |>
    filter(name == "name") |>
    pull(value)
  au <- get_tmTheme |>
    filter(name == "author") |>
    pull(value)
  thejson <- list(
    name = nm, author = au, semanticHighlighting = TRUE,
    type = typ
  )


  # get initial cols
  comm <- get_tmTheme |>
    filter(scope == "comment") |>
    pull(foreground)
  fg <- get_tmTheme |>
    filter(name == "foreground") |>
    pull(value)
  bg <- get_tmTheme |>
    filter(name == "background") |>
    pull(value)
  sel <- get_tmTheme |>
    filter(name == "selection") |>
    pull(value)
  accent <- get_tmTheme |>
    filter(name == "caret") |>
    pull(value)

  init <- additional_cols(bg, fg, comm, sel, accent)

  # Add mapping
  colorss <- get_tmTheme |>
    filter(section == "Top-level config") |>
    select(tm = name, color = value) |>
    inner_join(mapping, by = join_by(tm)) |>
    select(name = vscode, color)



  col_l <- colorss$color |> unlist()
  names(col_l) <- colorss$name |> unlist()
  col_l <- as.list(col_l)

  # If is hc then add rule
  hc <- grepl("_hc_", ss, fixed = TRUE)

  if (hc) {
    col_l$contrastBorder <- fg
    col_l$editor.selectionForeground <- accent
  }


  # Blend and sort
  col_end <- modifyList(init, col_l)

  col_end <- col_end[sort(names(col_end))]

  tokencols <- get_tmTheme |>
    filter(section == "Scopes") |>
    mutate(foreground = ifelse(tolower(foreground) == fg, NA,
      foreground
    )) |>
    select(name, scope, foreground, background, fontStyle)

  tokencols$index <- seq_len(nrow(tokencols))
  tok_g <- tokencols |>
    group_by(name) |>
    arrange(name, scope) |>
    group_by(name, foreground, background, fontStyle) |>
    summarise(sc = paste0(scope, collapse = ", "), minr = min(index)) |>
    arrange(minr)


  tok <- list()
  # Create list for tokens
  tok[[1]] <- list(settings = list(
    background = col_l$editor.background,
    foreground = col_l$editor.foreground
  ))
  toJSON(tok, auto_unbox = TRUE, pretty = TRUE)

  ntok <- seq_len(nrow(tok_g))

  i <- 6
  for (i in ntok) {
    thiscope <- tok_g[i, ]
    scp <- thiscope$sc |>
      as.character() |>
      strsplit(",") |>
      unlist() |>
      trimws()
    thistok <- list(
      name = thiscope$name,
      scope = scp,
      settings = list()
    )


    dictt <- list()
    fg <- thiscope$foreground |> unlist()
    bg <- thiscope$background |> unlist()
    fnt <- thiscope$fontStyle |> unlist()
    if (!is.na(fg)) {
      dictt <- c(dictt, list(foreground = fg))
    }
    if (!is.na(bg)) {
      dictt <- c(dictt, list(background = bg))
    }
    if (!is.na(fnt)) {
      dictt <- c(dictt, list(fontStyle = fnt))
    }
    if (length(dictt) > 0) {
      thistok$settings <- dictt
      toJSON(thistok, pretty = TRUE)

      tok[[i + 1]] <- thistok
    }
  }
  toJSON(tok, pretty = TRUE)

  vs_l <- c(thejson, list(tokenColors = tok), list(colors = col_end))

  write_json(vs_l, path = output, auto_unbox = TRUE, pretty = TRUE)
  return(invisible(NULL))
}

additional_cols <- function(bg, fg, comment, selection, accent) {
  require(colorspace)
  bgaccent1 <- mixcolor(0.98, hex2RGB(accent), hex2RGB(bg)) |> hex()
  bgaccent2 <- mixcolor(0.80, hex2RGB(accent), hex2RGB(bg)) |> hex()
  bgfg1 <- mixcolor(0.90, hex2RGB(fg), hex2RGB(bg)) |> hex()
  bgfg2 <- mixcolor(0.70, hex2RGB(fg), hex2RGB(bg)) |> hex()

  list(
    # Integrated Terminal Colors
    "terminal.background" = bg,
    "terminal.foreground" = fg,
    "terminal.cursor" = accent,
    "terminalCursor.background" = bg,
    "terminalCursor.foreground" = accent,
    "terminal.border" = bgaccent2,

    # Base Colors
    "focusBorder" = accent,
    "foreground" = fg,

    # Button Control
    "button.background" = accent,
    "button.foreground" = bg,
    "button.secondaryBackground" = bgaccent1,
    "button.secondaryForeground" = fg,

    # Dropdown Control
    "dropdown.background" = bgfg1,
    "dropdown.foreground" = fg,

    # Input Control
    "input.background" = bgfg1,
    "input.foreground" = fg,
    "input.placeholderForeground" = comment,

    # Badge
    "badge.background" = accent,
    "badge.foreground" = bg,

    # Progress Bar
    "progressBar.background" = accent,

    # List & Trees
    "list.activeSelectionBackground" = selection,
    "list.activeSelectionForeground" = fg,
    "list.dropBackground" = selection,
    "list.hoverBackground" = selection,
    "list.inactiveSelectionBackground" = bgfg2,
    "list.highlightForeground" = accent,
    "list.focusBackground" = selection,

    # Activity Bar

    "activityBar.activeBackground" = bgaccent2,
    "activityBar.inactiveForeground" = comment,
    "activityBar.foreground" = accent,
    "activityBar.background" = bgaccent1,
    "activityBarBadge.background" = accent,
    "activityBarBadge.foreground" = bg,

    # Side Bar
    "sideBar.background" = bgfg1,
    "sideBar.foreground" = fg,
    "sideBarSectionHeader.background" = bg,
    "sideBarTitle.foreground" = fg,
    "sideBarTitle.background" = bgaccent1,

    # Editor Group & Tabs
    "editorGroupHeader.tabsBackground" = bgaccent1,
    "tab.activeBackground" = bgaccent2,
    "tab.activeForeground" = accent,
    "tab.inactiveBackground" = bgfg1,
    "tab.inactiveForeground" = fg,

    # Editor Colors
    "editor.background" = bg,
    "editor.foreground" = fg,
    "editor.lineHighlightBorder" = selection,
    "editor.selectionBackground" = selection,
    "editor.snippetFinalTabstopHighlightBackground" = bg,
    "editor.snippetTabstopHighlightBackground" = bg,
    "editor.snippetTabstopHighlightBorder" = comment,
    "editorBracketHighlight.foreground1" = fg,
    "editorCodeLens.foreground" = comment,
    "editorHoverWidget.background" = bgaccent1,
    "editorHoverWidget.border" = comment,
    "editorLineNumber.foreground" = comment,
    "editorSuggestWidget.foreground" = fg,
    "editorSuggestWidget.background" = bgaccent1,
    "editorSuggestWidget.focusHighlightForeground" = accent,
    "editorSuggestWidget.highlightForeground" = accent,
    "editorSuggestWidget.selectedBackground" = bgaccent1,
    "editorSuggestWidget.selectedIconForeground" = accent,
    "editorWidget.background" = bgaccent1,

    # Peek View Colors
    "peekView.border" = selection,
    "peekViewEditor.background" = bg,
    "peekViewResult.fileForeground" = fg,
    "peekViewResult.lineForeground" = fg,
    "peekViewResult.selectionBackground" = selection,
    "peekViewResult.selectionForeground" = fg,
    "peekViewTitleDescription.foreground" = comment,
    "peekViewTitleLabel.foreground" = fg,

    # Panel Colors

    "panel.background" = bgfg1,
    "panelTitle.activeForeground" = fg,
    "panelTitle.inactiveForeground" = comment,

    # Status Bar Colors
    "statusBar.background" = bgaccent2,
    "statusBar.foreground" = fg,
    "statusBar.noFolderForeground" = fg,
    "statusBar.noFolderBackground" = selection,
    "statusBarItem.remoteForeground" = bg,

    # Title Bar Colors (MacOS Only)
    "titleBar.activeForeground" = fg,
    "titleBar.activeBackground" = bgaccent1,
    "titleBar.inactiveForeground" = comment,

    # Setting Editor
    "settings.checkboxForeground" = fg,
    "settings.dropdownForeground" = fg,
    "settings.headerForeground" = fg,
    "settings.numberInputForeground" = fg,
    "settings.textInputForeground" = fg,

    # Breadcrumbs

    "breadcrumb.activeSelectionForeground" = fg,
    "breadcrumb.background" = bgfg1,
    "breadcrumb.focusForeground" = fg,
    "breadcrumb.foreground" = comment,

    # Misc
    "gitDecoration.ignoredResourceForeground" = comment,
    "scrollbarSlider.background" = bgaccent2,
    "scrollbarSlider.activeBackground" = bgfg2,
    "icon.foreground" = accent,
    "menu.background" = bgaccent1,
    "menu.foreground" = fg,
    "menu.separatorBackground" = bgaccent2,
    "menubar.selectionBackground" = selection,
    "menu.selectionBackground" = selection,
    "notifications.background" = bgaccent1,
    "notificationLink.foreground" = accent,
    "editorLink.activeForeground" = accent,
    "keybindingLabel.foreground" = fg,
    "keybindingLabel.background" = bg,
    "pickerGroup.foreground" = accent,
    "pickerGroup.border" = bgaccent1,
    "textLink.foreground" = accent
  )
}

tmtheme2rstheme <- function(tminput, rtheme_out) {
  require(tidyverse)
  require(xml2)
  require(sass)

  ## Additional colors -----
  source("src/functions.R")
  tmcols <- read_tmtheme(tminput)
  mapping <- read_csv("src/mapping_themes.csv", show_col_types = FALSE)

  ### Rules ----


  tmcols_clean <- tmcols |>
    filter(section != "Scopes") |>
    mutate(
      tm = coalesce(scope, name),
      fg = coalesce(foreground, value),
      bg = background,
      fontweight = ifelse(str_detect(fontStyle, "old"), "bold", NA),
      fontstyle = ifelse(str_detect(fontStyle, "talic"), "italic", NA)
    ) |>
    select(tm, fg, bg, fontweight, fontstyle)


  col2add <- tmcols_clean |>
    inner_join(mapping, by = "tm") |>
    filter(!is.na(rstheme)) |>
    select(rstheme, fg:fontstyle)

  # Mapping of scopes to ace_editor


  tmcols_scopes <- tmcols |>
    filter(section == "Scopes") |>
    mutate(
      tm = coalesce(scope, name),
      fg = coalesce(foreground, value),
      bg = background,
      fontweight = ifelse(str_detect(fontStyle, "old"), "bold", NA),
      fontstyle = ifelse(str_detect(fontStyle, "talic"), "italic", NA)
    ) |>
    select(tm, fg, bg, fontweight, fontstyle) |>
    arrange(tm)

  tmcols_scopes <- tmcols_scopes |>
    filter(str_detect(tm, "link")) |>
    filter(str_detect(tm, " ", negate = TRUE)) |>
    mutate(tm = "markup.href") |>
    bind_rows(tmcols_scopes) |>
    distinct() |>
    arrange(tm)

  tmcols_scopes <- tmcols_scopes |>
    filter(str_detect(tm, "markup.heading")) |>
    mutate(tm = "heading") |>
    bind_rows(tmcols_scopes) |>
    distinct() |>
    arrange(tm)

  tmcols_scopes <- tmcols_scopes |>
    filter(tm %in% c(
      "entity.name.tag.html",
      "meta.tag"
    )) |>
    mutate(tm = "meta.tag") |>
    bind_rows(tmcols_scopes) |>
    distinct() |>
    arrange(tm)

  tmcols_scopes <- tmcols_scopes |>
    filter(tm == "comment") |>
    mutate(tm = "xml-pe") |>
    bind_rows(tmcols_scopes) |>
    distinct() |>
    arrange(tm)

  # Workout levels
  lev3 <- tmcols_scopes |>
    filter(str_count(tm, fixed(".")) == 2)

  lev2 <- tmcols_scopes |>
    filter(str_count(tm, fixed(".")) == 1)


  lev1 <- tmcols_scopes |>
    filter(str_count(tm, fixed(".")) == 0)

  lev3 <- lev3 |>
    group_by(tm) |>
    mutate(n_times = n()) |>
    arrange(desc(n_times)) |>
    slice_max(n = 1, with_ties = FALSE, order_by = n_times) |>
    select(-n_times)

  # Convert in lev2 to enrich
  newlev2 <- lev3 |>
    mutate(new_tm = str_split_fixed(tm, fixed("."), n = 3)[1:2] |>
      paste0(collapse = ".")) |>
    group_by(new_tm) |>
    group_map(function(x, y) {
      x |>
        group_by(fg, bg, fontweight, fontstyle) |>
        summarise(n = n()) |>
        ungroup() |>
        arrange(desc(n)) |>
        slice_head(n = 1) |>
        mutate(tm = unlist(y)) |>
        select(tm, fg:fontstyle)
    }, .keep = TRUE) |>
    bind_rows() |>
    arrange(tm)


  lev2 <- newlev2 |>
    filter(!tm %in% lev2$tm) |>
    bind_rows(lev2) |>
    arrange(tm) |>
    distinct() |>
    ungroup()



  # Convert in lev1 to enrich

  lev1_vars <- str_split_fixed(lev2$tm, fixed("."), n = 2)[, 1] |>
    unlist()
  newlev1 <- lev2 |>
    mutate(new_tm = lev1_vars) |>
    group_by(new_tm) |>
    group_map(function(x, y) {
      x |>
        group_by(fg, bg, fontweight, fontstyle) |>
        summarise(n = n()) |>
        ungroup() |>
        arrange(desc(n)) |>
        slice_head(n = 1) |>
        mutate(tm = unlist(y)) |>
        select(tm, fg:fontstyle)
    }, .keep = TRUE) |>
    bind_rows() |>
    arrange(tm)


  lev1 <- newlev1 |>
    filter(!tm %in% lev1$tm) |>
    bind_rows(lev1) |>
    arrange(tm) |>
    distinct() |>
    ungroup()



  ace_scopes <- lev1 |>
    bind_rows(lev2, lev3) |>
    arrange(tm) |>
    ungroup() |>
    group_by(tm) |>
    slice_head(n = 1) |>
    mutate(
      rstheme = str_replace_all(tm, fixed("."), ".ace_"),
      rstheme = paste0(".ace_", rstheme)
    ) |>
    filter(!is.na(rstheme))


  col2add <- col2add |>
    bind_rows(ace_scopes) |>
    select(rstheme:fontstyle)

  new_css <- c("/* Rules from tmTheme */", "")
  cssrule <- ".ace_heading"
  for (cssrule in col2add$rstheme) {
    thisval <- col2add[col2add$rstheme == cssrule, ]
    if (cssrule == ".ace_print-margin") {
      thisrule <- paste0(".ace_print-margin {background: ", thisval$fg, ";}")
      new_css <- c(new_css, thisrule, "")
    } else {
      newr <- list(
        color = thisval$fg,
        "background-color" = thisval$bg,
        "font-weight" = thisval$fontweight,
        "font-style" = thisval$fontstyle
      )
      newr_clean <- newr[!is.na(newr)]
      specs <- paste0(names(newr_clean), ": ", newr_clean, ";", collapse = " ")
      thisrule <- paste0(cssrule, " {", specs, "}")
      new_css <- c(new_css, thisrule, "")
    }
  }


  ### Compiled vars ----

  fg_col <- tmcols |>
    filter(name == "foreground") |>
    pull(value) %>%
    paste0("$fg: ", ., ";")

  bg_col <- tmcols |>
    filter(name == "background") |>
    pull(value) %>%
    paste0("$bg: ", ., ";")

  accent_col <- tmcols |>
    filter(name == "caret") |>
    pull(value) %>%
    paste0("$accent: ", ., ";")

  sel_col <- tmcols |>
    filter(name == "selection") |>
    pull(value) %>%
    paste0("$selection: ", ., ";")

  com_col <- tmcols |>
    filter(str_detect(scope, "comment") & !is.na(foreground)) |>
    arrange(scope) |>
    slice_head(n = 1) |>
    pull(foreground) %>%
    paste0("$comment: ", ., ";")

  # SASS stylesheet
  compiler <- readLines("./src/_better_rstheme.scss")

  ## Build ----

  # Create a first compilation
  rstudioapi::convertTheme(tminput,
    add = FALSE, outputLocation = outdir,
    force = TRUE
  )


  # Read the lines of the auto-generated rstheme (is a css)
  # and add new css rules and extra cols
  v <- rstudioapi::versionInfo()

  vtext <- paste0(
    "/* Generated and tested in RStudio ", v$long_version,
    ' ("', v$release_name, '") */'
  )

  readLines(rtheme_out) %>%
    # New rules
    c(vtext, "", new_css) %>%
    # Compilers
    c(fg_col, bg_col, accent_col, sel_col, com_col, compiler) |>
    # Compile and write
    sass::sass(output = rtheme_out, cache = FALSE)
  return(invisible())
}
