# census_report.R
# Library of functions for generating a two-sheet Excel report of Census data
# for an appraisal study area.
#
# Sheet 1 (Trends)       : one region, 4 censuses (2006 / 2011 / 2016 / 2021)
#                          - Population, Population % Change, Total Private Dwellings,
#                            Occupied Dwellings by Structural Type
# Sheet 2 (Demographics) : multiple regions, 2021 census only
#                          - Age, Marital Status, Age of Dwellings, Tenure,
#                            Shelter Cost, Income
#
# Depends on: cancensus, openxlsx, dplyr, tidyr, purrr, stringr
#
# First-time setup (run once):
#   install.packages(c("cancensus","openxlsx","dplyr","tidyr","purrr","stringr"))
#   cancensus::set_cancensus_api_key("YOUR_KEY", install = TRUE)
#   cancensus::set_cancensus_cache_path("~/cancensus_cache", install = TRUE)
#   # restart R so .Renviron is re-read

suppressPackageStartupMessages({
  library(cancensus)
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
})

# ---- Dataset identifiers per census year ------------------------------------

DATASETS <- c(`2006` = "CA06", `2011` = "CA11", `2016` = "CA16", `2021` = "CA21")

# ---- Vector lookup ---------------------------------------------------------
#
# cancensus assigns a unique vector ID per (census year, variable). Rather than
# hard-coding IDs (which change between vintages), we look them up by their
# published label using list_census_vectors(). Each field below specifies:
#   - label  : regex matched against the "label" column (leaf variable)
#   - parent : regex matched against the parent's label, used to disambiguate
#              variables that share a common leaf name (e.g. "Total" appears
#              under dozens of headers). Pass NA to skip parent matching.
#
# If a year's published label differs, adjust the regex here; run
# verify_vectors() to confirm resolution.

resolve_vector <- function(dataset, label_regex, parent_regex = NA_character_) {
  vecs <- list_census_vectors(dataset, use_cache = TRUE, quiet = TRUE)
  hits <- vecs[grepl(label_regex, vecs$label, perl = TRUE), ]
  # Every demographic variable is published three times (Total / Men+ / Women+
  # or Total / Male / Female). Prefer the Total row.
  if (nrow(hits) > 1 && "type" %in% names(hits)) {
    totals <- hits[hits$type == "Total", ]
    if (nrow(totals) >= 1) hits <- totals
  }
  if (!is.na(parent_regex) && nrow(hits) > 1) {
    parent_labels <- vecs$label[match(hits$parent_vector, vecs$vector)]
    hits <- hits[grepl(parent_regex, parent_labels, perl = TRUE), ]
  }
  if (nrow(hits) == 0) {
    # Pull near-miss candidates. Use the longest literal word from the regex
    # (stripping metacharacters) so results aren't over-constrained.
    words <- unlist(regmatches(label_regex,
                               gregexpr("[A-Za-z][A-Za-z-]{3,}", label_regex)))
    keyword <- if (length(words) == 0) label_regex
               else words[which.max(nchar(words))]
    cand <- vecs[grepl(keyword, vecs$label, ignore.case = TRUE, perl = TRUE), ]
    cand <- head(cand, 100)
    cand_txt <- if (nrow(cand) == 0)
                  sprintf("  (no labels in %s contain '%s')", dataset, keyword)
                else paste(sprintf("  %s  |  %s", cand$vector, cand$label),
                           collapse = "\n")
    stop(sprintf("No vector found in %s for /%s/ (parent /%s/)\nCandidates containing '%s':\n%s",
                 dataset, label_regex, parent_regex, keyword, cand_txt),
         call. = FALSE)
  }
  if (nrow(hits) > 1)
    stop(sprintf("Ambiguous in %s for /%s/ (parent /%s/):\n%s",
                 dataset, label_regex, parent_regex,
                 paste(sprintf("  %s  |  %s", hits$vector, hits$label),
                       collapse = "\n")),
         call. = FALSE)
  hits$vector
}

# Resolve a list of fields (name = list(label=..., parent=...)) into a
# named character vector of vector IDs.
resolve_fields <- function(dataset, fields) {
  map_chr(fields, \(f) resolve_vector(dataset, f$label, f$parent %||% NA))
}

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

# ---- Field catalogs --------------------------------------------------------

# Table 1 — Trends. Population and Dwellings come from cancensus's built-in
# columns (which exist regardless of census vintage), so they aren't listed
# here. Only the structural-type breakdowns need vector lookup.
TRENDS_FIELDS <- list(
  single_detached     = list(label = "^Single-detached house$",                                   parent = "structural type"),
  apt_ge5             = list(label = "five or more storeys",                                      parent = "structural type"),
  apt_lt5             = list(label = "fewer than five storeys",                                   parent = "structural type"),
  semi_detached       = list(label = "^Semi-detached house$",                                     parent = "structural type"),
  row_house           = list(label = "^Row house$",                                               parent = "structural type"),
  apt_duplex          = list(label = "duplex",                                                    parent = "structural type"),
  movable             = list(label = "^Movable dwelling$",                                        parent = "structural type"),
  other_attached      = list(label = "^Other single-attached house$",                             parent = "structural type")
)

# Table 2 — Demographics (2021 only)
DEMO_FIELDS <- list(
  # Age (pop_total comes from the built-in Population column).
  # Parent in CA21 is "Total - Age"; earlier censuses used "Age groups" — match either.
  age_0_14            = list(label = "^0 to 14 years$",                                           parent = "Age"),
  age_15_64           = list(label = "^15 to 64 years$",                                          parent = "Age"),
  age_65_plus         = list(label = "^65 years and (over|older)$",                                parent = "Age"),
  median_age          = list(label = "^Median age$",                                              parent = NA,
                              agg   = "wmean_pop"),
  # Household size (apartment unit-mix signal)
  hh_size_total       = list(label = "^Private households by household size$",                   parent = NA),
  hh_size_1           = list(label = "^1 person$",                                                parent = "household size"),
  hh_size_2           = list(label = "^2 persons$",                                               parent = "household size"),
  hh_size_3           = list(label = "^3 persons$",                                               parent = "household size"),
  hh_size_4           = list(label = "^4 persons$",                                               parent = "household size"),
  hh_size_5plus       = list(label = "^5 or more persons$",                                       parent = "household size"),
  avg_hh_size         = list(label = "^Average household size$",                                  parent = NA,
                              agg   = "wmean_hh"),
  # Occupied private dwellings by number of bedrooms (existing supply side)
  bed_total           = list(label = "^Total.*by number of bedrooms$",                            parent = NA),
  bed_0               = list(label = "^No bedrooms$",                                             parent = "number of bedrooms"),
  bed_1               = list(label = "^1 bedroom$",                                               parent = "number of bedrooms"),
  bed_2               = list(label = "^2 bedrooms$",                                              parent = "number of bedrooms"),
  bed_3               = list(label = "^3 bedrooms$",                                              parent = "number of bedrooms"),
  bed_4plus           = list(label = "^4 or more bedrooms$",                                      parent = "number of bedrooms"),
  # Period of construction (25% sample)
  period_total        = list(label = "^Total.*period of construction",                            parent = NA),
  built_1960          = list(label = "^1960 or before$",                                          parent = "period of construction"),
  built_1961_1980     = list(label = "^1961 to 1980$",                                            parent = "period of construction"),
  built_1981_1990     = list(label = "^1981 to 1990$",                                            parent = "period of construction"),
  built_1991_2000     = list(label = "^1991 to 2000$",                                            parent = "period of construction"),
  built_2001_2005     = list(label = "^2001 to 2005$",                                            parent = "period of construction"),
  built_2006_2010     = list(label = "^2006 to 2010$",                                            parent = "period of construction"),
  built_2011_2015     = list(label = "^2011 to 2015$",                                            parent = "period of construction"),
  built_2016_2021     = list(label = "^2016 to 2021$",                                            parent = "period of construction"),
  # Tenure
  tenure_total        = list(label = "^Total.*tenure",                                            parent = NA),
  owner               = list(label = "^Owner$",                                                   parent = "tenure"),
  renter              = list(label = "^Renter$",                                                  parent = "tenure"),
  # Shelter cost (2020 reference year in 2021 census)
  median_dwelling_val = list(label = "^Median value of dwellings",                                parent = NA,
                              agg   = "wmean_hh"),
  median_rent         = list(label = "^Median monthly shelter costs for rented dwellings",        parent = NA,
                              agg   = "wmean_hh"),
  # Income (2020 reference year)
  median_ind_income   = list(label = "^Median total income in 2020 among recipients",             parent = NA,
                              agg   = "wmean_pop"),
  median_hh_income    = list(label = "^Median total income of household in 2020",                 parent = NA,
                              agg   = "wmean_hh"),
  # Shelter-cost stress (tenant households) — cancensus publishes this as a
  # pre-computed percentage (0-100 scale).
  tenant_stir_30      = list(label = "^% of tenant households spending 30% or more",              parent = NA,
                              agg   = "wmean_hh")
)

verify_vectors <- function(dataset = "CA21", fields = DEMO_FIELDS) {
  ids <- tryCatch(resolve_fields(dataset, fields), error = function(e) {
    message(e$message); return(NULL)
  })
  if (is.null(ids)) return(invisible(NULL))
  vecs <- list_census_vectors(dataset, use_cache = TRUE, quiet = TRUE)
  out <- data.frame(
    field    = names(ids),
    vector   = unname(ids),
    label    = vecs$label[match(ids, vecs$vector)],
    stringsAsFactors = FALSE
  )
  print(out, row.names = FALSE)
  invisible(out)
}

# ---- Region lookup ---------------------------------------------------------
#
# Search census geographies by name. Returns a data frame of matches sorted
# by geography level (broadest first) and population (largest first).
#
#   find_region("Winnipeg")
#   find_region("Steinbach", level = "CSD")
#   find_region("Hanover", dataset = "CA21")   # find a rural municipality
#   find_region("^Winnipeg$", level = "CMA")   # exact match via regex
#
# Levels returned by cancensus include: C (country), PR (province),
# CMA/CA, CD (census division), CSD, CT (tract), DA (dissemination area).

find_region <- function(pattern, level = NULL, dataset = "CA21", top = 25) {
  regs <- list_census_regions(dataset, use_cache = TRUE, quiet = TRUE)
  hits <- regs[grepl(pattern, regs$name, ignore.case = TRUE, perl = TRUE), ]
  if (!is.null(level)) hits <- hits[hits$level %in% level, ]
  level_order <- c("C","PR","CMA","CA","CD","CSD","CT","DA","DB")
  hits <- hits[order(match(hits$level, level_order), -as.numeric(hits$pop)), ]
  hits <- head(hits, top)
  out <- data.frame(
    level  = hits$level,
    id     = hits$region,
    name   = hits$name,
    pop    = hits$pop,
    stringsAsFactors = FALSE
  )
  print(out, row.names = FALSE)
  invisible(out)
}

# ---- Region helpers --------------------------------------------------------
#
# A "region spec" is a named list of geography -> character vector of IDs:
#   list(CSD = "4602042")                        # Steinbach
#   list(CSD = "4611040")                        # City of Winnipeg
#   list(CMA = "602")                            # Winnipeg CMA
#   list(PR  = "46")                             # Manitoba
#   list(CT = c("6020001.00","6020002.00"))      # two Census Tracts
#   list(CSD = "4602042", DA = c("..."))         # CSD + extra DAs (unioned)
#
# Pass level = "Regions" to get_census() to have cancensus sum the components
# into a single aggregated row.

fetch_region <- function(region_spec, dataset, fields) {
  vector_ids <- resolve_fields(dataset, fields)
  df <- get_census(
    dataset    = dataset,
    regions    = region_spec,
    vectors    = unname(vector_ids),
    level      = "Regions",
    use_cache  = TRUE,
    quiet      = TRUE,
    geo_format = NA
  )
  # cancensus returns vector columns as "v_XXXX: label"; match by prefix.
  # When region_spec unions multiple areas (e.g. DAs for a Winnipeg cluster),
  # cancensus returns one row per component. Count fields are summed; medians,
  # averages, and percentages are weighted-mean'd so we don't report nonsense
  # like "summed percentages" for aggregated areas. Weights use the built-in
  # Population or Households columns — true weights (e.g. renter count for
  # rent medians) would be more precise but require extra vector fetches;
  # Population / Households are close enough for appraisal commentary and
  # the report footnotes the approximation when DA-aggregated.
  agg_value <- function(df, col, agg) {
    if (is.na(col)) return(NA)
    vals <- df[[col]]
    if (all(is.na(vals))) return(NA)
    switch(agg,
      sum       = sum(vals, na.rm = TRUE),
      wmean_pop = stats::weighted.mean(vals, df$Population, na.rm = TRUE),
      wmean_hh  = stats::weighted.mean(vals, df$Households, na.rm = TRUE),
      sum(vals, na.rm = TRUE))
  }
  out <- list()
  for (nm in names(fields)) {
    vid <- vector_ids[[nm]]
    col <- grep(sprintf("^%s(:|$)", vid), names(df), value = TRUE)[1]
    agg <- fields[[nm]]$agg %||% "sum"
    out[[nm]] <- agg_value(df, col, agg)
  }
  out$population      <- if ("Population" %in% names(df)) sum(df$Population, na.rm = TRUE) else NA
  out$total_dwellings <- if ("Dwellings"  %in% names(df)) sum(df$Dwellings,  na.rm = TRUE) else NA
  out$households      <- if ("Households" %in% names(df)) sum(df$Households, na.rm = TRUE) else NA
  out
}

# ---- Table 1 (Trends) ------------------------------------------------------

build_trends <- function(region_spec) {
  years <- names(DATASETS)
  rows <- map(years, function(y) {
    ds <- DATASETS[[y]]
    vals <- fetch_region(region_spec, ds, TRENDS_FIELDS)
    vals$year <- as.integer(y)
    vals
  })
  df <- bind_rows(rows) |> relocate(year)
  # Population % change vs prior census row
  df$pop_pct_change <- c(NA, diff(df$population) / head(df$population, -1))
  df
}

# ---- Table 2 (Demographics) ------------------------------------------------

build_demographics <- function(regions) {
  ds <- "CA21"
  rows <- imap(regions, function(spec, nm) {
    vals <- fetch_region(spec, ds, DEMO_FIELDS)
    c(region = nm, vals)
  })
  bind_rows(rows)
}

# ---- Excel writing ---------------------------------------------------------
#
# Layout mirrors Jason's existing Excel style:
#   - red border on the bounding rectangle
#   - bold merged title bar
#   - bold section-header rows
#   - paired Count / % columns per region on Sheet 2

red_border <- function() openxlsx::createStyle(border = "TopBottomLeftRight",
                                               borderColour = "#C00000",
                                               borderStyle  = "thin")

title_style <- function() openxlsx::createStyle(
  fontSize = 12, textDecoration = "bold",
  halign = "center", valign = "center",
  fgFill = "#FFFFFF",
  border = "TopBottomLeftRight", borderColour = "#C00000")

section_style <- function() openxlsx::createStyle(
  textDecoration = "bold",
  fgFill = "#F2F2F2",
  border = "TopBottomLeftRight", borderColour = "#C00000")

header_style <- function() openxlsx::createStyle(
  textDecoration = "bold", halign = "center",
  border = "TopBottomLeftRight", borderColour = "#C00000")

body_style <- function() openxlsx::createStyle(
  border = "TopBottomLeftRight", borderColour = "#C00000")

write_trends_sheet <- function(wb, sheet, region_name, trends) {
  addWorksheet(wb, sheet)
  setColWidths(wb, sheet, cols = 1, widths = 34)
  setColWidths(wb, sheet, cols = 2:5, widths = 11)

  years <- trends$year
  yr_cols <- as.character(years)

  # Title row
  writeData(wb, sheet, sprintf("Population & Dwelling Trends - %s", region_name),
            startCol = 1, startRow = 1)
  mergeCells(wb, sheet, cols = 1:(1 + length(years)), rows = 1)
  addStyle(wb, sheet, title_style(),
           rows = 1, cols = 1:(1 + length(years)), gridExpand = TRUE)

  # Year header row
  writeData(wb, sheet, data.frame(matrix(yr_cols, nrow = 1)),
            startCol = 2, startRow = 2, colNames = FALSE)
  addStyle(wb, sheet, header_style(),
           rows = 2, cols = 2:(1 + length(years)), gridExpand = TRUE)

  # Row data: label -> vector of values across years
  body_rows <- list(
    list("Population",                              trends$population,                      "#,##0"),
    list("Population % Change",                     trends$pop_pct_change,                  "0%"),
    list("Total Private Dwellings*",                trends$households,                      "#,##0"),
    list("Occupied Dwellings by Type*",             rep(NA, length(years)),                 NA),
    list("Single-Detached House",                   trends$single_detached,                 "#,##0"),
    list("Apartment (<5 Storeys)",                  trends$apt_lt5,                         "#,##0"),
    list("Apartment (5+ Storeys)",                  trends$apt_ge5,                         "#,##0"),
    list("Semi-Detached House",                     trends$semi_detached,                   "#,##0"),
    list("Row House",                               trends$row_house,                       "#,##0"),
    list("Apt or Flat in a Duplex",                 trends$apt_duplex,                      "#,##0"),
    list("Movable Dwelling",                        trends$movable,                         "#,##0"),
    list("Other",                                   trends$other_attached,                  "#,##0")
  )

  for (i in seq_along(body_rows)) {
    r <- 2 + i
    label <- body_rows[[i]][[1]]
    vals  <- body_rows[[i]][[2]]
    fmt   <- body_rows[[i]][[3]]
    writeData(wb, sheet, label, startCol = 1, startRow = r)
    if (is.na(fmt)) {
      # Section header
      addStyle(wb, sheet, section_style(),
               rows = r, cols = 1:(1 + length(years)), gridExpand = TRUE)
    } else {
      writeData(wb, sheet, data.frame(matrix(vals, nrow = 1)),
                startCol = 2, startRow = r, colNames = FALSE)
      addStyle(wb, sheet, openxlsx::createStyle(
        numFmt = fmt,
        border = "TopBottomLeftRight", borderColour = "#C00000"),
        rows = r, cols = 2:(1 + length(years)), gridExpand = TRUE)
      addStyle(wb, sheet, body_style(), rows = r, cols = 1, gridExpand = TRUE)
    }
  }

  # Footnote
  fn_row <- 2 + length(body_rows) + 1
  writeData(wb, sheet, "* Occupied by usual residents",
            startCol = 1, startRow = fn_row)
}

# Helper: write a section block to Sheet 2 (Demographics)
#   rows_def: list of list(label, count_values_by_region, pct_denom_values_by_region|NA, number_format)
write_demo_section <- function(wb, sheet, start_row, section_label, regions, rows_def) {
  n_regions <- length(regions)
  total_cols <- 1 + 2 * n_regions

  # Section header row
  writeData(wb, sheet, section_label, startCol = 1, startRow = start_row)
  addStyle(wb, sheet, section_style(),
           rows = start_row, cols = 1:total_cols, gridExpand = TRUE)

  r <- start_row + 1
  for (rd in rows_def) {
    writeData(wb, sheet, rd$label, startCol = 1, startRow = r)
    for (j in seq_len(n_regions)) {
      c_count <- 2 + (j - 1) * 2
      c_pct   <- c_count + 1
      val   <- rd$counts[[j]]
      denom <- if (is.null(rd$denoms)) NA else rd$denoms[[j]]
      writeData(wb, sheet, val, startCol = c_count, startRow = r)
      if (!is.null(rd$fmt)) {
        addStyle(wb, sheet,
                 openxlsx::createStyle(numFmt = rd$fmt,
                                       border = "TopBottomLeftRight",
                                       borderColour = "#C00000"),
                 rows = r, cols = c_count)
      }
      if (!is.na(denom) && !is.na(val) && denom != 0) {
        writeData(wb, sheet, val / denom, startCol = c_pct, startRow = r)
        addStyle(wb, sheet,
                 openxlsx::createStyle(numFmt = "0%",
                                       border = "TopBottomLeftRight",
                                       borderColour = "#C00000"),
                 rows = r, cols = c_pct)
      } else {
        addStyle(wb, sheet, body_style(), rows = r, cols = c_pct)
      }
    }
    addStyle(wb, sheet, body_style(), rows = r, cols = 1)
    r <- r + 1
  }
  r  # next available row
}

write_demographics_sheet <- function(wb, sheet, regions, demo) {
  addWorksheet(wb, sheet)
  n_regions <- length(regions)
  total_cols <- 1 + 2 * n_regions

  setColWidths(wb, sheet, cols = 1, widths = 34)
  setColWidths(wb, sheet, cols = 2:total_cols, widths = 12)

  # Title
  writeData(wb, sheet, sprintf("Demographics (2021) - %s",
                               paste(names(regions), collapse = " vs ")),
            startCol = 1, startRow = 1)
  mergeCells(wb, sheet, cols = 1:total_cols, rows = 1)
  addStyle(wb, sheet, title_style(),
           rows = 1, cols = 1:total_cols, gridExpand = TRUE)

  # Region header row (spans two cols each)
  for (j in seq_along(regions)) {
    c_start <- 2 + (j - 1) * 2
    writeData(wb, sheet, names(regions)[j], startCol = c_start, startRow = 2)
    mergeCells(wb, sheet, cols = c_start:(c_start + 1), rows = 2)
  }
  addStyle(wb, sheet, header_style(), rows = 2, cols = 1:total_cols, gridExpand = TRUE)

  # Sub-header row: "Population / %"  etc — generic "Amount / %"
  writeData(wb, sheet, "Category", startCol = 1, startRow = 3)
  for (j in seq_along(regions)) {
    c_start <- 2 + (j - 1) * 2
    writeData(wb, sheet, "Amount", startCol = c_start,     startRow = 3)
    writeData(wb, sheet, "%",      startCol = c_start + 1, startRow = 3)
  }
  addStyle(wb, sheet, header_style(), rows = 3, cols = 1:total_cols, gridExpand = TRUE)

  # Pull per-region values
  v <- function(field) map(demo$region, \(rn) demo[[field]][demo$region == rn])
  vals <- function(field) map_dbl(seq_len(nrow(demo)), \(i) demo[[field]][i])

  pop_tot    <- vals("population")
  hh_size_tot<- vals("hh_size_total")
  bed_tot    <- vals("bed_total")
  period_tot <- vals("period_total")
  tenure_tot <- vals("tenure_total")

  r <- 4

  # Age
  r <- write_demo_section(wb, sheet, r, "Age Range", regions, list(
    list(label = "Total Counted",     counts = as.list(pop_tot),           denoms = as.list(pop_tot), fmt = "#,##0"),
    list(label = "0 to 14 years",     counts = as.list(vals("age_0_14")),  denoms = as.list(pop_tot), fmt = "#,##0"),
    list(label = "15 to 64 years",    counts = as.list(vals("age_15_64")), denoms = as.list(pop_tot), fmt = "#,##0"),
    list(label = "65 years and over", counts = as.list(vals("age_65_plus")),denoms= as.list(pop_tot), fmt = "#,##0"),
    list(label = "Median Age of Population", counts = as.list(vals("median_age")), denoms = NULL, fmt = "0.0")
  ))

  # Household Size (apartment unit-mix signal)
  r <- write_demo_section(wb, sheet, r, "Household Size", regions, list(
    list(label = "1 person",               counts = as.list(vals("hh_size_1")),     denoms = as.list(hh_size_tot), fmt = "#,##0"),
    list(label = "2 persons",              counts = as.list(vals("hh_size_2")),     denoms = as.list(hh_size_tot), fmt = "#,##0"),
    list(label = "3 persons",              counts = as.list(vals("hh_size_3")),     denoms = as.list(hh_size_tot), fmt = "#,##0"),
    list(label = "4 persons",              counts = as.list(vals("hh_size_4")),     denoms = as.list(hh_size_tot), fmt = "#,##0"),
    list(label = "5 or more persons",      counts = as.list(vals("hh_size_5plus")), denoms = as.list(hh_size_tot), fmt = "#,##0"),
    list(label = "Average household size", counts = as.list(vals("avg_hh_size")),   denoms = NULL,                 fmt = "0.0")
  ))

  # Occupied Dwellings by Bedrooms (existing supply side)
  r <- write_demo_section(wb, sheet, r, "Occupied Dwellings by Bedrooms (25% Sample)", regions, list(
    list(label = "No bedrooms (bachelor)", counts = as.list(vals("bed_0")),      denoms = as.list(bed_tot), fmt = "#,##0"),
    list(label = "1 bedroom",              counts = as.list(vals("bed_1")),      denoms = as.list(bed_tot), fmt = "#,##0"),
    list(label = "2 bedrooms",             counts = as.list(vals("bed_2")),      denoms = as.list(bed_tot), fmt = "#,##0"),
    list(label = "3 bedrooms",             counts = as.list(vals("bed_3")),      denoms = as.list(bed_tot), fmt = "#,##0"),
    list(label = "4 or more bedrooms",     counts = as.list(vals("bed_4plus")),  denoms = as.list(bed_tot), fmt = "#,##0")
  ))

  # Age of dwellings
  r <- write_demo_section(wb, sheet, r, "Age of Dwellings (25% Sample)", regions, list(
    list(label = "1960 or before", counts = as.list(vals("built_1960")),      denoms = as.list(period_tot), fmt = "#,##0"),
    list(label = "1961 to 1980",   counts = as.list(vals("built_1961_1980")), denoms = as.list(period_tot), fmt = "#,##0"),
    list(label = "1981 to 1990",   counts = as.list(vals("built_1981_1990")), denoms = as.list(period_tot), fmt = "#,##0"),
    list(label = "1991 to 2000",   counts = as.list(vals("built_1991_2000")), denoms = as.list(period_tot), fmt = "#,##0"),
    list(label = "2001 to 2005",   counts = as.list(vals("built_2001_2005")), denoms = as.list(period_tot), fmt = "#,##0"),
    list(label = "2006 to 2010",   counts = as.list(vals("built_2006_2010")), denoms = as.list(period_tot), fmt = "#,##0"),
    list(label = "2011 to 2015",   counts = as.list(vals("built_2011_2015")), denoms = as.list(period_tot), fmt = "#,##0"),
    list(label = "2016 to 2021",   counts = as.list(vals("built_2016_2021")), denoms = as.list(period_tot), fmt = "#,##0")
  ))

  # Tenure
  r <- write_demo_section(wb, sheet, r, "Dwelling Tenure (25% sample)", regions, list(
    list(label = "Owner",  counts = as.list(vals("owner")),  denoms = as.list(tenure_tot), fmt = "#,##0"),
    list(label = "Renter", counts = as.list(vals("renter")), denoms = as.list(tenure_tot), fmt = "#,##0")
  ))

  # Shelter
  r <- write_demo_section(wb, sheet, r, "Median Shelter (2020)", regions, list(
    list(label = "Median Value of Dwellings",  counts = as.list(vals("median_dwelling_val")), denoms = NULL, fmt = "$#,##0"),
    list(label = "Median Monthly Rental Cost", counts = as.list(vals("median_rent")),         denoms = NULL, fmt = "$#,##0")
  ))

  # Income
  r <- write_demo_section(wb, sheet, r, "Income (2020)", regions, list(
    list(label = "Median Individual Income", counts = as.list(vals("median_ind_income")), denoms = NULL, fmt = "$#,##0"),
    list(label = "Median Household Income",  counts = as.list(vals("median_hh_income")),  denoms = NULL, fmt = "$#,##0")
  ))

  # Shelter-cost stress (tenant households) — cancensus returns 0-100; scale to 0-1 for "0%"
  tenant_stir <- vals("tenant_stir_30") / 100
  r <- write_demo_section(wb, sheet, r, "Shelter-Cost Stress", regions, list(
    list(label = "Tenants spending 30%+ on shelter", counts = as.list(tenant_stir), denoms = NULL, fmt = "0%")
  ))
}

# ---- Top-level entry point -------------------------------------------------

generate_report <- function(regions, output_path) {
  stopifnot(is.list(regions), length(regions) >= 1, !is.null(names(regions)))
  first_name <- names(regions)[1]
  first_spec <- regions[[1]]

  message("Building Trends table for: ", first_name)
  trends <- build_trends(first_spec)

  message("Building Demographics table for: ",
          paste(names(regions), collapse = ", "))
  demo <- build_demographics(regions)

  build_workbook(regions, trends, demo, output_path)
}

# Build an .xlsx from already-computed trends & demographics data frames.
# Useful when the data was fetched elsewhere (e.g. cached in a Shiny reactive).
build_workbook <- function(regions, trends, demo, output_path) {
  first_name <- names(regions)[1]
  wb <- createWorkbook()
  write_trends_sheet(wb, "Trends", first_name, trends)
  write_demographics_sheet(wb, "Demographics", regions, demo)
  saveWorkbook(wb, output_path, overwrite = TRUE)
  message("Wrote ", output_path)
  invisible(output_path)
}
