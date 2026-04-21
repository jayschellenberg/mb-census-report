# run_report.R
# Edit the `regions` list below, then source this file to produce the Excel.
#
#   source("run_report.R")
#
# Region spec format — a named list per region. Each region is itself a list
# of geography_level -> character vector of Census geography IDs. cancensus
# will sum the components into a single aggregated row.
#
# Geography levels (most common):
#   PR  = Province              (e.g. "46" for Manitoba)
#   CMA = Census Metro Area     (e.g. "46602" for Winnipeg CMA — PR prefix + 3-digit CMA)
#   CSD = Census Subdivision    (e.g. "4611040" for City of Winnipeg,
#                                      "4602042" for Steinbach)
#   CT  = Census Tract          (7-digit CMA + decimal)
#   DA  = Dissemination Area    (8-digit)
#
# Find IDs with the built-in helper (run after source()ing below):
#   find_region("Winnipeg")               # all levels matching "Winnipeg"
#   find_region("Winnipeg", level = "CMA")
#   find_region("Steinbach", level = "CSD")
#   find_region("^Hanover$", level = "CSD")   # exact name match via regex
# Returns a table of: level | id | name | pop — copy the `id` into your spec.
#
# Example (name = Sheet/header label, body = geography spec):
#
#   regions <- list(
#     `Steinbach`        = list(CSD = "4602042"),
#     `City of Winnipeg` = list(CSD = "4611040"),
#     `Manitoba`         = list(PR  = "46")
#   )
#
# Combine geography levels to build a custom study area; cancensus unions them:
#
#   regions <- list(
#     `Niverville + nearby RM` = list(CSD = c("4602050","4602029")),
#     `City of Winnipeg`       = list(CSD = "4611040"),
#     `Manitoba`               = list(PR  = "46")
#   )
#
# The FIRST region in the list is used for the Trends sheet (one region,
# four censuses). All regions appear on the Demographics sheet (2021 only).

# Set working directory to this file's folder.
# Works both in RStudio (via rstudioapi) and under Rscript (cwd set by caller).
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  tryCatch(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)),
           error = function(e) NULL)
}
source("census_report.R")

# ---- Edit below for each report -------------------------------------------

regions <- list(
  `RM of Springfield` = list(CSD = "4612047"),
  `Winnipeg CMA`      = list(CMA = "46602"),
  `Manitoba`          = list(PR  = "46")
)

output_file <- sprintf("Census_Report_%s_2021-%s.xlsx",
                       gsub("[^A-Za-z0-9]+", "_", names(regions)[1]),
                       format(Sys.Date(), "%Y%m"))

# ---- Run ------------------------------------------------------------------

generate_report(regions, output_file)

# ---- One-time troubleshooting helpers -------------------------------------
#
# If generate_report() errors with "No vector found" or "Ambiguous",
# run these to see what cancensus actually publishes:
#
#   verify_vectors("CA21", DEMO_FIELDS)
#   verify_vectors("CA06", TRENDS_FIELDS)
#   verify_vectors("CA11", TRENDS_FIELDS)
#   verify_vectors("CA16", TRENDS_FIELDS)
#   verify_vectors("CA21", TRENDS_FIELDS)
#
# Then adjust the regex in TRENDS_FIELDS / DEMO_FIELDS inside census_report.R
# to match the published label for that census year.
