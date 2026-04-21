# Census Report Generator

Generates a two-sheet Excel workbook of Statistics Canada census data for a
user-defined area, for drop-in use in appraisal reports.

- **Sheet 1 — Trends**: one region across four censuses (2006 / 2011 / 2016 / 2021).
  Population, % change, total occupied private dwellings, and occupied dwellings
  by structural type.
- **Sheet 2 — Demographics**: up to three regions side-by-side, 2021 census only.
  Age, household size, bedrooms, age of dwellings, tenure, median shelter cost,
  income, and tenant shelter-cost stress (STIR ≥ 30%).

Apartment-focused. Marital status is intentionally omitted; vacancy and market
rents are collected separately and are not part of this report.

## Files

| File                    | Purpose                                                        |
| ----------------------- | -------------------------------------------------------------- |
| `app.R`                 | **Shiny dashboard** — searchable region pickers, live tables & charts, one-click Excel download. Recommended entry point. |
| `run_report.R`          | Script entry point — edit the `regions` list and source. For batch / scripted use. |
| `census_report.R`       | Library — field catalogs, vector lookup, Excel writer. Shared by both entry points. |
| `Cancensus API Key.R`   | Local-only helper for setting the cancensus API key. Not committed. |
| `ReportCensusData.Rproj`| RStudio project file.                                          |

## Using the dashboard (recommended)

1. Open `ReportCensusData.Rproj` in RStudio.
2. Open `app.R`, click **Run App** (top-right of the editor).
3. The dashboard opens in your default browser.
4. Pick **Region 1** (drives Trends) and **Region 2** (comparison). Manitoba
   is always Region 3. Defaults are RM of Springfield + Winnipeg CMA.
5. Review the Trends and Demographics tabs — tables and charts update live.
6. Click **Download Excel**. The browser prompts where to save.

Tip: the first lookup for any region is slow (fetches four census vintages
from the API). Subsequent lookups for the same region are cached locally.

## First-time setup

Run once in R:

```r
install.packages(c("cancensus", "openxlsx", "dplyr", "tidyr", "purrr", "stringr"))
cancensus::set_cancensus_api_key("YOUR_KEY_FROM_CENSUSMAPPER_CA", install = TRUE)
cancensus::set_cancensus_cache_path("~/cancensus_cache", install = TRUE)
# Restart R so .Renviron is reloaded.
```

The API key is free from <https://censusmapper.ca/users/sign_up>. It's stored
in `~/.Renviron` (on this machine: `C:/Users/Jason/Documents/.Renviron`) and
never needs to be pasted into scripts or chats.

## Running the script (batch / scripted)

For one-off runs where you'd rather edit a file than use the dashboard:

1. Open `ReportCensusData.Rproj` in RStudio.
2. Open `run_report.R`, edit the `regions` list (see below).
3. `source("run_report.R")`.

Output lands in the working directory as
`Census_Report_<RegionName>_2021-YYYYMM.xlsx` (census vintage + month created).

## Defining regions

`regions` is a named list. Each entry's **name** is the column/title label; the
**body** is a geography spec — `list(<level> = "<id>")`:

```r
regions <- list(
  `Steinbach`    = list(CSD = "4602044"),
  `Winnipeg CMA` = list(CMA = "46602"),
  `Manitoba`     = list(PR  = "46")
)
```

- The **first** region drives Sheet 1 (Trends).
- **All** regions appear on Sheet 2 (Demographics).

### Custom study areas

To aggregate multiple sub-geographies into one region, supply a vector of IDs
or multiple levels — cancensus will sum them:

```r
regions <- list(
  `My study area` = list(CT = c("6020001.00", "6020002.00"),
                         DA = c("46020xxxx", "46020yyyy"))
)
```

### Finding geography IDs

After sourcing `census_report.R`, use the built-in helper from the R console:

```r
find_region("Winnipeg")                # all levels matching "Winnipeg"
find_region("Winnipeg", level = "CMA") # filter to one level
find_region("^Hanover$", level = "CSD")# anchor regex when name is ambiguous
```

For map-based picking of Census Tracts / DAs, use <https://censusmapper.ca>.

### Geography levels & ID conventions

| Level | Description                   | Example                   |
| ----- | ----------------------------- | ------------------------- |
| `PR`  | Province                      | `"46"` (Manitoba)         |
| `CMA` | Census Metropolitan Area      | `"46602"` (Winnipeg CMA)  |
| `CSD` | Census Subdivision            | `"4611040"` (Winnipeg CY), `"4602044"` (Steinbach) |
| `CT`  | Census Tract                  | `"6020001.00"`            |
| `DA`  | Dissemination Area            | 8-digit                   |

**Important:** CMA IDs are prefixed with the 2-digit province code. Winnipeg
CMA is `"46602"`, not `"602"`. The `find_region()` helper prints the correct
full ID in its `id` column — copy that value.

## Fields by sheet

### Sheet 1 — Trends (2006 → 2021)
- Population, Population % change
- Total Private Dwellings (occupied by usual residents)
- Occupied Dwellings by Structural Type: single-detached, apartment (<5 storeys),
  apartment (5+ storeys), semi-detached, row house, duplex, movable, other

### Sheet 2 — Demographics (2021 only, side-by-side)
- **Age Range** — 0-14, 15-64, 65+, median age
- **Household Size** — 1 / 2 / 3 / 4 / 5+ persons, average household size
- **Occupied Dwellings by Bedrooms** (25% sample) — 0 / 1 / 2 / 3 / 4+ BR
- **Age of Dwellings** (25% sample) — period-of-construction buckets 1960→2021
- **Dwelling Tenure** — owner, renter
- **Median Shelter (2020)** — median dwelling value, median monthly rental cost
- **Income (2020)** — median individual, median household
- **Shelter-Cost Stress** — % tenant households spending 30%+ on shelter

## Data sources

- **cancensus** R package (CensusMapper) wrapping the StatCan Census Profile API.
- Covers Census Programs CA06 (2006), CA11 (2011), CA16 (2016), CA21 (2021).
- 25%-sample data (long-form) for dwelling age, bedrooms, tenure, shelter cost,
  income, and STIR — rounded to nearest 5 per StatCan random-rounding rules.

## Troubleshooting

### "No vector found" or "Ambiguous" errors
The script looks up census vectors by label regex. If StatCan publishes a label
differently in a given vintage, the match fails with a helpful candidate list.
Use `verify_vectors()` in the R console:

```r
verify_vectors("CA21", DEMO_FIELDS)
verify_vectors("CA16", TRENDS_FIELDS)
```

Then adjust the regex in `TRENDS_FIELDS` / `DEMO_FIELDS` in `census_report.R`.

### Running from a shell (Rscript) instead of RStudio
On this machine, Rscript looks for `.Renviron` in `C:/Users/Jason/` but
cancensus writes the key to `C:/Users/Jason/Documents/.Renviron`. Prepend
the explicit path:

```bash
R_ENVIRON_USER="C:/Users/Jason/Documents/.Renviron" Rscript run_report.R
```

### "Permission denied" on save
The destination `.xlsx` is open in Excel. Close it and re-run.

### Empty values for a region
Likely a bad geography ID. Use `find_region(...)` to re-verify the ID. A typo
in a CSD/CMA code returns zero rows silently rather than erroring.

## Extending

The current report is tuned for **apartment / residential** work. Adding
sections for other property types would mean extending `DEMO_FIELDS` in
`census_report.R` and adding a corresponding `write_demo_section(...)` call in
`write_demographics_sheet()`:

- **Retail / commercial** — employment by industry (NAICS), daytime population
  ("Place of Work"), household income distribution (not just median),
  educational attainment.
- **Industrial** — labour force by industry, especially manufacturing and
  transportation/warehousing share.
- **Development land** — pair with external CMHC Starts & Completions data and
  Manitoba Bureau of Statistics projections (not in census).

Also worth pairing separately with **CMHC Rental Market Report** data
(vacancy, average rent by unit size) for apartment work — census doesn't
publish vacancy.
