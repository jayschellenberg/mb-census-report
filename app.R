# app.R — Shiny dashboard for the Census Report Generator.
#
# Run:
#   - In RStudio: open this file and click "Run App" (top-right of editor).
#   - From R console: shiny::runApp("path/to/CensusData")
#
# The app opens in your default browser. Pick Region 1 (drives the Trends
# sheet) and Region 2 (comparison). Manitoba is always Region 3. The tables
# and charts update live; click "Download Excel" to save the xlsx.

suppressPackageStartupMessages({
  library(shiny)
  library(cancensus)
})

source("census_report.R")

# ---- Hosted-environment guards --------------------------------------------
# When deployed (e.g. shinyapps.io) there's no .Renviron and no persistent
# cache. Set a temp cache path if the user's one isn't mounted, and fail
# fast with a friendly message if the API key is missing.
if (!nzchar(Sys.getenv("CM_CACHE_PATH")) ||
    !dir.exists(Sys.getenv("CM_CACHE_PATH"))) {
  Sys.setenv(CM_CACHE_PATH = tempfile("cancensus_cache_"))
  dir.create(Sys.getenv("CM_CACHE_PATH"), recursive = TRUE, showWarnings = FALSE)
}
if (!nzchar(Sys.getenv("CM_API_KEY"))) {
  stop("CM_API_KEY environment variable not set. ",
       "Locally: run cancensus::set_cancensus_api_key('<key>', install = TRUE). ",
       "On shinyapps.io: set it via rsconnect::deployApp(envVars = 'CM_API_KEY') ",
       "or in the app's Settings → Variables panel.", call. = FALSE)
}

# ---- Region catalog (pre-loaded once at app start) ------------------------
#
# Scoped to Manitoba only — Manitoba PR itself plus any geography whose
# PR_UID == "46" (Winnipeg CMA, MB census divisions, MB CSDs).

.raw_regions <- list_census_regions("CA21", use_cache = TRUE, quiet = TRUE)
all_regions  <- .raw_regions[
  .raw_regions$region == "46" |
  (!is.na(.raw_regions$PR_UID) & .raw_regions$PR_UID == "46"), ]
all_regions$pop <- suppressWarnings(as.integer(all_regions$pop))
all_regions$label <- ifelse(
  is.na(all_regions$pop),
  sprintf("%s  [%s]",         all_regions$name, all_regions$level),
  sprintf("%s  [%s, pop %s]", all_regions$name, all_regions$level,
          format(all_regions$pop, big.mark = ",", scientific = FALSE))
)

# Only levels that actually appear in Manitoba's catalog
AVAILABLE_LEVELS <- sort(unique(all_regions$level))   # PR, CMA, CD, CSD
DEFAULT_LEVELS   <- intersect(c("CSD", "CMA", "CD"), AVAILABLE_LEVELS)
DEFAULT_R1       <- "4612047"   # RM of Springfield
DEFAULT_R2       <- "46602"     # Winnipeg CMA

build_choices <- function(levels_included) {
  df <- all_regions[all_regions$level %in% levels_included, ]
  df <- df[order(match(df$level, c("PR","CMA","CD","CSD")),
                 -df$pop, na.last = TRUE), ]
  setNames(df$region, df$label)
}

# Names that appear at more than one level in Manitoba (e.g. Winnipeg = CSD + CMA).
# Used to append "(LEVEL)" only when needed, so Region 1 and Region 2 never share
# a header on the Demographics sheet.
.ambiguous_names <- names(which(table(all_regions$name) > 1))

display_name <- function(row) {
  if (length(row$name) == 1 && row$name %in% .ambiguous_names) {
    sprintf("%s (%s)", row$name, row$level)
  } else {
    as.character(row$name)
  }
}

# ---- Formatting helpers ---------------------------------------------------

fmt_int <- function(x) ifelse(is.na(x), "", format(round(x), big.mark = ","))
fmt_pct <- function(x) ifelse(is.na(x), "", sprintf("%.0f%%", 100 * x))
fmt_usd <- function(x) ifelse(is.na(x), "", sprintf("$%s", format(round(x), big.mark = ",")))

# Build a display data.frame for the Trends sheet
display_trends <- function(trends) {
  yrs <- trends$year
  rows <- list(
    c("Population",                 fmt_int(trends$population)),
    c("Population % Change",        c("", fmt_pct(trends$pop_pct_change[-1]))),
    c("Total Private Dwellings*",   fmt_int(trends$households)),
    c("",                           rep("", length(yrs))),
    c("Single-Detached House",      fmt_int(trends$single_detached)),
    c("Apartment (<5 Storeys)",     fmt_int(trends$apt_lt5)),
    c("Apartment (5+ Storeys)",     fmt_int(trends$apt_ge5)),
    c("Semi-Detached House",        fmt_int(trends$semi_detached)),
    c("Row House",                  fmt_int(trends$row_house)),
    c("Apt or Flat in a Duplex",    fmt_int(trends$apt_duplex)),
    c("Movable Dwelling",           fmt_int(trends$movable)),
    c("Other",                      fmt_int(trends$other_attached))
  )
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(df) <- c("Metric", as.character(yrs))
  df
}

# Build a display data.frame for the Demographics sheet
display_demographics <- function(regions, demo) {
  rn <- names(regions)
  get <- function(field) setNames(
    vapply(rn, \(r) demo[[field]][demo$region == r], numeric(1)), rn)

  pop_tot <- get("population"); hh_tot  <- get("hh_size_total")
  bed_tot <- get("bed_total");  per_tot <- get("period_total")
  ten_tot <- get("tenure_total")

  section <- function(label) c(Category = label, setNames(rep("", 2 * length(rn)),
                                                          outer(rn, c("Amt","%"), paste, sep="_")))
  make_row <- function(label, vals, denoms = NULL, fmt = fmt_int) {
    v <- fmt(vals)
    p <- if (is.null(denoms)) rep("", length(rn)) else fmt_pct(vals / denoms)
    # interleave: Amt, %, Amt, %, ...
    out <- character(2 * length(rn))
    out[seq(1, 2 * length(rn), 2)] <- v
    out[seq(2, 2 * length(rn), 2)] <- p
    c(Category = label, setNames(out, NULL))
  }
  sec_hdr <- function(label) {
    out <- c(Category = label, rep("", 2 * length(rn)))
    out
  }

  rows <- list(
    sec_hdr("— Age Range —"),
    make_row("Total Counted",             pop_tot,              pop_tot),
    make_row("0 to 14 years",             get("age_0_14"),      pop_tot),
    make_row("15 to 64 years",            get("age_15_64"),     pop_tot),
    make_row("65 years and over",         get("age_65_plus"),   pop_tot),
    make_row("Median Age",                get("median_age"),    NULL,
             function(x) ifelse(is.na(x), "", sprintf("%.1f", x))),
    sec_hdr("— Household Size —"),
    make_row("1 person",                  get("hh_size_1"),     hh_tot),
    make_row("2 persons",                 get("hh_size_2"),     hh_tot),
    make_row("3 persons",                 get("hh_size_3"),     hh_tot),
    make_row("4 persons",                 get("hh_size_4"),     hh_tot),
    make_row("5 or more persons",         get("hh_size_5plus"), hh_tot),
    make_row("Average household size",    get("avg_hh_size"),   NULL,
             function(x) ifelse(is.na(x), "", sprintf("%.1f", x))),
    sec_hdr("— Occupied Dwellings by Bedrooms (25% Sample) —"),
    make_row("No bedrooms (bachelor)",    get("bed_0"),         bed_tot),
    make_row("1 bedroom",                 get("bed_1"),         bed_tot),
    make_row("2 bedrooms",                get("bed_2"),         bed_tot),
    make_row("3 bedrooms",                get("bed_3"),         bed_tot),
    make_row("4 or more bedrooms",        get("bed_4plus"),     bed_tot),
    sec_hdr("— Age of Dwellings (25% Sample) —"),
    make_row("1960 or before",            get("built_1960"),      per_tot),
    make_row("1961 to 1980",              get("built_1961_1980"), per_tot),
    make_row("1981 to 1990",              get("built_1981_1990"), per_tot),
    make_row("1991 to 2000",              get("built_1991_2000"), per_tot),
    make_row("2001 to 2005",              get("built_2001_2005"), per_tot),
    make_row("2006 to 2010",              get("built_2006_2010"), per_tot),
    make_row("2011 to 2015",              get("built_2011_2015"), per_tot),
    make_row("2016 to 2021",              get("built_2016_2021"), per_tot),
    sec_hdr("— Dwelling Tenure (25% Sample) —"),
    make_row("Owner",                     get("owner"),  ten_tot),
    make_row("Renter",                    get("renter"), ten_tot),
    sec_hdr("— Median Shelter (2020) —"),
    make_row("Median Value of Dwellings", get("median_dwelling_val"), NULL, fmt_usd),
    make_row("Median Monthly Rental Cost",get("median_rent"),         NULL, fmt_usd),
    sec_hdr("— Income (2020) —"),
    make_row("Median Individual Income",  get("median_ind_income"), NULL, fmt_usd),
    make_row("Median Household Income",   get("median_hh_income"),  NULL, fmt_usd),
    sec_hdr("— Shelter-Cost Stress —"),
    make_row("Tenants spending 30%+ on shelter",
             get("tenant_stir_30") / 100, NULL, fmt_pct)
  )

  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  # Build readable column headers: each region gets two columns (Count, %)
  hdrs <- c("Category",
            as.vector(rbind(rn, rep("%", length(rn)))))
  names(df) <- hdrs
  df
}

# ---- UI -------------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: -apple-system, Segoe UI, Helvetica, Arial, sans-serif; }
    h3   { margin-top: 0; }
    .small-note { color:#666; font-size:0.9em; }
    table.census-table td:first-child { font-weight: 500; }
    table.census-table td, table.census-table th { padding: 4px 10px; }
    table.census-table th { background: #f6f2f2; border-bottom: 2px solid #c00000; }
    .dl-wrap   { margin-top: 12px; }
  "))),
  titlePanel("Manitoba Census Report — Dashboard"),
  div(class = "small-note", style = "margin: -8px 0 12px 0;",
      "Searches are limited to Manitoba geographies (264 municipalities, census divisions, and Winnipeg CMA)."),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      checkboxGroupInput(
        "levels", "Manitoba geography levels:",
        choices  = AVAILABLE_LEVELS,
        selected = DEFAULT_LEVELS, inline = TRUE
      ),
      div(class = "small-note",
          "CSD = municipalities / RMs.  CD = census divisions.  CMA = Winnipeg."),
      tags$hr(),
      selectizeInput(
        "region1", "Region 1 (Trends + Demographics):",
        choices = NULL,
        options = list(placeholder = "Type a place name…", maxOptions = 200)
      ),
      selectizeInput(
        "region2", "Region 2 (Demographics comparison):",
        choices = NULL,
        options = list(placeholder = "Type a place name…", maxOptions = 200)
      ),
      div(class = "small-note", "Region 3 is always Manitoba (PR 46)."),
      div(class = "dl-wrap",
          downloadButton("download", "Download Excel",
                         class = "btn-primary"))
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel("Trends",
                 h3(textOutput("trends_title")),
                 plotOutput("trends_plot", height = "280px"),
                 tags$div(tableOutput("trends_table"), class = "census-table")),
        tabPanel("Demographics",
                 h3(textOutput("demo_title")),
                 plotOutput("demo_plot", height = "320px"),
                 tags$div(tableOutput("demo_table"), class = "census-table")),
        tabPanel("Selection",
                 h3("Current selection"),
                 tableOutput("preview"),
                 htmlOutput("tips"))
      )
    )
  )
)

# ---- Server ---------------------------------------------------------------

server <- function(input, output, session) {

  # Only rebuild dropdowns when the level checkboxes change — reading the
  # region inputs here (without isolate) would create a feedback loop because
  # updateSelectizeInput writes back to them.
  observeEvent(input$levels, {
    ch <- build_choices(input$levels)
    cur1 <- isolate(input$region1) %||% ""
    cur2 <- isolate(input$region2) %||% ""
    sel1 <- if (nzchar(cur1) && cur1 %in% ch) cur1
            else if (DEFAULT_R1 %in% ch) DEFAULT_R1 else ""
    sel2 <- if (nzchar(cur2) && cur2 %in% ch) cur2
            else if (DEFAULT_R2 %in% ch) DEFAULT_R2 else ""
    updateSelectizeInput(session, "region1", choices = ch,
                        selected = sel1, server = TRUE)
    updateSelectizeInput(session, "region2", choices = ch,
                        selected = sel2, server = TRUE)
  }, ignoreNULL = FALSE)

  selection <- reactive({
    req(nzchar(input$region1 %||% ""), nzchar(input$region2 %||% ""))
    r1 <- all_regions[all_regions$region == input$region1, ]
    r2 <- all_regions[all_regions$region == input$region2, ]
    if (nrow(r1) == 0 || nrow(r2) == 0) return(NULL)
    list(r1 = r1, r2 = r2)
  })

  regions_list <- reactive({
    s <- selection(); req(s)
    regs <- list()
    regs[[display_name(s$r1)]] <- setNames(list(s$r1$region), s$r1$level)
    regs[[display_name(s$r2)]] <- setNames(list(s$r2$region), s$r2$level)
    regs[["Manitoba"]] <- list(PR = "46")
    regs
  })

  # Expensive API calls — cached per selection
  artifacts <- reactive({
    regs <- regions_list(); req(regs)
    withProgress(message = "Fetching census data…", value = 0.1, {
      incProgress(0.2, detail = "Trends (2006–2021)")
      tr <- build_trends(regs[[1]])
      incProgress(0.6, detail = "Demographics (2021)")
      dm <- build_demographics(regs)
      incProgress(1.0, detail = "Done")
    })
    list(regions = regs, trends = tr, demo = dm)
  })

  # ---- Selection tab ----
  output$preview <- renderTable({
    s <- selection(); if (is.null(s)) return(NULL)
    data.frame(
      Slot = c("Region 1 (Trends + Demographics)",
               "Region 2 (Demographics)",
               "Region 3 (fixed)"),
      Name = c(display_name(s$r1), display_name(s$r2), "Manitoba"),
      Level = c(s$r1$level, s$r2$level, "PR"),
      ID    = c(s$r1$region, s$r2$region, "46"),
      Population = c(format(s$r1$pop, big.mark = ","),
                     format(s$r2$pop, big.mark = ","),
                     "1,342,153"),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, width = "100%")

  output$tips <- renderUI({
    tagList(
      tags$p(class = "small-note",
             "Output filename: ",
             tags$code("Census_Report_<Region1>_2021-YYYYMM.xlsx"))
    )
  })

  # ---- Trends tab ----
  output$trends_title <- renderText({
    sprintf("Population & Dwelling Trends — %s", names(regions_list())[1])
  })

  output$trends_plot <- renderPlot({
    a <- artifacts(); req(a)
    op <- par(mar = c(4, 4.5, 1, 1)); on.exit(par(op))
    plot(a$trends$year, a$trends$population, type = "o", pch = 16, lwd = 2,
         col = "#c00000", xlab = "Census year", ylab = "Population",
         xaxt = "n", las = 1)
    axis(1, at = a$trends$year)
    text(a$trends$year, a$trends$population,
         labels = format(a$trends$population, big.mark = ","),
         pos = 3, cex = 0.9)
    grid(nx = NA, ny = NULL, lty = 3, col = "grey80")
  })

  output$trends_table <- renderTable({
    a <- artifacts(); req(a)
    display_trends(a$trends)
  }, striped = TRUE, width = "100%", align = "lrrrr")

  # ---- Demographics tab ----
  output$demo_title <- renderText({
    sprintf("Demographics (2021) — %s",
            paste(names(regions_list()), collapse = " vs "))
  })

  output$demo_plot <- renderPlot({
    a <- artifacts(); req(a)
    rn <- names(a$regions)
    hh_tot <- vapply(rn, \(r) a$demo$hh_size_total[a$demo$region == r], numeric(1))
    mat <- rbind(
      `1 person`  = vapply(rn, \(r) a$demo$hh_size_1[a$demo$region == r], numeric(1)),
      `2 persons` = vapply(rn, \(r) a$demo$hh_size_2[a$demo$region == r], numeric(1)),
      `3 persons` = vapply(rn, \(r) a$demo$hh_size_3[a$demo$region == r], numeric(1)),
      `4 persons` = vapply(rn, \(r) a$demo$hh_size_4[a$demo$region == r], numeric(1)),
      `5+ persons`= vapply(rn, \(r) a$demo$hh_size_5plus[a$demo$region == r], numeric(1))
    )
    mat <- sweep(mat, 2, hh_tot, "/") * 100
    op <- par(mar = c(4, 4.5, 2.5, 8), xpd = NA); on.exit(par(op))
    cols <- c("#c00000","#e06666","#f2a7a7","#8a3a3a","#5d1e1e")
    barplot(mat, beside = TRUE, col = cols,
            ylab = "% of households", las = 1,
            main = "Household size distribution",
            ylim = c(0, max(mat) * 1.15))
    legend("topright", inset = c(-0.22, 0), legend = rownames(mat),
           fill = cols, bty = "n", cex = 0.9)
  })

  output$demo_table <- renderTable({
    a <- artifacts(); req(a)
    display_demographics(a$regions, a$demo)
  }, striped = TRUE, width = "100%", sanitize.text.function = identity)

  # ---- Download ----
  output$download <- downloadHandler(
    filename = function() {
      s <- selection(); req(s)
      sprintf("Census_Report_%s_2021-%s.xlsx",
              gsub("[^A-Za-z0-9]+", "_", display_name(s$r1)),
              format(Sys.Date(), "%Y%m"))
    },
    content = function(file) {
      a <- artifacts(); req(a)
      withProgress(message = "Writing Excel…", value = 0.5, {
        build_workbook(a$regions, a$trends, a$demo, file)
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
}

shinyApp(ui, server)
