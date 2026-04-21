# scripts/build_wpg_lookup.R
# One-time: build a lookup table mapping StatCan Dissemination Areas (DAs)
# to City of Winnipeg Neighbourhood / Cluster / Community Area.
#
# Run once after any City of Winnipeg boundary update. Writes
# `data/wpg_geography_lookup.csv` into the repo.
#
# Input: D:/Dropbox/Appraisal/RProjects/BaseFiles/WpgNeighbourhoods.geojson
# Output: data/wpg_geography_lookup.csv with columns:
#   DA_UID, Neighbourhood, Cluster, CommunityArea

suppressPackageStartupMessages({
  library(sf)
  library(cancensus)
  library(dplyr)
})

# Set cache / env
if (!nzchar(Sys.getenv("CM_CACHE_PATH"))) {
  Sys.setenv(CM_CACHE_PATH = tempfile("cancensus_cache_"))
  dir.create(Sys.getenv("CM_CACHE_PATH"), recursive = TRUE, showWarnings = FALSE)
}

WPG_NBHD_FILE <- "D:/Dropbox/Appraisal/RProjects/BaseFiles/WpgNeighbourhoods.geojson"
OUT_DIR       <- "data"
OUT_FILE      <- file.path(OUT_DIR, "wpg_geography_lookup.csv")

# Cluster → Community Area hardcoded map.
# Winnipeg's 12 Community Areas, each containing 1–4 clusters.
# Source: City of Winnipeg census geography hierarchy.
cluster_to_ca <- c(
  "Assiniboine South"       = "Assiniboine South",
  "Downtown East"           = "Downtown",
  "Downtown West"           = "Downtown",
  "Fort Garry North"        = "Fort Garry",
  "Fort Garry South"        = "Fort Garry",
  "Inkster East"            = "Inkster",
  "Inkster West"            = "Inkster",
  "Point Douglas North"     = "Point Douglas",
  "Point Douglas South"     = "Point Douglas",
  "River East East"         = "River East",
  "River East North"        = "River East",
  "River East South"        = "River East",
  "River East West"         = "River East",
  "River Heights East"      = "River Heights",
  "River Heights West"      = "River Heights",
  "Fort Rouge-East Fort Garry" = "River Heights",
  "Seven Oaks East"         = "Seven Oaks",
  "Seven Oaks North"        = "Seven Oaks",
  "Seven Oaks West"         = "Seven Oaks",
  "St. Boniface East"       = "St. Boniface",
  "St. Boniface West"       = "St. Boniface",
  "St. James-Assiniboia East"= "St. James-Assiniboia",
  "St. James-Assiniboia West"= "St. James-Assiniboia",
  "St. Vital North"         = "St. Vital",
  "St. Vital South"         = "St. Vital",
  "Transcona"               = "Transcona"
)

# ---- Load City boundaries -----------------------------------------------

cat("Reading Winnipeg neighbourhoods…\n")
nb <- st_read(WPG_NBHD_FILE, quiet = TRUE)
cat("Neighbourhoods:", nrow(nb), "\n")
cat("Unique clusters:\n")
print(sort(unique(nb$Cluster)))

# Verify every cluster is in our cluster_to_ca map
unmapped <- setdiff(unique(nb$Cluster), names(cluster_to_ca))
if (length(unmapped) > 0) {
  cat("\nWARN — clusters missing from cluster_to_ca map (add them):\n")
  print(unmapped)
  stop("Update cluster_to_ca and re-run.", call. = FALSE)
}
nb$CommunityArea <- cluster_to_ca[nb$Cluster]

# ---- Pull StatCan DAs within Winnipeg CSD -------------------------------
# CSD 4611040 = City of Winnipeg.

cat("\nFetching Dissemination Area boundaries (CA21, Winnipeg CSD)…\n")
da <- get_census(dataset    = "CA21",
                 regions    = list(CSD = "4611040"),
                 level      = "DA",
                 geo_format = "sf",
                 use_cache  = TRUE,
                 quiet      = TRUE)
cat("DAs in Winnipeg CSD:", nrow(da), "\n")

# ---- Spatial join: assign each DA centroid to a neighbourhood -----------

# Match CRS
nb_wgs <- st_transform(nb, st_crs(da))

# Use centroids so each DA gets a single parent (DAs that straddle a boundary
# are small and the centroid rule introduces negligible error at this scale).
da_cen <- st_centroid(st_geometry(da))
suppressWarnings({
  joined <- st_join(
    st_sf(GeoUID = da$GeoUID, geometry = da_cen),
    nb_wgs[, c("Name","Cluster","CommunityArea")],
    join = st_within
  )
})

# Tidy to a data frame (include Population so the app can show it in the dropdown)
lookup <- data.frame(
  DA_UID        = joined$GeoUID,
  Population    = da$Population[match(joined$GeoUID, da$GeoUID)],
  Neighbourhood = as.character(joined$Name),
  Cluster       = as.character(joined$Cluster),
  CommunityArea = as.character(joined$CommunityArea),
  stringsAsFactors = FALSE
)

# Report coverage
n_unmatched <- sum(is.na(lookup$Neighbourhood))
cat("DAs matched to a neighbourhood:", nrow(lookup) - n_unmatched, "/", nrow(lookup), "\n")
if (n_unmatched > 0) {
  cat("WARN —", n_unmatched, "DAs fell outside every neighbourhood polygon.\n")
  cat("These will be omitted from cluster/CA aggregations.\n")
}

# ---- Write lookup -------------------------------------------------------

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
write.csv(lookup, OUT_FILE, row.names = FALSE)
cat("\nWrote", OUT_FILE, "—", nrow(lookup), "rows\n")

# Sanity check
cat("\nDA counts per Community Area:\n")
print(table(lookup$CommunityArea, useNA = "ifany"))
