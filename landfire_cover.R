# ============================================================
# Pull LANDFIRE land cover data for a watershed via the LFPS API
# and compute percent area by category.
#
# NOTE: LANDFIRE rebuilt LFPS in 2026. It is now a JSON REST API
# (not the old ArcGIS GPServer form), verified against the live
# service and current LF2024/LF2025 layer names as of July 2026.
#
# Requires: sf, terra, httr2, dplyr
# install.packages(c("sf","terra","httr2","dplyr"))
# ============================================================

library(sf)
library(terra)
library(httr2)
library(dplyr)

# ---- 0. SETTINGS ----------------------------------------------------------

boundary_path <- "/Users/megansears/Documents/Repos/fourmile/data/GIS/hrus_final.shp"
out_dir       <- "/Users/megansears/Documents/Repos/fourmile/data/landfire_download"
your_email    <- "searsmg1@gmail.com"   # required by the LFPS API

# Existing Vegetation Type (EVT) = LANDFIRE's "land cover" layer.
# LF2024_EVT covers all of CONUS and is confirmed current.
# LF2025_EVT is newer and does cover the SW geoArea (which includes CO),
# but its full-extent rollout is still in progress as of mid-2026 -- check
# https://lfps.usgs.gov/api/products for the latest list of layer names
# and which geoAreas/versions are available before switching.
layer_code <- "LF2024_EVT"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 1. Build area of interest (bounding box, WGS84) -----------------------

aoi <- st_read(boundary_path, quiet = TRUE)
aoi_wgs84 <- st_transform(aoi, 4326)
bb <- st_bbox(aoi_wgs84)
aoi_string <- paste(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])

# ---- 2. Submit job to LFPS --------------------------------------------------

submit <- request("https://lfps.usgs.gov/api/job/submit") |>
  req_method("POST") |>
  req_body_json(list(
    Email = your_email,
    Layer_List = layer_code,
    Area_of_Interest = aoi_string,
    Include_Layer_List_XML_File = FALSE
  )) |>
  req_perform() |>
  resp_body_json()

job_id <- submit$jobId
message("Submitted LFPS job: ", job_id, " (status: ", submit$status, ")")

# ---- 3. Poll until job finishes --------------------------------------------
# NOTE: the query parameter name must be capitalized "JobId" -- lowercase
# "jobId" returns a "Parameter JobId not found" error.

repeat {
  status <- request("https://lfps.usgs.gov/api/job/status") |>
    req_url_query(JobId = job_id) |>
    req_perform() |>
    resp_body_json()
  
  message("Job status: ", status$status,
          if (!is.null(status$queuePosition) && status$queuePosition > 0)
            paste0(" (queue position ", status$queuePosition, ")") else "")
  
  if (status$status == "Succeeded") break
  if (status$status %in% c("Failed", "Cancelled")) {
    msgs <- vapply(status$messages, function(m) m$description, character(1))
    stop("LFPS job failed: ", paste(msgs, collapse = "; "))
  }
  Sys.sleep(15)
}

# ---- 4. Download and unzip --------------------------------------------------
# outputFile stays live for 60 minutes after the job succeeds.

zip_path <- file.path(out_dir, "landfire.zip")
download.file(status$outputFile, destfile = zip_path, mode = "wb")
unzip(zip_path, exdir = out_dir)

tif_file <- list.files(out_dir, pattern = "\\.tif$", full.names = TRUE)[1]

# ---- 5. Clip raster to actual watershed polygon (not just the bbox) -------

r <- rast(tif_file)
aoi_proj <- st_transform(aoi, crs(r))
r_clip <- mask(crop(r, vect(aoi_proj)), vect(aoi_proj))

# ---- 6. Percent area by category -------------------------------------------

cell_area_m2 <- prod(res(r_clip))

pct_area <- freq(r_clip) |>
  as_tibble() |>
  mutate(
    area_ha = count * cell_area_m2 / 10000,
    pct_area = 100 * count / sum(count)
  ) |>
  rename(class = value) |>
  arrange(desc(pct_area))

print(pct_area)

write.csv(pct_area, file.path("./data/landcover_pct_area.csv"), row.names = FALSE)
