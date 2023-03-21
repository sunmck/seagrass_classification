# Load libraries
library(getSpatialData)
library(sp)
library(maptools)
library(raster)
library(sf)
library(lattice)
library(mapview)

getwd()
# Set wd if necessary
# setwd("path/to/wd")

##=====================
##  Global Variables  =
##=====================

# Write username for the Sentinel Copernicus Open Access Hub
# Requires registration
usercop <- "avinnus"

# Time ranges in which we want to classify seagrass cover
summer_22 <-  c("2022-08-20", "2022-08-31")
winter_23 <-  c("2023-02-20", "2023-03-10")
# List of time ranges
time_ranges <- list(summer_22, winter_23)
time_ranges_txt <- c("summer_22", "winter_23")

# Define further parameters to download the Sentinel-2 data
maxcloudcover <- 25
orbitnumber <- 8
# tile_id <- "32UMF"
# product_type <- "S2MSI1C"

# Bands we want to stack for further analysis, here RGB
bands <- c("B04","B03","B02")

# Range of water depth relevant for seagrass detection
minWaterDepth <- -6
maxWaterDepth <- 2

##===============
##  Functions   =
##===============

# Returns RasterStack of multiple RasterLayers defined by their band names of a downloaded Sentinel-2 .SAFE directory
# Parameters:
#   safe_dir: .SAFE directory of downloaded Sentinel-2 imagery
#   bands: vector of bands we want to stack
get_bands <- function(safe_dir, bands) {
  # empty list of .jp2 files
  b <- list()

  for (band in bands) {
    print(band)
    file_list = list.files(path = safe_dir,
                           pattern=band,
                           recursive=TRUE)
    for (file in file_list){
      if (grepl("IMG_DATA", file)) {
        b <- append(b, paste(safe_dir,"/",file,sep=""))
        print(b)
      }
    }
  }
  return(stack(b))
}

##=====================
##  Search for data   =
##=====================

# Log in to Copernicus Open Access Hub
login_CopHub(username = usercop)

# Load AOI
# Here, the North Frisian Wadden Sea
aoi_sf <- readShapePoly("./data/aoi/aoi.shp", proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Define AOI as search area for Sentinel-2 imagery
aoi <- set_aoi(aoi_sf)
view_aoi()

# Search for one image per time range we want to analyse and save them in a df
all_records <- data.frame()

for (time_range in time_ranges) {
  records <- getSentinel_records(time_range, "sentinel-2")

  # Filter data by cloud cover, relative orbit number and availability
  records <- check_availability(records)
  records <- records[records$cloudcov <= maxcloudcover & records$relativeorbit_number == orbitnumber & records$download_available == T,]

  # Sort images by cloud coverage, choose only best image
  records <- records[order(records$cloudcov),]
  records <- records[1,]

  all_records <- rbind(all_records, records)
}

##=======================
##  Download the data   =
##=======================

# Download previews and plot them
all_records <- get_previews(all_records, dir_out="./data/")
plot_previews(all_records[1,]) # plot only first preview

# Download the Sentinel-2 imagery and save it in the output directory
all_records <- get_data(all_records, dir_out="./data/")

# Unzip downloaded files
path = paste(getwd(),"/data/sentinel-2/",sep="")
zipFiles = list.files(path, pattern="zip")

for (zipFile in zipFiles) {
  unzip(paste(path, zipFile, sep=""),
        files = NULL,
        list = F,
        overwrite = T,
        junkpaths = F,
        exdir = path,
        unzip = "internal",
        setTimes = F)
}

##========================
##  Data preprocessing   =
##========================

# Stack RBG bands and rename the Layers
s2_stacks <- list()
safe_dir_df <- data.frame()

for (i in 1:nrow(all_records)){
  # Directory where Sentinel-2 .SAFE files are located
  safe_dir <- paste(getwd(), "/data/sentinel-2/", all_records[i,]$record_id,".SAFE", sep = "")
  stack <- get_bands(safe_dir, bands)
  names(stack) <- bands

  # Save RGB stacks
  s2_stacks <- append(s2_stacks, stack)
  safe_dir_df <- rbind(safe_dir_df, safe_dir)
}

# Write .SAFE directories to .csv file for later usage
write.table(safe_dir_df, file = "./data/safe_dir.csv", row.names = F)

# Visualize exemplary Sentinel-2 image as RGB
first_img <- s2_stacks[[1]]
viewRGB(first_img[[c(1,2,3)]], r=3, g=2, b=1)


# To reduce computational effort, only select water depth relevant for seagrass as it only grows in shallow waters
# Ocean Bayometric data provided by GEBCO (https://www.gebco.net/data_and_products/gridded_bathymetry_data/#global)
depth <- raster("./data/waterdepth/gebco_2022_n55.3_s53.8_w7.8_e9.3.tif")
depthmask_poly <- rasterToPolygons(depth, fun = function(x){x>minWaterDepth & x<maxWaterDepth})
depthmask_spdf <- SpatialPolygonsDataFrame(depthmask_poly, data = data.frame(ID = 1:length(depthmask_poly)))
depthmask_spdf <- unionSpatialPolygons(depthmask_spdf, rep(1, length(depthmask_spdf)))
# if unionSpatialPolygons gives error "isTRUE(gpclibPermitStatus()) ist nicht TRUE" run first:
# install.packages("gpclib", type="source")
# library(gpclib)
# gpclibPermit()
depthmask_sf <- st_as_sf(depthmask_spdf)
depthmask_sf <- st_transform(depthmask_sf,crs = proj4string(s2_stacks[[1]]))

# Crop downloaded Sentinel-2 images to relevant water depth
s2_stacks_cropped <- list()
# Create new folder, if not already existing
dir.create("./data/cropped/", showWarnings = FALSE)

for (i in 1:length(s2_stacks)){
  cropped <- raster::mask(s2_stacks[[i]], depthmask_sf)
  cropped <- raster::crop(cropped, depthmask_sf)
  # writeRaster(cropped, filename = paste0("./data/cropped/",time_ranges_txt[[i]],".grd
  # ", sep = ""), overwrite=T)
  s2_stacks_cropped <- append(s2_stacks_cropped, cropped)
}

# Optionally view cropped Sentinel-2 images
mapview::mapview(s2_stacks_cropped[[1]]) # first image