# Load libraries
library(ggplot2)
library(terra)
library(raster)
library(sf)
library(dplyr)

# Load classifications if necessary
classification_summer <- raster("./data/classification/classification_summer.grd")
classification_winter <- raster("./data/classification/classification_winter.grd")

##=============
##  Results   =
##=============

# View classification results with overlying groundtruth shapefile
seagrass_gt <- st_read("./data/groundtruth/2021_seagrass_UTM.shp")
cols_class <- c("green", "darkgreen","sandybrown")
mapview::mapview(seagrass_gt) +
  mapview::mapview(classification_summer, col.regions = cols_class, trim = T) +
  mapview::mapview(classification_winter, col.regions = cols_class, trim = T)

##======================
##    Seagrass Area    =
##======================

# Extract values from summer and winter classification and save them in a vector
vcs <- terra::values(classification_summer)
vcw <- terra::values(classification_winter)

# Count pixels of each class
area_summer <- table(vcs)
area_summer_df <- as.data.frame(area_summer)
area_winter <- table(vcw)
area_winter_df <- as.data.frame(area_winter)

# Calculate total area of seagrass
area_summer_df$area_km2 <- ((area_summer_df$Freq*100)/1000000) # resolution of 10x10m
area_winter_df$area_km2 <- ((area_winter_df$Freq*100)/1000000)

# Adjust data frame
area_summer_df$season <- "summer"
area_winter_df$season <- "winter"

# Rename class column
names(area_summer_df)[names(area_summer_df) == "vcs"] <- "class_id"
names(area_winter_df)[names(area_winter_df) == "vcw"] <- "class_id"

# Merge to single data frame
area_df <- data.frame()
area_df <- rbind(area_summer_df, area_winter_df)

# Choose only seagrass classes
area_df$class_id <- as.numeric(as.character(area_df$class_id))
area_df <- subset(area_df, area_df$class_id <= 2)
area_df$class_id <- as.factor(area_df$class_id)

# Add column with class name
area_df$seagrass_coverage <- ifelse(area_df$class_id == 1, "20-60%", ifelse(area_df$class_id == 2, ">60%", "NA"))

# Plot seagrass area
ggplot(area_df, aes(fill=seagrass_coverage, y=area_km2, x=season)) +
  geom_bar(position='stack', stat='identity') +
  labs(x=element_blank(), y="area [km2]", title = "Area of Seagrass in the North Frisian Wadden Sea") +
  scale_fill_manual(values=c("green","darkgreen"))

##======================
##   Change Detection  =
##======================

# Create empty raster with same extent, resolution and crs as classifications
r <- terra::rast(ext(classification_summer), resolution=res(classification_summer))
crs(r) <- toString(crs(classification_summer))

# Seagrass extent stays the same: 1
# Non-Seagrass extent stays the same: 2
# Seagrass extent changes from summer to winter: 3
vr <- terra::values(r)
# vcs <- terra::values(classification_summer)
# vcw <- terra::values(classification_winter)

for (i in 1:length(vr)) {
  if(is.na(vcs[i]) | is.na(vcw[i])){
    vr[i] <- 0
  }
  else if (vcs[i] == 3) {
    if (vcw[i] == 3) {
      vr[i] <- 2
    } else {
      vr[i] <- 3
    }
  } else {
    if (vcw[i] == 3) {
      vr[i] <- 3
    } else {
      vr[i] <- 1
    }
  }
}

r <- setValues(r,vr)

# Plot change
cols_cd <- c("white", "darkgreen", "sandybrown","green")
classnames_cd <- c("NA", "always seagrass", "never seagrass", "change of extent")

plot(r,
     col = cols_cd,
     main = "Change Detection of Summer and Winter Seagrass Extent in the North Frisian Wadden Sea",
     type = "classes",
     levels = classnames_cd)