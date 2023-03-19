# Load libraries
library(ggplot2)
library(terra)
library(raster)
library(sf)

# Load classifications
classification_summer <- raster("./classification/classification_summer.tif")
classification_winter <- raster("./classification/classification_winter.tif")

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

# Calculate total area of seagrass
seagrass_area_summer <- as.data.frame(classification_summer) %>%
  group_by(layer) %>%
  tally() %>%
  mutate(area = (n * res(classification_summer)[1] * res(classification_summer)[2])/100000)

seagrass_area_winter <- as.data.frame(classification_winter) %>%
  group_by(layer) %>%
  tally() %>%
  mutate(area = (n * res(classification_winter)[1] * res(classification_winter)[2])/1000000)

# Add column with season
seagrass_area_summer$season <- "summer"
seagrass_area_winter$season <- "winter"


# Merge to single data frame
seagrass_area_df <- data.frame()
seagrass_area_df <- rbind(seagrass_area_summer, seagrass_area_winter)
seagrass_area_df <- subset(seagrass_area_df,layer <= 2)

# Add column with class name
seagrass_area_df$class <- ifelse(seagrass_area_df$layer == 1, "20-60%", ifelse(seagrass_area_df$layer == 2, ">60%", "NA"))
seagrass_area_df$area <- round(seagrass_area_df$area)

# Plot seagrass area
ggplot(seagrass_area_df, aes(fill=class, y=area, x=season)) +
  geom_bar(position='stack', stat='identity') +
  labs(x="area [km2]", title = "Area of Seagrass in the North Frisian Wadden Sea") +
  scale_fill_manual(values=c("darkgreen","green")) +
  geom_text(aes(y = area, label = area), vjust = 1.5, colour = "white")


##======================
##   Change Detection  =
##======================

# Create empty raster with same extent, reolution and crs as classifications
r <- terra::rast(ext(classification_summer), resolution=res(classification_summer))
crs(r) <- toString(crs(classification_summer))

# Seagrass extent stays the same: 1
# Non-Seagrass extent stays the same: 2
# Seagrass extent changes from summer to winter: 3
vr <- terra::values(r)
vcs <- terra::values(classification_summer)
vcw <- terra::values(classification_winter)

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
classnames_cd <- c("NA", "always seagrass", "never seagrass", "change of seagrass extent")

plot(r,
     col = cols_cd,
     main = "Change Detection of Summer and Winter Seagrass Extent in the North Frisian Wadden Sea",
     type = "classes",
     levels = classnames_cd)
