# Load libraries
library(caret)
library(dplyr)
library(raster)

##=====================
##  Global Variables  =
##=====================

# Bands we want to stack for further analysis, here RGB
bands <- c("B04","B03","B02")

##==========================
##  Train Seagrass Model   =
##==========================

# Load Sentinel-2 data if not still saved in Global Environment
safe_dir <- read.csv("./data/safe_dir.csv")
s2_stacks <- list()

for (i in 1:nrow(safe_dir)){
  stack <- get_bands(safe_dir[i,], bands)
  names(stack) <- bands

  # Save RGB stacks
  s2_stacks <- append(s2_stacks, stack)
}

# Load and reproject groundtruth data and pre-processed training data
# Data downloaded from Umweltportal Schleswig-Holstein (https://umweltportal.schleswig-holstein.de/trefferanzeige?docuuid=a14cf8b7-dd8c-4661-9f67-10ee73ae1feb)
seagrass_gt <- st_read("./data/groundtruth/2021_seagrass_UTM.shp")
# Class_id 1: seagrass with coverage of 20-60%
# Class_id 2: seagrass with coverage of >60%
# Class_id 3: no seagrass
seagrass_training <- st_read("./data/training/2021_seagrass_training.shp")

# Randomly select certain amount of points in each training polygons class and save labels to them
labeled_points <- list()
for(i in unique(seagrass_training$class_id)){
  message(paste0("Sampling points from polygons with resp_var=", i))

  # sample points for polygons of resp_var = i
  labeled_points[[i]] <- st_sample(
    x = seagrass_training[seagrass_training$class_id == i,],
    size = 500 # amount of points saved per class
  )
  labeled_points[[i]] <- st_as_sf(labeled_points[[i]])
  labeled_points[[i]]$resp_var <- i
}
labeled_points <- do.call(rbind, labeled_points)

# Extract features and label them with our response variable
# Choose features from Summer Raster because training data was mapped in summer
  unlabeled_features <- raster::extract(s2_stacks[[1]], labeled_points, df = T)
  unlabeled_features <- unlabeled_features[,-1] # no ID column needed
  labeled_features <- cbind(
    resp_var = labeled_points$resp_var,
    unlabeled_features
  )

# Remove duplicates (in case multiple points fall into the same pixel)
dupl <- duplicated(labeled_features)
which(dupl)
length(which(dupl)) # Number of duplicates in labeled features that we need to remove
labeled_features <- labeled_features[!dupl,]
labeled_features <- na.omit(labeled_features)

# Fit the classification model, here a random Forest
x <- labeled_features[,2:ncol(labeled_features)] # remove ID column
y <- as.factor(labeled_features$resp_var) # we want caret to treat this as categories, thus factor
levels(y)[1] <- "seagrass_coarse"
levels(y)[2] <- "seagrass_dense"
levels(y)[3] <- "non_seagrass"

set.seed(825)
model <- train(
  x = x,
  y = y,
  trControl = trainControl(
    p = 0.75, # Percentage of samples used for training, rest for validation
    method  = "cv", # Cross validation
    number  = 5, # 5-fold
    verboseIter = T, # Progress update per iteration
    classProbs = T # Probabilities for each example
  ),
  method = "rf" # random forest
)

# Performance
model
confusionMatrix(model)

##=============================
##  Seagrass Classification   =
##=============================

# Get cropped images of relevant water depths either from s2_stacks_cropped or load .tif file
image_summer <- s2_stacks_cropped[[1]]
# image_summer <- raster("./data/cropped/summer_22.tif" )
image_winter <- s2_stacks_cropped[[2]]
# image_winter <- raster("./data/cropped/winter_22.tif" )

# Predict seagrass coverage for relevant water depths of Sentinel-2 images and save as .tif
classification_summer <- predict(image_summer, model, type='raw')
writeRaster(classification_summer, filename = paste0("./classification/classification_summer.tif", sep = ""), datatype = "INT1U", overwrite=T)

classification_winter <- predict(image_winter, model, type='raw')
writeRaster(classification_winter, filename = paste0("./classification/classification_winter.tif", sep = ""), datatype = "INT1U", overwrite=T)


# Plot classifications
cols_class <- c("green", "darkgreen","sandybrown")
par(mfrow = c(1,2))
plot(classification_summer, col = cols_class)
plot(classification_winter, col = cols_class)
