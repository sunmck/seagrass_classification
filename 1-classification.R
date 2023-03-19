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

# Load Sentinel-2 data if needed
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

# Parameters to play around with
# Points sampled per class
nb_points = 200 # also tested with: 100, 1000, 500
# Factor to sample more points of class 3 because area of non-seagrass is bigger and more heterogene
factor_class_3 = 10 # also tested with: 1, 5

sf_points_labeled <- list()

for(i in unique(seagrass_training$class_id)){
  # message(paste0("Sampling points from polygons with class_id=", i))
  n <- 1
  if (i == 3) {
    n <- factor_class_3
  }
  sf_points_labeled[[i]] <- st_sample(
    x = seagrass_training[seagrass_training$class_id == i,],
    size = nb_points * n # amount of points saved per class
  )
  sf_points_labeled[[i]] <- st_as_sf(sf_points_labeled[[i]])
  sf_points_labeled[[i]]$resp_var <- i
}
# Data frame with labeled points of seagrass_training sf
sf_points_labeled <- do.call(rbind, sf_points_labeled)

# Extract features from Sentinel-2 imagery and label the repective pixels with the labeled_points response variable
# Choose features from Summer Raster because training data was mapped in summer
summer_stack <- s2_stacks[[1]]

s2_feat <- raster::extract(summer_stack, sf_points_labeled, df = T)
s2_feat <- s2_feat[,-1] # no ID column needed
s2_feat_labeled <- cbind(resp_var = sf_points_labeled$resp_var, s2_feat)

# Remove duplicates (in case multiple points fall into the same pixel)
dupl <- duplicated(s2_feat_labeled)
which(dupl)
length(which(dupl)) # Number of duplicates in labeled features that we need to remove
s2_feat_labeled <- s2_feat_labeled[!dupl,]
s2_feat_labeled <- na.omit(s2_feat_labeled)

# Split into training (80%) and test data (20%)
set.seed(123)  # for reproducibility
index <- sample(1:nrow(s2_feat_labeled), round(nrow(s2_feat_labeled) * 0.8))
train_data <- s2_feat_labeled[index,]
test_data  <- s2_feat_labeled[-index,]

# Define input to model
x <- train_data[,2:ncol(train_data)] # remove ID column
y <- as.factor(train_data$resp_var) # we want caret to treat this as categories, thus factor

# Same for the test data
x_test <- test_data[,2:ncol(test_data)]
y_test <- as.factor(test_data$resp_var)

# Rename levels, doesn't work with class numbers
levels(y)[1] <- "seagrass_coarse"
levels(y)[2] <- "seagrass_dense"
levels(y)[3] <- "non_seagrass"
levels(y_test)[1] <- "seagrass_coarse"
levels(y_test)[2] <- "seagrass_dense"
levels(y_test)[3] <- "non_seagrass"

# Train the model using random forest
set.seed(825)
model <- train(
  x = x,
  y = y,
  trControl = trainControl(
    p = 0.75, # percentage of samples used for training, rest for validation
    method  = "cv", # cross validation
    number  = 5, # 5-fold
    verboseIter = T, # progress update per iteration
    classProbs = T # probabilities for each example
  ),
  method = "rf" # random forest
)

# Performance of random forest model
# Check performance of the model with train_data
model
confusionMatrix(model)
# Final check performance with test_data that was not used when training the model
confusionMatrix(reference = y_test,
                data = predict(model, x_test))

# Train alternative model using SVM
set.seed(321)
model_svm <- train(
  x = x,
  y = y,
  trControl = trainControl(
    p = 0.75, # percentage of samples used for training, rest for validation
    method  = "cv", # cross validation
    number  = 5, # 5-fold
    verboseIter = TRUE, # progress update per iteration
    classProbs = TRUE # probabilities for each example
  ),
  #preProcess = c("center", "scale"), #center/scale if not done by you on the raster (see previous code rescl)
  method = "svmRadial" # SVM
)

# Performance of SVM model
# Check performance of the model with train_data
model_svm
confusionMatrix(model_svm)
# Final check performance with test_data that was not used when training the model
confusionMatrix(reference = y_test,
                data = predict(model_svm, x_test))

# Choose random forest model although overall svm accuracy is higher because more seagrass is defined correctly
# Main problem is that data base is not very good, and...
# I will hopefully improve the model building after next semesters machine learning course ;)


##=============================
##  Seagrass Classification   =
##=============================

# Get cropped images of relevant water depths either from s2_stacks_cropped or load .tif file
image_summer <- s2_stacks_cropped[[1]]
# image_summer <- raster("./data/cropped/summer_22.tif" )
image_winter <- s2_stacks_cropped[[2]]
# image_winter <- raster("./data/cropped/winter_23.tif" )
plot(image_summer)

# Predict seagrass coverage with rf model for relevant water depths of Sentinel-2 images and save as .tif
classification_summer <- predict(image_summer, model, type='raw')
# Create new folder, if not already existing
dir.create("./data/classification/", showWarnings = FALSE)
writeRaster(classification_summer, filename = paste0("./data/classification/classification_summer.tif", sep = ""), overwrite=T)

classification_winter <- predict(image_winter, model, type='raw')
writeRaster(classification_winter, filename = paste0("./data/classification/classification_winter.tif", sep = ""), overwrite=T)


# Plot classifications
cols_class <- c("green", "darkgreen","sandybrown") # colours
par(mfrow = c(1,2))
plot(classification_summer, col = cols_class)
plot(classification_winter, col = cols_class)
