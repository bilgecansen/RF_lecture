
library(magrittr)
library(sf)
library(terra)
library(dplyr)
library(spatstat)
library(mlr)
library(tuneRanger)

# GOAL: to use RF to classify shaded pixels as land vs water as a way to 
# make accurate land_mask around islands in Antarctica

# PROBLEM: NDWI is usually used to classify land from water in satellite 
# imagery. Usually when ndwi is > 0.4 - 0.5 a pixel is water and land otherwise. 
# However, shade and water are hard to tell apart spectrally, making it 
# difficult to use NDWI to classify land from water in shaded areas of landsat 
# imagery

# load the landsat mosaic raster around Possession Island in Antarctica
r <- terra::rast("rasters/landsat_mosaic.tif")
# load the REMA DEM around Possession Island in Antarctica
dem <- terra::rast("rasters/dem.tif")

# load the water and shade mask based on the landsat mosaic raster around 
# Possession Island in Antarctica
water_mask <- terra::rast("rasters/landsat_water_mask.tif")
shade_mask <- terra::rast("rasters/landsat_shade_mask.tif")

# ok we want to use RF to classify pixels in the shade mask as water or land
# first let's check how bad the original water mask was at separating water 
# from land an easy way to look at this is to open the mask in google earth
# set to image to 2/2010 for best results
land_sf <- sf::st_as_sf(as.polygons(water_mask, dissolve = TRUE)) %>% 
  dplyr::filter(water == 0)
kml_rough <- paste0(tempdir(), "/water_mask.kml")
sf::st_write(land_sf, dsn = kml_rough, layer = "land", driver = "KML", 
             append = FALSE)
system(command <- paste("open -a 'Google Earth Pro' ", kml_rough))

# now do the same thing with the shade mask
# notice the water_mask is not very accurate in this shaded area
shade_sf <- sf::st_as_sf(as.polygons(shade_mask, dissolve = TRUE)) %>% 
  dplyr::filter(shade == 1)
kml_rough <- paste0(tempdir(), "/shade_mask.kml")
sf::st_write(shade_sf, dsn = kml_rough, layer = "land", driver = "KML", 
             append = FALSE)
system(command <- paste("open -a 'Google Earth Pro' ", kml_rough))

# let's use RF to see if we can use spectral and information from the DEM to 
# predict water vs land in this shaded area.
# first we need a training set - you can either load the one that we provide 
train_mask <- terra::rast("rasters/train_mask.tif")

# or make one yourself by clicking on the mask to make a polygon using 
# clickpoly from the spatstat package
water_mask2 <- water_mask + 5 * shade_mask
# select some points not in the area where the plot is 5 or 6 by clicking 
# with your mouse.
# make sure you pick some water (0) and some land (1)
plot(water_mask2)
pts <- clickpoly(add = TRUE)
train_sf <- st_as_sf(as.owin(pts), crs = 4326)
train_mask <- water_mask
names(train_mask) <- "train"
train_mask[] <- 0
train_mask[terra::vect(train_sf)] <- 1

# ok these variables can be used in the RF, either the spectral data and dem data
# or just the dem data, try both and see which does better!
rf_vars  <- c("blue",  "red",   "green", "nir", "swir1", "ndwi", "dem", 
              "slope", "cv", "aspect", "roughness")
rf_vars  <- c("dem", "slope", "cv", "aspect", "roughness")

# make land mask as invert of water mask
land_mask <- 1 - water_mask
names(land_mask) <- "land"
r2 <- c(land_mask, dem, r, train_mask, shade_mask)
pred_vars <- c("land", rf_vars, "x", "y")
train_vars <- c("land", rf_vars)

# convert to df
rf_df <- as.data.frame(r2, xy = TRUE)

# subset for prediction
pred_df <- rf_df %>% dplyr::filter(shade == 1) %>% 
  dplyr::mutate(land = NA) %>% 
  dplyr::select(all_of(c(pred_vars)))

# subset for training
train_df <- rf_df %>% 
  dplyr::filter(shade == 0 & train == 1) %>% 
  dplyr::mutate(land = as.factor(land)) %>% 
  dplyr::select(all_of(train_vars))

# make df for spatial stratification
train_coords_df <- rf_df %>% 
  dplyr::filter(shade == 0 & train == 1) %>% 
  dplyr::select(x, y)
pred_vars <- c("land", pred_vars)

# make land classification task
mask_task = mlr::makeClassifTask(
  data = train_df, 
  coordinates = train_coords_df,
  target = "land")

# tune rf
rf <- tuneRanger::tuneRanger(
  task = mask_task, 
  measure = list(multiclass.brier), 
  num.trees = 1000,
  num.threads = detectCores() - 1, 
  iters = 70, 
  save.file.path = NULL, 
  tune.parameters = c("mtry", "min.node.size", "sample.fraction"))

# output prediction probability where default is .5 for labeling as 1 (land)
pred_out <- predict(rf$model, newdata = pred_df)
pred_df$prob <- pred_out$data$prob.1
pred_df <- pred_df %>% 
  dplyr::select(x, y, prob) %>% 
  dplyr::right_join(rf_df %>% dplyr::select(x, y), by = c("x", "y"))
pred_water_mask <- terra::rast(pred_df, crs = crs(dem), type = 'xyz')
pred_water_mask <- terra::ifel(pred_water_mask >= .5, 0, 1)
plot(pred_water_mask)

# now we need to combine these predictions with the original water mask
pred_water_mask[is.na(pred_water_mask)] <- 0
updated_water_mask <- water_mask 
updated_water_mask[shade_mask == 1] <- 0
updated_water_mask <- updated_water_mask + pred_water_mask
plot(water_mask)
plot(updated_water_mask)

# let's look at this again in google earth
land_sf <- sf::st_as_sf(as.polygons(updated_water_mask, dissolve = TRUE)) %>% 
  dplyr::filter(water == 0)
kml_rough <- paste0(tempdir(), "/updated_water_mask.kml")
sf::st_write(land_sf, dsn = kml_rough, layer = "land", driver = "KML", 
             append = FALSE)
system(command <- paste("open -a 'Google Earth Pro' ", kml_rough))
