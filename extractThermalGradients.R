#========================================================================================
# Script to extract regions of interest and temperature gradients from thermal images
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('brick')) library (raster)
if (!existsFunction ('tibble')) library (tidyverse)
if (!existsFunction ('as_date')) library (lubridate)
if (!exists ('tColours')) source ('plotingFunctions.R')

# define functions to get standard deviation for rows of data
#----------------------------------------------------------------------------------------
rowSd <- function (x, na.rm = FALSE) apply (X = x, MARGIN = 1, FUN = sd, na.rm = na.rm)

# define function to extract temperature data from FLIR output file
#----------------------------------------------------------------------------------------
read_flir_csv <- function (filename, flip = FALSE, rot = FALSE) {
  
  # open connection
  conn <- file (filename, open="r")
  
  # read lines
  linn <- readLines (conn)
  
  # close connection
  close (conn)
  
  # split the input string
  fn <- strsplit (linn [1],',') [[1]] [2]
  
  data <- paste (linn [3:length (linn)], collapse = "")
  data <- strsplit (data,',')
  data <- as.numeric (data [[1]] [-1])
  
  # extract thermal data and flip matrix, if necessary
  if (flip == TRUE){
    thermaldata <- raster (matrix (data, nrow = 320, ncol = 76800 / 320) [1:320, ])
    extent (thermaldata) <- extent (0, 240, 0, 320)
    
  } else {
    thermaldata <- raster (matrix (data, nrow = 320, ncol = 76800 / 320) [320:1, ])
    extent (thermaldata) <- extent (0, 240, 0, 320)
  }
  
  # rotate image matrix if necessary
  if (rot == TRUE) thermaldata <- t (flip (thermaldata, 2))
  
  # return thermal data
  return (thermaldata)
}

# define function to extract gradient from ROI
#----------------------------------------------------------------------------------------
get_gradient <- function (thermalData, # input thermal data (degC) 
                          xMin, xMax, yMin, yMax, # Coordinates of ROI 
                          xScale, yScale, dScale, scaleOffset, # Parameters to scale image 
                          treeID, image){
  
  # estimate resolution
  res <- dScale / sqrt (diff (xScale)^2 + diff (yScale)^2)
  
  # create three plot of thermal image, region of interest, and temperature gradient
  par (mfrow = c (1,3))
  plot (thermalData, main = image)
  rect (xMin, yMin, xMax, yMax)
  points (x = xScale, y = yScale, pch = 19, col = 'darkred')
  segments (x0 = xScale [1], y0 = yScale [1],
            x1 = xScale [2], y1 = yScale [2], col = 'darkred')
  
  # crop image to region of interest
  selection <- crop (thermalData, extent (xMin, xMax, yMin, yMax))
  plot (selection, main = treeID)
  abline (h = yScale, col = 'darkred')
  
  # initialise vecotr of pixels
  y <- yMax:(yMin + 1)
  
  # scale pixels to distance above ground
  h <- (y - yScale [1]) * res + scaleOffset
  
  # calculate mean and standard deviation for temperature along the gradient
  tgradient <- rowMeans (as.matrix (selection))
  tgradientSD <- rowSd (as.matrix (selection))
  plot (x = tgradient, 
        y = h, las = 1,
        xlim = c (5, 30))
  
  # plot lower collar
  rect (xleft = rep (0, 2), xright = rep (35, 2), ybottom = c (0.85, 1.85), 
        ytop = c (1.15, 2.15), lty = 0, 
        col = addOpacity (tColours [['colour']] [4], 0.6))
  
  # plot uncertainty
  segments (tgradient - tgradientSD, h, 
            tgradient + tgradientSD, h)
  
  # get temperature gradient
  tg <- tibble (y = y, h = h, t = tgradient, SD = tgradientSD, 
                tree = treeID, image = image)
  return (tg)
}

# set working directory
#----------------------------------------------------------------------------------------
orgDir <- getwd ()
setwd ('/media/tim/dataDisk/PlantGrowth/data/thermalImages/')

# define directory with files
#----------------------------------------------------------------------------------------
datapath <- "./selectedImages"

# camera time was not correct for first batch of images and needs to be corrected
#----------------------------------------------------------------------------------------
cameraTime <- as_datetime ('2018:08:25 04:34:00', tz = 'EST')
imageTime <- as_datetime ('2018:08:24 22:38:23', tz = 'EST')
timeDiff <- time_length (cameraTime - imageTime) # Time difference in seconds

# read file with metadata and delete rows without ROIs
#----------------------------------------------------------------------------------------
imageInfo <- read_csv ('metadata_FLIR.csv', col_types = cols ())
imageInfo <- imageInfo %>% filter (!is.na (n.rois))

# correct imageInfo to include the correct time
#----------------------------------------------------------------------------------------
imageInfo <- imageInfo %>%
  mutate (datetime = as_datetime (ifelse (incorrect.timestamp, camera.datetime - timeDiff, 
                                          camera.datetime))) 

# output graphs for all selected images to pdf
#----------------------------------------------------------------------------------------
pdf ("flir_2018.pdf", width = 16, height = 10)
for (i in 1:nrow (imageInfo)) { #1:nrow (imageInfo)){
  filename <- sprintf ("%s.csv", file.path (datapath, imageInfo$record [i]))
  filename_photo <- sprintf ("%s-photo.jpg", file.path (datapath, imageInfo$record [i]))
  
  # plot photo
  defaultW <- getOption ("warn")
  options (warn = -1)
  op <- par ()
  par (mfrow = c (1, 1))
  photo <- raster::brick (filename_photo)
  plotRGB (photo)
  par (op)
  options (warn = defaultW)
  
  # read Data
  thermaldata <- read_flir_csv (filename, imageInfo$flip [i], imageInfo$rotate [i])
  
  # extract tree ids
  treeIDs <- imageInfo %>% dplyr::slice (i) %>% select (tree.ids) %>% 
    unlist () %>% strsplit (split = ', ') %>% 
    unlist () %>% as.numeric ()
  
  # loop over each region of interest
  for (n in 1:imageInfo$n.rois [i]) {
    if (n == 1) {
      # get gradient
      tmp <- get_gradient (thermalData = thermaldata,
                           xMin = imageInfo$x1.1 [i], 
                           xMax = imageInfo$x1.2 [i], 
                           yMin = imageInfo$y1.1 [i], 
                           yMax = imageInfo$y1.2 [i],
                           xScale = c (imageInfo$x.scale.1.1 [i], 
                                       imageInfo$x.scale.1.2 [i]),
                           yScale = c (imageInfo$y.scale.1.1 [i], 
                                       imageInfo$y.scale.1.2 [i]),
                           dScale = imageInfo$d.scale.1 [i],
                           scaleOffset = imageInfo$scale.offset.1 [i],
                           treeID = treeIDs [n], 
                           image = imageInfo$record [i])
    } else if (n == 2) {
      # get gradient
      tmp <- get_gradient (thermalData = thermaldata,
                           xMin = imageInfo$x2.1 [i], 
                           xMax = imageInfo$x2.2 [i], 
                           yMin = imageInfo$y2.1 [i], 
                           yMax = imageInfo$y2.2 [i],
                           xScale = c (imageInfo$x.scale.2.1 [i], 
                                       imageInfo$x.scale.2.2 [i]),
                           yScale = c (imageInfo$y.scale.2.1 [i], 
                                       imageInfo$y.scale.2.2 [i]),
                           dScale = imageInfo$d.scale.2 [i],
                           scaleOffset = imageInfo$scale.offset.2 [i],
                           treeID = treeIDs [n], 
                           image = imageInfo$record [i])
    } else if (n == 3) {
      # get gradient
      tmp <- get_gradient (thermalData = thermaldata,
                           xMin = imageInfo$x3.1 [i], 
                           xMax = imageInfo$x3.2 [i], 
                           yMin = imageInfo$y3.1 [i], 
                           yMax = imageInfo$y3.2 [i],
                           xScale = c (imageInfo$x.scale.3.1 [i], 
                                       imageInfo$x.scale.3.2 [i]),
                           yScale = c (imageInfo$y.scale.3.1 [i], 
                                       imageInfo$y.scale.3.2 [i]),
                           dScale = imageInfo$d.scale.3 [i],
                           scaleOffset = imageInfo$scale.offset.3 [i],
                           treeID = treeIDs [n], 
                           image = imageInfo$record [i])
    } else if (n == 4) {
      # get gradient
      tmp <- get_gradient (thermalData = thermaldata,
                           xMin = imageInfo$x4.1 [i], 
                           xMax = imageInfo$x4.2 [i], 
                           yMin = imageInfo$y4.1 [i], 
                           yMax = imageInfo$y4.2 [i],
                           xScale = c (imageInfo$x.scale.4.1 [i], 
                                       imageInfo$x.scale.4.2 [i]),
                           yScale = c (imageInfo$y.scale.4.1 [i], 
                                       imageInfo$y.scale.4.2 [i]),
                           dScale = imageInfo$d.scale.4 [i],
                           scaleOffset = imageInfo$scale.offset.4 [i],
                           treeID = treeIDs [n], 
                           image = imageInfo$record [i])
    } else if (n == 5) {
      # get gradient
      tmp <- get_gradient (thermalData = thermaldata,
                           xMin = imageInfo$x5.1 [i], 
                           xMax = imageInfo$x5.2 [i], 
                           yMin = imageInfo$y5.1 [i], 
                           yMax = imageInfo$y5.2 [i],
                           xScale = c (imageInfo$x.scale.5.1 [i], 
                                       imageInfo$x.scale.5.2 [i]),
                           yScale = c (imageInfo$y.scale.5.1 [i], 
                                       imageInfo$y.scale.5.2 [i]),
                           dScale = imageInfo$d.scale.5 [i],
                           scaleOffset = imageInfo$scale.offset.5 [i],
                           treeID = treeIDs [n], 
                           image = imageInfo$record [i])
    }
    # add datetime to the data tibble
    tmp <- tmp %>% mutate (datetime = imageInfo$datetime [i])
    
    # append new data to long format data tibble
    if (i == 1 & n == 1) {
      gradients <- tmp; rm (tmp)
    } else {
      gradients <- gradients %>% add_row (tmp); rm (tmp)
    }
  }
}
dev.off ()

# This dataframe contains all the gradient data
#----------------------------------------------------------------------------------------
gradient_data <- do.call (rbind, gradients)

# continue to work with gradient data here 
#----------------------------------------------------------------------------------------

# reset working directory 
#----------------------------------------------------------------------------------------
setwd (orgDir)
#========================================================================================