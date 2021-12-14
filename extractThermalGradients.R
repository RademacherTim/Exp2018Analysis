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
for (i in 1:nrow (imageInfo)) {
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
  treeIDs <- imageInfo %>% dplyr::slice (i) %>% dplyr::select (tree.ids) %>% 
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

# reset working directory 
#----------------------------------------------------------------------------------------
setwd (orgDir)

# addthe nearest 15 minute interval to match met station and thermistor data format 
#----------------------------------------------------------------------------------------
gradients <- gradients %>%
  add_row (datetime = as_datetime ('2018-04-19 14:30:00'), .before = 1) %>%
  mutate (timeSlice = as_datetime (cut (datetime, breaks = '15 min', ordered_result = TRUE))) %>%
  slice (-1)

# add treatment to gradients tibble (N.B. for temperature compression is control)
#----------------------------------------------------------------------------------------
gradients <- gradients %>% mutate (treatment = ifelse (tree <= 1805, 'chilled', 'control')) 


# read Fisher station temperature data from Harvard Forest Data Archive and add it to the 
# tibble
#----------------------------------------------------------------------------------------
LOCAL <- TRUE
if (LOCAL) {
  tmp <- read_csv (file = './data/hf001-10-15min-m-airt-only.csv',
                   col_types = cols ())
} else {
  fileURL <- url ('https://harvardforest.fas.harvard.edu/data/p00/hf001/hf001-10-15min-m.csv')
  tmp <- read_csv (file = fileURL, col_types = cols (
    f.airt = col_character (),
    f.prec = col_character (),
    f.rh   = col_character (),
    f.dewp = col_character (),
    f.slrr = col_character (),
    f.parr = col_character (),
    f.bar  = col_character (),
    f.wspd = col_character (),
    f.wres = col_character (),
    f.wdir = col_character (),
    f.wdev = col_character (),
    f.gspd = col_character (),
    f.s10t = col_character ())
  ) %>% select (datetime, airt) %>% filter (datetime > min (tempData [['datetime']], na.rm = TRUE), 
                                            datetime < max (tempData [['datetime']], na.rm = TRUE)) 
  # Save a local copy 
  #--------------------------------------------------------------------------------------
  write_csv (tmp, './data/hf001-10-15min-m-airt-only.csv')
  LOCAL <- TRUE
}

# add air temperature to the tibble with phloem temperature gradients
#----------------------------------------------------------------------------------------
gradients <- left_join (x = gradients, y = tmp, by = c ('timeSlice' = 'datetime')); rm (tmp)

# calculate temperature difference from air temperature 
#----------------------------------------------------------------------------------------
gradients <- gradients %>% mutate (deltaT = t - airt)

# bin height by 10cm increments
#----------------------------------------------------------------------------------------
gradients <- gradients %>% 
  mutate (height = cut (h, breaks = seq (0, 10, by = 0.1), ordered_results = TRUE))

# bin by days
#----------------------------------------------------------------------------------------
gradients <- gradients %>% mutate (date = date (datetime))

# summarise by treatment and heights
#----------------------------------------------------------------------------------------
tg <- gradients %>% group_by (treatment, date, height) %>%
  summarise (meanDeltaT = mean (deltaT),
             sdDeltaT = sd (deltaT), .groups = 'drop') %>% 
  filter (!is.na (height))

# source thermistor data
#----------------------------------------------------------------------------------------
if (!exists ('tempData')) source ('readTemperatureData.R')

# plot mean temperature gradients 
#----------------------------------------------------------------------------------------
par (mfrow = c (1, 1), mar = c (5, 5, 1, 1))
con <- tg$treatment == 'control' & tg$date == as_date ('2018-08-24') 
plot (x = tg$meanDeltaT [con], y = as.numeric (tg$height [con]) * 0.1, typ = 'l',
      axes = FALSE, xlim = c (-25, 1), ylim = c (0, 3.9), 
      col = 'white', lwd = 2, 
      xlab = expression (paste ('Temperature deviation (',degree,'C)', sep = '')),
      ylab = 'Height above ground (m)')
axis (side = 1) 
axis (side = 2, las = 1)
abline (v = 0, col = '#66666666')
#rect (xleft = -12, xright = 2, ybottom = 0.85, ytop = 1.15, lty = 0, col = '#66666666')
#rect (xleft = -12, xright = 2, ybottom = 1.85, ytop = 2.15, lty = 0, col = '#66666666')
polygon (x = c (tg$meanDeltaT [con] - tg$sdDeltaT [con], 
                rev (tg$meanDeltaT [con] + tg$sdDeltaT [con])),
         y = c (as.numeric (tg$height [con]) * 0.1 - 0.1, 
                rev (as.numeric (tg$height [con]) * 0.1 - 0.1)),
         col = addOpacity (tColours [['colour']] [1], 0.4), lty = 0)
lines (x = tg$meanDeltaT [con], y = as.numeric (tg$height [con]) * 0.1 - 0.1, 
       col = tColours [['colour']] [1], lwd = 2)

# add midnight measurement for chilled lines
#----------------------------------------------------------------------------------------
con <- tg$treatment == 'chilled' & tg$date == as_date ('2018-08-24') 
polygon (x = c (tg$meanDeltaT [con] - tg$sdDeltaT [con], 
                rev (tg$meanDeltaT [con] + tg$sdDeltaT [con])),
         y = c (as.numeric (tg$height [con]) * 0.1 - 0.1, 
                rev (as.numeric (tg$height [con]) * 0.1 - 0.1)),
         col = addOpacity (tColours [['colour']] [6], 0.4), lty = 0)
lines (x = tg$meanDeltaT [con], y = as.numeric (tg$height [con]) * 0.1 - 0.1, 
       col = tColours [['colour']] [6], lwd = 2, lty = 2)

# add midday measwurement for chilled lines
#----------------------------------------------------------------------------------------
con <- tg$treatment == 'chilled' & tg$date == as_date ('2018-08-25') 
polygon (x = c (tg$meanDeltaT [con] - tg$sdDeltaT [con], 
                rev (tg$meanDeltaT [con] + tg$sdDeltaT [con])),
         y = c (as.numeric (tg$height [con]) * 0.1 - 0.1, 
                rev (as.numeric (tg$height [con]) * 0.1 - 0.1)),
         col = addOpacity (tColours [['colour']] [4], 0.4), lty = 0)
lines (x = tg$meanDeltaT [con], y = as.numeric (tg$height [con]) * 0.1 - 0.1, 
       col = tColours [['colour']] [4], lwd = 2)

# extract thermistor data from under the collars at the time of measurement
#----------------------------------------------------------------------------------------
temp <- tempData %>% 
  filter ((datetime > as_datetime ('2018-08-24 22:00:00') & 
             datetime <= as_datetime ('2018-08-24 23:00:00')) |
          (datetime > as_datetime ('2018-08-25 14:00:00') & 
             datetime <= as_datetime ('2018-08-25 15:00:00'))) %>%
  dplyr::select (datetime, t.01.1p0m, t.01.1p5m, t.01.2p0m, t.02.1p0m, t.02.1p5m, t.02.2p0m, 
                 t.03.1p0m, t.03.1p5m, t.03.2p0m, t.04.1p0m, t.04.1p5m, t.04.2p0m, 
                 t.05.1p0m, t.05.1p5m, t.05.2p0m, t.06.1p5m, t.07.1p5m, t.08.1p5m, 
                 t.10.1p5m, t.air.1p5m) %>% 
  pivot_longer (cols = !datetime, names_to =  c ('tree','height'), 
                names_prefix = 't.', 
                names_pattern = '(.*)\\.(.*)', 
                values_to = 'temp') %>%
  mutate (height = as.numeric (paste (substr (height, 1, 1), 
                                      substr (height, 3, 3), sep = '.'))) %>%
  mutate (date = date (datetime),
          treatment = ifelse (tree %in% c ('01','02','03','04','05'), 'chilled', 
                              ifelse (tree %in% c ('06','07','08','09','10'), 'control', 'air'))) %>%
  group_by (date, treatment, height) %>% 
  summarise (meanT = mean (temp, na.rm = TRUE),
             sdT = sd (temp, na.rm = TRUE), .groups = 'drop')
temp <- temp %>% mutate (deltaT = meanT - meanT [treatment == 'air'])
# add the control tree thermistor data
con <- temp$treatment == 'control'
segments (y0 = 1.5, 
          x0 = mean (temp$deltaT [con] - temp$sdT [con]),
          x1 = mean (temp$deltaT [con] + temp$sdT [con]),
          col = tColours [['colour']] [1], lwd = 3)
points (x = mean (temp$deltaT [con]),
        y = 1.5, pch = 19, cex = 2, col = tColours [['colour']] [1])
# add the midnight chilled tree thermistor data
con <- temp$treatment == 'chilled' & temp$date == as_date ('2018-08-24')
segments (y0 = c (1, 1.5, 2.0), 
          x0 = temp$deltaT [con] - temp$sdT [con],
          x1 = temp$deltaT [con] + temp$sdT [con],
          col = tColours [['colour']] [6], lwd = 3)
points (x = temp$deltaT [con],
        y = c (1, 1.5, 2), pch = 23, lwd = 3, cex = 2, 
        bg = 'white', col = tColours [['colour']] [6])
# add the midnight chilled tree thermistor data
con <- temp$treatment == 'chilled' & temp$date == as_date ('2018-08-25')
segments (y0 = c (1, 1.5, 2.0), 
          x0 = temp$deltaT [con] - temp$sdT [con],
          x1 = temp$deltaT [con] + temp$sdT [con],
          col = tColours [['colour']] [4], lwd = 3)
points (x = temp$deltaT [con],
        y = c (1, 1.5, 2), pch = 23, lwd = 3, cex = 2, 
        bg = 'white', col = tColours [['colour']] [4])


# plot temperature deviations after the chilling was switched off for supp
con <- tg$treatment == 'control' & tg$date == as_date ('2018-09-14') 
plot (x = tg$meanDeltaT [con], y = as.numeric (tg$height [con]) * 0.1, typ = 'l',
      axes = FALSE, xlim = c (-10, 2), ylim = c (0, 4), 
      col = 'white', lwd = 2, 
      xlab = expression (paste ('Temperature deviation (',degree,'C)', sep = '')),
      ylab = 'Height above ground (m)')
axis (side = 1) 
axis (side = 2, las = 1)
rect (xleft = -12, xright = 2, ybottom = 0.85, ytop = 1.15, lty = 0, col = '#66666666')
rect (xleft = -12, xright = 2, ybottom = 1.85, ytop = 2.15, lty = 0, col = '#66666666')
polygon (x = c (tg$meanDeltaT [con] - tg$sdDeltaT [con], 
                rev (tg$meanDeltaT [con] + tg$sdDeltaT [con])),
         y = c (as.numeric (tg$height [con]) * 0.1 - 0.1, 
                rev (as.numeric (tg$height [con]) * 0.1 - 0.1)),
         col = addOpacity (tColours [['colour']] [1], 0.4), lty = 0)
lines (x = tg$meanDeltaT [con], y = as.numeric (tg$height [con]) * 0.1 - 0.1, 
       col = tColours [['colour']] [1], lwd = 2, lty = 2)

# add midday measurement for chilled lines
con <- tg$treatment == 'chilled' & tg$date == as_date ('2018-09-14') 
polygon (x = c (tg$meanDeltaT [con] - tg$sdDeltaT [con], 
                rev (tg$meanDeltaT [con] + tg$sdDeltaT [con])),
         y = c (as.numeric (tg$height [con]) * 0.1 - 0.1, 
                rev (as.numeric (tg$height [con]) * 0.1 - 0.1)),
         col = addOpacity (tColours [['colour']] [5], 0.4), lty = 0)
lines (x = tg$meanDeltaT [con], y = as.numeric (tg$height [con]) * 0.1 - 0.1, 
       col = tColours [['colour']] [5], lwd = 2, lty = 2)

# pivot temperature data longer
#----------------------------------------------------------------------------------------
tmp <- tempData %>% 
  select (datetime, t.oak.1p5m, t.air.2p0m, t.01.2p0m, t.01.1p5m, t.01.1p0m,  t.02.2p0m, 
          t.02.1p5m, t.02.1p0m, t.03.2p0m, t.03.1p5m, t.03.1p0m, t.04.2p0m, t.04.1p5m, 
          t.04.1p0m, t.05.2p0m, t.05.1p5m, t.05.1p0m, t.06.1p5m, t.07.1p5m, t.08.1p5m, 
          t.10.1p5m) %>%
  pivot_longer (cols = !datetime, names_to =  c ('tree','height'), 
                names_prefix = 't.', 
                names_pattern = '(.*)\\.(.*)', 
                values_to = 'temp') %>% 
  filter (!is.nan (temp),
          !is.na (temp),
          substr (tree, 1, 3) != 'end',
          substr (tree, 1, 4) != 'line') %>%
  mutate (treeID = tree) %>%
  mutate (height = as.numeric (paste (substr (height, 1, 1), substr (height, 3, 3), sep = '.'))) %>%
  mutate (tree = ifelse (tree == 'oak', -1799, ifelse (tree == 'air', -1800, tree))) %>%
  mutate (tree = as.numeric (tree) + 1800)

# subset to only the relevant dates and periods
#----------------------------------------------------------------------------------------
tmp <- tmp %>% filter ()

left_join  (x = gradients, y = tmp [tmp$treeID == 'air' & tmp$height != '1p5m'], by = c ('timeSlice'= 'datetime'))


#========================================================================================
