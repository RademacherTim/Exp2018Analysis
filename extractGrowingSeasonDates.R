# script to fit general additive model to ring width data from the 2018 experiment

# start from clean slate
#rm (list = ls ())

# load dependencies
#if (!existsFunction ('')) library ('mgcv')
#library ('easynls')
if (!exists ('ringWidths')) source ('readRingWidths.R')
if (!existsFunction ('scam')) library ('scam')

# create tibble for start and end of growing season dates
#----------------------------------------------------------------------------------------
growingSeasonDates <- tibble (treeId = rep (1:15, each = 4), 
                              treatment = rep (c (5, 4, 1), each = 20),
                              sampleHeight = rep (c (0.5, 1.5, 2.5, 4.0), 15),
                              startOfGrowth = NA,
                              endOfGrowth = NA,
                              maxRWIfit = NA)

# set growing season threshold
#----------------------------------------------------------------------------------------
threshold <- 0.05

# determine growing season dates and plot growth over time for chilled trees
#----------------------------------------------------------------------------------------
PLOT <- TRUE 
if (PLOT) {
  png (file = './fig/woodGrowthOverTimeChilledTrees.png', width = 1255, height = 622)
  layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
          heights = c (1, 1, 1, 1.3))
}
# Loop over heights
for (h in c (4.0, 2.5, 1.5, 0.5)) {
  # Loop over trees
  for (i in 1:5) 
  {
    
    # Determine plot margins
    if (PLOT) {
      if (i == 1 & h != 0.5) {
        par (mar = c (2, 5, 1, 1))
      } else if (i == 1 & h == 0.5) {
        par (mar = c (5, 5, 1, 1))
      } else if (i != 1 & h != 0.5) {
        par (mar = c (2, 1, 1, 1))
      } else if (i != 1 & h == 0.5) {
        par (mar = c (5, 1, 1, 1))
      }
    }
    
    # Select only relevant data and add day of year column
    tempData <- ringWidths %>% 
      filter (treeId == i, sampleHeight == h) %>% 
      select (sampleDate, RWI2018) %>%
      mutate (doy = lubridate::yday (sampleDate)) 
    
    # determine treatment
    t <- ifelse (i <= 10, ifelse (i <= 5, 5, 4), 1) 
    
    # Assume that the ring measured in 2019 is the end of the year growth
    if (sum (tempData [['sampleDate']] == as_date ('2019-10-24'), na.rm = TRUE) > 0) 
    {
      tempData [['sampleDate']] [tempData [['sampleDate']] == as_date ('2019-10-24')] <- as_date ('2018-11-30')
      tempData [['doy']] [tempData [['sampleDate']] == as_date ('2018-11-30')] <- yday ('2018-11-30')
    }
    
    # Fit general additive model to growth data
    fit.gam <- scam::scam (RWI2018 ~ s (doy, k = 8, bs = 'mpi'), 
                           data   = tempData, 
                           family = quasipoisson)
    
    # Plot the growth index measurement and the fitted growth curve
    if (PLOT) { 
      plot (x = tempData [['doy']],
            y = tempData [['RWI2018']],
            xlim = c (0, 365), las = 1, 
            xlab = ifelse (h == 0.5, 'day of year',''), 
            ylab = ifelse (i == 1, 'radial growth index', ''), 
            yaxt = ifelse (i == 1, 't','n'),
            pch = 19, col = tColours [['colour']] [t], ylim = c (0, 2.3)#,
            )#main = paste ('Tree',i,' height',h))
    
      lines (x = 1:365, y = exp (predict (fit.gam, newdata = data.frame (doy = 1:365))),
             col = tColours [['colour']] [t])
      
      # plot treatment period
      sDoy <- lubridate::yday (criticalDates (group = t, startOnly = TRUE))
      eDoy <- lubridate::yday (criticalDates (group = t, endOnly = TRUE))
      abline (v = c (sDoy, eDoy), col = '#99999999') 
    }
    
    # get the maximal value of the GAM for the year (to correct the predicted values to 
    # growth fractions) 
    #------------------------------------------------------------------------------------
    maxRWIfit <- max (exp (predict (fit.gam, newdata = data.frame (doy = 1:365))))
    
    # determine date when 5% of growth had occured 
    # within a 0.1% error on the fraction growth
    #------------------------------------------------------------------------------------
    error <- 10000 # set error to a large number to start with
    iDoy <- 150 # start halfway through the year
    while (error < -0.001 | error > 0.001) {
      growthFraction <- exp (predict (fit.gam, newdata = data.frame (doy = iDoy))) / maxRWIfit
      if (growthFraction > threshold) {
        if (iDoy != 150) {
          if (direction == 'later') break
        }
        iDoy <- iDoy - 1
        direction <- 'earlier'
      } else {
        if (iDoy != 150) {
          if (direction == 'earlier') break
        }
        iDoy <- iDoy + 1
        direction <- 'later'
      }
      error <- growthFraction - threshold
    }
   
    # Save start of growth
    growingSeasonDates [['startOfGrowth']] [growingSeasonDates [['treeId']] == i & growingSeasonDates [['sampleHeight']] == h] <- iDoy
    
    # plot 5% of volume growth had occured
    if (PLOT) points (x = iDoy, y = 2.25, pch = 5)
    
    # determine date when 95% of growth had occured 
    # within a 0.1% error on the fraction growth
    #------------------------------------------------------------------------------------
    error <- 10000 # set error to a large number to start with
    iDoy <- 180 # start halfway through the year
    while (error < -0.001 | error > 0.001) {
      growthFraction <- exp (predict (fit.gam, newdata = data.frame (doy = iDoy))) / maxRWIfit
      if (growthFraction > (1.0 - threshold)) {
        if (iDoy != 180) {
          if (direction == 'later') break
        }
        iDoy <- iDoy - 1
        direction <- 'earlier'
      } else {
        if (iDoy != 180) {
          if (direction == 'earlier') break
        }
        iDoy <- iDoy + 1
        direction <- 'later'
      }
      error <- growthFraction - (1.0 - threshold)
    }
    
    # Save start of growth
    growingSeasonDates [['endOfGrowth']] [growingSeasonDates [['treeId']] == i & growingSeasonDates [['sampleHeight']] == h] <- iDoy
    
    # Save the maximum value of the fitted 
    growingSeasonDates [['maxRWIfit']] [growingSeasonDates [['treeId']] == i & growingSeasonDates [['sampleHeight']] == h] <- maxRWIfit
    
    # plot 5% of volume growth had occured
    if (PLOT) points (x = iDoy, y = 2.25, pch = 5)
  }
}
if (PLOT) dev.off ()

# determine growing season dates and plot growth over time for compressed trees
#----------------------------------------------------------------------------------------
if (PLOT) {
  png (file = './fig/woodGrowthOverTimeCompressedTrees.png', width = 1255, height = 622)
  layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
          heights = c (1, 1, 1, 1.3))
}
# Loop over heights
for (h in c (4.0, 2.5, 1.5, 0.5)) {
  # Loop over trees
  for (i in 6:10) 
  {
    
    # Determine plot margins
    if (PLOT) {
      if (i == 6 & h != 0.5) {
        par (mar = c (2, 5, 1, 1))
      } else if (i == 6 & h == 0.5) {
        par (mar = c (5, 5, 1, 1))
      } else if (i != 6 & h != 0.5) {
        par (mar = c (2, 1, 1, 1))
      } else if (i != 6 & h == 0.5) {
        par (mar = c (5, 1, 1, 1))
      }
    }
    
    # Select only relevant data and add day of year column
    tempData <- ringWidths %>% 
      filter (treeId == i, sampleHeight == h) %>% 
      select (sampleDate, RWI2018) %>%
      mutate (doy = lubridate::yday (sampleDate)) 
    
    # determine treatment
    t <- ifelse (i <= 10, ifelse (i <= 5, 5, 4), 1) 
    
    # Assume that the ring measured in 2019 is the end of the year growth
    if (sum (tempData [['sampleDate']] == as_date ('2019-10-24'), na.rm = TRUE) > 0) 
    {
      tempData [['sampleDate']] [tempData [['sampleDate']] == as_date ('2019-10-24')] <- as_date ('2018-11-30')
      tempData [['doy']] [tempData [['sampleDate']] == as_date ('2018-11-30')] <- yday ('2018-11-30')
    }
    
    # Fit general additive model to growth data
    fit.gam <- scam::scam (RWI2018 ~ s (doy, k = 8, bs = 'mpi'), 
                           data   = tempData, 
                           family = quasipoisson)
    
    # Plot the growth index measurement and the fitted growth curve
    if (PLOT) { 
      plot (x = tempData [['doy']],
            y = tempData [['RWI2018']],
            xlim = c (0, 365), las = 1, 
            xlab = ifelse (h == 0.5, 'day of year',''), 
            ylab = ifelse (i == 6, 'radial growth index', ''), 
            yaxt = ifelse (i == 6, 't','n'),
            pch = 19, col = tColours [['colour']] [t], ylim = c (0, 2.3)#,
      )#main = paste ('Tree',i,' height',h))
      
      lines (x = 1:365, y = exp (predict (fit.gam, newdata = data.frame (doy = 1:365))),
             col = tColours [['colour']] [t])
      
      # plot treatment period
      sDoy <- lubridate::yday (criticalDates (group = t, startOnly = TRUE))
      eDoy <- lubridate::yday (criticalDates (group = t, endOnly = TRUE))
      abline (v = c (sDoy, eDoy), col = '#99999999') 
    }
    
    # get the maximal value of the GAM for the year (to correct the predicted values to 
    # growth fractions) 
    #------------------------------------------------------------------------------------
    maxRWIfit <- max (exp (predict (fit.gam, newdata = data.frame (doy = 1:365))))
    
    # determine date when 5% of growth had occured 
    # within a 0.1% error on the fraction growth
    #------------------------------------------------------------------------------------
    error <- 10000 # set error to a large number to start with
    iDoy <- 150 # start halfway through the year
    while (error < -0.001 | error > 0.001) {
      growthFraction <- exp (predict (fit.gam, newdata = data.frame (doy = iDoy))) / maxRWIfit
      if (growthFraction > threshold) {
        if (iDoy != 150) {
          if (direction == 'later') break
        }
        iDoy <- iDoy - 1
        direction <- 'earlier'
      } else {
        if (iDoy != 150) {
          if (direction == 'earlier') break
        }
        iDoy <- iDoy + 1
        direction <- 'later'
      }
      error <- growthFraction - threshold
    }
    
    # Save start of growth
    growingSeasonDates [['startOfGrowth']] [growingSeasonDates [['treeId']] == i & growingSeasonDates [['sampleHeight']] == h] <- iDoy
    
    # plot 5% of volume growth had occured
    if (PLOT) points (x = iDoy, y = 2.25, pch = 5)
    
    # determine date when 95% of growth had occured 
    # within a 0.1% error on the fraction growth
    #------------------------------------------------------------------------------------
    error <- 10000 # set error to a large number to start with
    iDoy <- 180 # start halfway through the year
    while (error < -0.001 | error > 0.001) {
      growthFraction <- exp (predict (fit.gam, newdata = data.frame (doy = iDoy))) / maxRWIfit
      if (growthFraction > (1.0 - threshold)) {
        if (iDoy != 180) {
          if (direction == 'later') break
        }
        iDoy <- iDoy - 1
        direction <- 'earlier'
      } else {
        if (iDoy != 180) {
          if (direction == 'earlier') break
        }
        iDoy <- iDoy + 1
        direction <- 'later'
      }
      error <- growthFraction - (1.0 - threshold)
    }
    
    # Save start of growth
    growingSeasonDates [['endOfGrowth']] [growingSeasonDates [['treeId']] == i & growingSeasonDates [['sampleHeight']] == h] <- iDoy
    
    # Save the maximum value of the fitted 
    growingSeasonDates [['maxRWIfit']] [growingSeasonDates [['treeId']] == i & growingSeasonDates [['sampleHeight']] == h] <- maxRWIfit
    
    # plot 5% of volume growth had occured
    if (PLOT) points (x = iDoy, y = 2.25, pch = 5)
  }
}
if (PLOT) dev.off ()

# determine growing season dates and plot growth over time for control trees
#----------------------------------------------------------------------------------------
if (PLOT) {
  png (file = './fig/woodGrowthOverTimeControlTrees.png', width = 1255, height = 622)
  layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
          heights = c (1, 1, 1, 1.3))
}
# Loop over heights
for (h in c (4.0, 2.5, 1.5, 0.5)) {
  # Loop over trees
  for (i in 11:15) 
  {
    
    # Determine plot margins
    if (PLOT) {
      if (i == 11 & h != 0.5) {
        par (mar = c (2, 5, 1, 1))
      } else if (i == 11 & h == 0.5) {
        par (mar = c (5, 5, 1, 1))
      } else if (i != 11 & h != 0.5) {
        par (mar = c (2, 1, 1, 1))
      } else if (i != 11 & h == 0.5) {
        par (mar = c (5, 1, 1, 1))
      }
    }
    
    # Select only relevant data and add day of year column
    tempData <- ringWidths %>% 
      filter (treeId == i, sampleHeight == h) %>% 
      select (sampleDate, RWI2018) %>%
      mutate (doy = lubridate::yday (sampleDate)) 
    
    # determine treatment
    t <- ifelse (i <= 10, ifelse (i <= 5, 5, 4), 1) 
    
    # Assume that the ring measured in 2019 is the end of the year growth
    if (sum (tempData [['sampleDate']] == as_date ('2019-10-24'), na.rm = TRUE) > 0) 
    {
      tempData [['sampleDate']] [tempData [['sampleDate']] == as_date ('2019-10-24')] <- as_date ('2018-11-30')
      tempData [['doy']] [tempData [['sampleDate']] == as_date ('2018-11-30')] <- yday ('2018-11-30')
    }
    
    # Fit general additive model to growth data
    fit.gam <- scam::scam (RWI2018 ~ s (doy, k = 8, bs = 'mpi'), 
                           data   = tempData, 
                           family = quasipoisson)
    
    # Plot the growth index measurement and the fitted growth curve
    if (PLOT) { 
      plot (x = tempData [['doy']],
            y = tempData [['RWI2018']],
            xlim = c (0, 365), las = 1, 
            xlab = ifelse (h == 0.5, 'day of year',''), 
            ylab = ifelse (i == 11, 'radial growth index', ''), 
            yaxt = ifelse (i == 11, 't','n'),
            pch = 19, col = tColours [['colour']] [t], ylim = c (0, 2.3)#,
      )#main = paste ('Tree',i,' height',h))
      
      lines (x = 1:365, y = exp (predict (fit.gam, newdata = data.frame (doy = 1:365))),
             col = tColours [['colour']] [t])
      
      # plot treatment period
      sDoy <- lubridate::yday (criticalDates (group = t, startOnly = TRUE))
      abline (v = sDoy, col = '#99999999') 
    }
    
    # get the maximal value of the GAM for the year (to correct the predicted values to 
    # growth fractions) 
    #------------------------------------------------------------------------------------
    maxRWIfit <- max (exp (predict (fit.gam, newdata = data.frame (doy = 1:365))))
    
    # determine date when 5% of growth had occured 
    # within a 0.1% error on the fraction growth
    #------------------------------------------------------------------------------------
    error <- 10000 # set error to a large number to start with
    iDoy <- 150 # start halfway through the year
    while (error < -0.001 | error > 0.001) {
      growthFraction <- exp (predict (fit.gam, newdata = data.frame (doy = iDoy))) / maxRWIfit
      if (growthFraction > threshold) {
        if (iDoy != 150) {
          if (direction == 'later') break
        }
        iDoy <- iDoy - 1
        direction <- 'earlier'
      } else {
        if (iDoy != 150) {
          if (direction == 'earlier') break
        }
        iDoy <- iDoy + 1
        direction <- 'later'
      }
      error <- growthFraction - threshold
    }
    
    # Save start of growth
    growingSeasonDates [['startOfGrowth']] [growingSeasonDates [['treeId']] == i & growingSeasonDates [['sampleHeight']] == h] <- iDoy
    
    # plot 5% of volume growth had occured
    if (PLOT) points (x = iDoy, y = 2.25, pch = 5)
    
    # determine date when 95% of growth had occured 
    # within a 0.1% error on the fraction growth
    #------------------------------------------------------------------------------------
    error <- 10000 # set error to a large number to start with
    iDoy <- 180 # start halfway through the year
    while (error < -0.001 | error > 0.001) {
      growthFraction <- exp (predict (fit.gam, newdata = data.frame (doy = iDoy))) / maxRWIfit
      if (growthFraction > (1.0 - threshold)) {
        if (iDoy != 180) {
          if (direction == 'later') break
        }
        iDoy <- iDoy - 1
        direction <- 'earlier'
      } else {
        if (iDoy != 180) {
          if (direction == 'earlier') break
        }
        iDoy <- iDoy + 1
        direction <- 'later'
      }
      error <- growthFraction - (1.0 - threshold)
    }
    
    # Save start of growth
    growingSeasonDates [['endOfGrowth']] [growingSeasonDates [['treeId']] == i & growingSeasonDates [['sampleHeight']] == h] <- iDoy
    
    # Save the maximum value of the fitted 
    growingSeasonDates [['maxRWIfit']] [growingSeasonDates [['treeId']] == i & growingSeasonDates [['sampleHeight']] == h] <- maxRWIfit
    
    # plot 5% of volume growth had occured
    if (PLOT) points (x = iDoy, y = 2.25, pch = 5)
  }
}
if (PLOT) dev.off ()
  
# clean up
#----------------------------------------------------------------------------------------
rm (PLOT, fit.gam, ringWidths, tempData, tColours, sColours, direction, eDoy, error, 
    growthFraction, h, i, iDoy, maxRWIfit, sDoy, t, threshold)
#========================================================================================