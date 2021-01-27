# script to fit general additive model to ring width data from the 2018 experiment

# start from clean slate
rm (list = ls ())

# load dependencies
library ('mgcv')
library ('easynls')
source ('readRingWidths.R')
library ('scam')

# plot growth over time for chilled trees
layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1.3))
# Loop over heights
for (h in c (4.0, 2.5, 1.5, 0.5)) 
{
  # Loop over trees
  for (i in 1:5) 
  {
    
    # Determine plot margins
    if (i == 1 & h != 0.5) {
      par (mar = c (2, 5, 1, 1))
    } else if (i == 1 & h == 0.5) {
      par (mar = c (5, 5, 1, 1))
    } else if (i != 1 & h != 0.5) {
      par (mar = c (2, 1, 1, 1))
    } else if (i != 1 & h == 0.5) {
      par (mar = c (5, 1, 1, 1))
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
      tempData [['sampleDate']] [tempData [['sampleDate']] == as_date ('2019-10-24')] <- as_date ('2018-12-31')
      tempData [['doy']] [tempData [['sampleDate']] == as_date ('2018-12-31')] <- yday ('2018-12-31')
    }
    
    # Fit general additive model to growth data
    fit.gam <- scam (RWI2018 ~ s (doy, k = 8, bs = 'mpi'), 
                     data = tempData, 
                     family = quasipoisson)
    
    # Plot the growth index measurement and the fitted growth curve
    plot (x = tempData [['doy']],
          y = tempData [['RWI2018']],
          xlim = c (0, 365), las = 1, 
          xlab = ifelse (h == 0.5, 'day of year',''), 
          ylab = ifelse (i == 1, 'radial growth index', ''), 
          yaxt = ifelse (i == 1, 't','n'),
          pch = 19, col = tColours [['colour']] [t], ylim = c (0, 2.3)#,
          )#main = paste ('Tree',i,' height',h))
    
    lines (x = 1:365, y = exp (predict (fit.gam, newdata = data.frame (doy = c (1:365)))),
           col = tColours [['colour']] [t])
    
    # plot treatment period
    sDoy <- lubridate::yday (criticalDates (group = t, startOnly = TRUE))
    eDoy <- lubridate::yday (criticalDates (group = t, endOnly = TRUE))
    abline (v = c (sDoy, eDoy), col = '#99999999') 
  }
}

# plot growth over time for compressed trees
layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1.3))
# Loop over heights
for (h in c (4.0, 2.5, 1.5, 0.5)) 
{
  # Loop over trees
  for (i in 6:10) 
  {
    
    # Determine plot margins
    if (i == 6 & h != 0.5) {
      par (mar = c (2, 5, 1, 1))
    } else if (i == 6 & h == 0.5) {
      par (mar = c (5, 5, 1, 1))
    } else if (i != 6 & h != 0.5) {
      par (mar = c (2, 1, 1, 1))
    } else if (i != 6 & h == 0.5) {
      par (mar = c (5, 1, 1, 1))
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
      tempData [['sampleDate']] [tempData [['sampleDate']] == as_date ('2019-10-24')] <- as_date ('2018-12-31')
      tempData [['doy']] [tempData [['sampleDate']] == as_date ('2018-12-31')] <- yday ('2018-12-31')
    }
    
    # Fit general additive model to growth data
    fit.gam <- scam (RWI2018 ~ s (doy, k = 8, bs = 'mpi'), 
                     data = tempData, 
                     family = quasipoisson)
    
    # Plot the growth index measurement and the fitted growth curve
    plot (x = tempData [['doy']],
          y = tempData [['RWI2018']],
          xlim = c (0, 365), las = 1, 
          xlab = ifelse (h == 0.5, 'day of year',''), 
          ylab = ifelse (i == 6, 'radial growth index', ''), 
          yaxt = ifelse (i == 6, 't','n'),
          pch = 19, col = tColours [['colour']] [t], ylim = c (0, 2.3)#,
    )#main = paste ('Tree',i,' height',h))
    
    lines (x = 1:365, y = exp (predict (fit.gam, newdata = data.frame (doy = c (1:365)))),
           col = tColours [['colour']] [t])
    
    # plot treatment period
    sDoy <- lubridate::yday (criticalDates (group = t, startOnly = TRUE))
    eDoy <- lubridate::yday (criticalDates (group = t, endOnly = TRUE))
    abline (v = c (sDoy, eDoy), col = '#99999999') 
  }
}

# plot growth over time for compressed trees
layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1.3))
# Loop over heights
for (h in c (4.0, 2.5, 1.5, 0.5)) 
{
  # Loop over trees
  for (i in 11:15) 
  {
    
    # Determine plot margins
    if (i == 11 & h != 0.5) {
      par (mar = c (2, 5, 1, 1))
    } else if (i == 11 & h == 0.5) {
      par (mar = c (5, 5, 1, 1))
    } else if (i != 11 & h != 0.5) {
      par (mar = c (2, 1, 1, 1))
    } else if (i != 11 & h == 0.5) {
      par (mar = c (5, 1, 1, 1))
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
      tempData [['sampleDate']] [tempData [['sampleDate']] == as_date ('2019-10-24')] <- as_date ('2018-12-31')
      tempData [['doy']] [tempData [['sampleDate']] == as_date ('2018-12-31')] <- yday ('2018-12-31')
    }
    
    # Fit general additive model to growth data
    fit.gam <- scam (RWI2018 ~ s (doy, k = 8, bs = 'mpi'), 
                     data = tempData, 
                     family = quasipoisson)
    
    # Plot the growth index measurement and the fitted growth curve
    plot (x = tempData [['doy']],
          y = tempData [['RWI2018']],
          xlim = c (0, 365), las = 1, 
          xlab = ifelse (h == 0.5, 'day of year',''), 
          ylab = ifelse (i == 11, 'radial growth index', ''), 
          yaxt = ifelse (i == 11, 't','n'),
          pch = 19, col = tColours [['colour']] [t], ylim = c (0, 2.3)#,
    )#main = paste ('Tree',i,' height',h))
    
    lines (x = 1:365, y = exp (predict (fit.gam, newdata = data.frame (doy = c (1:365)))),
           col = tColours [['colour']] [t])
    
    # plot treatment period
    sDoy <- lubridate::yday (criticalDates (group = t, startOnly = TRUE))
    if (t != 1) eDoy <- lubridate::yday (criticalDates (group = t, endOnly = TRUE))
    abline (v = ifelse (t != 1, c (sDoy, eDoy), sDoy), col = '#99999999', lty = 2) 
  }
}
