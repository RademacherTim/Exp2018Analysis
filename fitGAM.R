# script to fit general additive model to ring width data from the 2018 experiment

# start from clean slate
rm (list = ls ())

# load dependencies
library ('mgcv')
library ('easynls')
source ('readRingWidths.R')

# Loop over trees
for (i in 1:15) 
{
  
  # Loop over heights
  for (h in c (0.5, 1.5, 2.5, 4.0)) 
  {
    
    # Select only relevant data
    tempData <- ringWidths %>% 
      filter (treeId == i, sampleHeight == h) %>% 
      select (sampleDate, RWI2018) %>%
      mutate (doy = lubridate::yday (sampleDate)) 
    
    # Assume that the ring measured in 2019 is the end of the year growth
    if (sum (tempData [['sampleDate']] == as_date ('2019-10-24'), na.rm = TRUE) > 0) 
    {
      tempData [['sampleDate']] [tempData [['sampleDate']] == as_date ('2019-10-24')] <- as_date ('2018-12-31')
      tempData [['doy']] [tempData [['sampleDate']] == as_date ('2018-12-31')] <- yday ('2018-12-31')
    }
    
    # Fit general additive model to growth data
    #fit.gam <- gam (RWI2018 ~ s (doy), data = tempData, family = quasipoisson)
    
    # Fit gompertz function
    #fit.gompertz <- nlsfit (tempData [, 2:3], model = 10, start = c (a = 100, b = 1, c = 1))
    
    # Plot the growth index measurement and the fitted growth curve
    plot (x = tempData [['doy']],
          y = tempData [['RWI2018']],
          xlim = c (0, 365), las = 1, 
          xlab = 'day of year', ylab = 'radial growth index', 
          pch = 19, col = tColours [['colour']] [1])
    
    # lines (x = 1:365, y = exp (predict (fit.gam, newdata = data.frame (doy = c (1:365)))),
    #        col = tColours [['colour']] [1])
    lines (x = 1:365, y = )
  }
}