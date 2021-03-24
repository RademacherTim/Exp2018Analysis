#========================================================================================
# This script plot median cell size variations between treatments and heights
#----------------------------------------------------------------------------------------

# Source anatomical data
#----------------------------------------------------------------------------------------
source ('processAnatomicalData.R')
source ('plotingFunctions.R')

# Plot cell-wall thickness by period of formation
#----------------------------------------------------------------------------------------
layout (matrix (1:4), height = c (1,1,1,1.3))
par (mar = c (1, 5, 1, 1))
tempData <- anatomicalData %>% filter (YEAR == 2018, PLOT  %in% c (5, 1))
plot (x = tempData [['RRADDISTR']] [tempData [['sampleHeight']] == 4.0 & tempData [['PLOT']]== 5],
      y = tempData [['cellRadWidth']] [tempData [['sampleHeight']] == 4.0 & tempData [['PLOT']]== 5],
      col = addOpacity (tColours [['colour']] [5], 0.5), pch = 5, xlab = '', 
      ylab = expression (paste ('radial cell diameter (',mu, m,')', sep = '')),
      xlim = c (0, 100), ylim = c (0, 75), axes = FALSE)
points (x = tempData [['RRADDISTR']] [tempData [['sampleHeight']] == 4.0 & tempData [['PLOT']] == 1],
        y = tempData [['cellRadWidth']] [tempData [['sampleHeight']] == 4.0 & tempData [['PLOT']] == 1],
        col = addOpacity (tColours [['colour']] [1], 0.3), pch = 19)  
axis (side = 1, at = seq (0, 100, 10), labels = rep ('', 11))
axis (side = 2, at = seq (0, 70, 10), las = 1)
# Add the average portion formed before, during and after the experiment

# Add panel for 2.5 m
par (mar = c (1,5,1,1))
tempData <- anatomicalData %>% filter (YEAR == 2018, PLOT  %in% c (5, 1))
plot (x = tempData [['RRADDISTR']] [tempData [['sampleHeight']] == 2.5 & tempData [['PLOT']]== 5],
      y = tempData [['cellRadWidth']] [tempData [['sampleHeight']] == 2.5 & tempData [['PLOT']]== 5],
      col = addOpacity (tColours [['colour']] [5], 0.5), pch = 5, xlab = '', 
      ylab = expression (paste ('radial cell diameter (',mu, m,')', sep = '')),
      xlim = c (0, 100), ylim = c (0, 75), axes = FALSE)
points (x = tempData [['RRADDISTR']] [tempData [['sampleHeight']] == 2.5 & tempData [['PLOT']] == 1],
        y = tempData [['cellRadWidth']] [tempData [['sampleHeight']] == 2.5 & tempData [['PLOT']] == 1],
        col = addOpacity (tColours [['colour']] [1], 0.3), pch = 19)  
axis (side = 1, at = seq (0, 100, 10), labels = rep ('', 11))
axis (side = 2, at = seq (0, 70, 10), las = 1)

# Add panel for 1.5 m
par (mar = c (1, 5, 1, 1))
tempData <- anatomicalData %>% filter (YEAR == 2018, PLOT  %in% c (5, 1))
plot (x = tempData [['RRADDISTR']] [tempData [['sampleHeight']] == 1.5 & tempData [['PLOT']]== 5],
      y = tempData [['cellRadWidth']] [tempData [['sampleHeight']] == 1.5 & tempData [['PLOT']]== 5],
      col = addOpacity (tColours [['colour']] [5], 0.5), pch = 5, xlab = '', 
      ylab = expression (paste ('radial cell diameter (',mu, m,')', sep = '')),
      xlim = c (0, 100), ylim = c (0, 75), axes = FALSE)
points (x = tempData [['RRADDISTR']] [tempData [['sampleHeight']] == 1.5 & tempData [['PLOT']] == 1],
        y = tempData [['cellRadWidth']] [tempData [['sampleHeight']] == 1.5 & tempData [['PLOT']] == 1],
        col = addOpacity (tColours [['colour']] [1], 0.3), pch = 19)  
axis (side = 1, at = seq (0, 100, 10), labels = rep ('', 11))
axis (side = 2, at = seq (0, 70, 10), las = 1)

# Add panel for 1.5 m
par (mar = c (5, 5, 1, 1))
tempData <- anatomicalData %>% filter (YEAR == 2018, PLOT  %in% c (5, 1))
plot (x = tempData [['RRADDISTR']] [tempData [['sampleHeight']] == 0.5 & tempData [['PLOT']]== 5],
      y = tempData [['cellRadWidth']] [tempData [['sampleHeight']] == 0.5 & tempData [['PLOT']]== 5],
      col = addOpacity (tColours [['colour']] [5], 0.5), pch = 5, xlab = '', 
      ylab = expression (paste ('radial cell diameter (',mu, m,')', sep = '')),
      xlim = c (0, 100), ylim = c (0, 75), axes = FALSE)
points (x = tempData [['RRADDISTR']] [tempData [['sampleHeight']] == 0.5 & tempData [['PLOT']] == 1],
        y = tempData [['cellRadWidth']] [tempData [['sampleHeight']] == 0.5 & tempData [['PLOT']] == 1],
        col = addOpacity (tColours [['colour']] [1], 0.3), pch = 19)  
axis (side = 1, at = seq (0, 100, 10))
axis (side = 2, at = seq (0, 70, 10), las = 1)

# Add column
legend (x = 75, y = 70, legend = 'control', pch = 19, bg = 'transparent',
        col = addOpacity (tColours [['colour']] [1], 0.5), box.lty = 0)
legend (x = 90, y = 70, legend = 'chilled', pch = 5,  bg = 'transparent',
        col = addOpacity (tColours [['colour']] [5], 0.5), box.lty = 0)
#========================================================================================
