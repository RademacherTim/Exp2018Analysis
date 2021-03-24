#========================================================================================
# This script plot median cell-wall area variations between treatments and heights
#----------------------------------------------------------------------------------------

# Source anatomical data
#----------------------------------------------------------------------------------------
source ('processAnatomicalData.R')
source ('plotingFunctions.R')

# Plot cell-wall thickness by period of formation
#----------------------------------------------------------------------------------------
layout (matrix (1:4), height = c (1,1,1,1.3))
par (mar = c (1, 5, 1, 1))
tempData <- anatomicalData %>% filter (YEAR == 2018, PLOT  %in% c (5, 1), sampleHeight == 4.0)
plot (x = tempData [['RRADDISTR']] [tempData [['PLOT']] == 5],
      y = tempData [['CWA']] [tempData [['PLOT']] == 5],
      col = 'white', xlab = '', 
      ylab = expression (paste ('cell-wall area (',mu, m^2,')', sep = '')),
      xlim = c (0, 100), ylim = c (0, 1400), axes = FALSE)

# Add the average portion formed before, during and after the experiment
FF <- tempData %>% filter (PLOT == 5, period <= as_date ('2018-06-25')) %>% group_by (TREE) %>% 
  summarise (maxFF = max (RRADDISTR, na.rm = TRUE)) 
rect (ytop = 1500, ybottom = -100, 
      xleft  = mean (FF [['maxFF']]) - se (FF [['maxFF']]),
      xright = mean (FF [['maxFF']]) + se (FF [['maxFF']]),
      col = addOpacity (tColours [['colour']] [5], 0.3), lty = 0)
abline (v = mean (FF [['maxFF']]), col = tColours [['colour']] [5], lty = 3, lwd = 2)
FF <- tempData %>% filter (PLOT == 1, period <= as_date ('2018-06-25')) %>% group_by (TREE) %>% 
  summarise (maxFF = max (RRADDISTR, na.rm = TRUE)) 
rect (ytop = 1500, ybottom = -100, 
      xleft  = mean (FF [['maxFF']]) - se (FF [['maxFF']]),
      xright = mean (FF [['maxFF']]) + se (FF [['maxFF']]),
      col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
abline (v = mean (FF [['maxFF']]), col = tColours [['colour']] [1], lty = 3, lwd = 2)
FF <- tempData %>% filter (PLOT == 5, period <= as_date ('2018-09-03')) %>% group_by (TREE) %>% 
  summarise (maxFF = max (RRADDISTR, na.rm = TRUE)) 
rect (ytop = 1500, ybottom = -100, 
      xleft  = mean (FF [['maxFF']]) - se (FF [['maxFF']]),
      xright = mean (FF [['maxFF']]) + se (FF [['maxFF']]),
      col = addOpacity (tColours [['colour']] [5], 0.3), lty = 0)
abline (v = mean (FF [['maxFF']]), col = tColours [['colour']] [5], lty = 4, lwd = 2)
FF <- tempData %>% filter (PLOT == 1, period <= as_date ('2018-09-03')) %>% group_by (TREE) %>% 
  summarise (maxFF = max (RRADDISTR, na.rm = TRUE)) 
rect (ytop = 1500, ybottom = -100, 
      xleft  = mean (FF [['maxFF']]) - se (FF [['maxFF']]),
      xright = mean (FF [['maxFF']]) + se (FF [['maxFF']]),
      col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
abline (v = mean (FF [['maxFF']]), col = tColours [['colour']] [1], lty = 4, lwd = 2)

# Add individual points
points (x = tempData [['RRADDISTR']] [tempData [['PLOT']] == 5],
        y = tempData [['CWA']] [tempData [['PLOT']] == 5],
        col = addOpacity (tColours [['colour']] [5], 0.2), pch = 5)
points (x = tempData [['RRADDISTR']] [tempData [['PLOT']] == 1],
        y = tempData [['CWA']] [tempData [['PLOT']] == 1],
        col = addOpacity (tColours [['colour']] [1], 0.2), pch = 19)  
axis (side = 1, at = seq (0, 100, 10), labels = rep ('', 11))
axis (side = 2, at = seq (0, 1400, 400), las = 1)

# Add smoothed mean signal
tempData <- tempData %>% mutate (bin = cut (RRADDISTR, seq (0, 100, 1)))
meanCWA <- tempData %>% filter (PLOT == 1) %>% 
  group_by (bin) %>% summarise (meanCWA = mean (CWA, na.rm = TRUE))
lines (x = seq (0.5, 99.5, 1), 
       y = rollmean (meanCWA [['meanCWA']], k = 10, na.pad = TRUE), 
       col = tColours [['colour']] [1], lwd  = 2)
meanCWA <- tempData %>% filter (PLOT == 5) %>% 
  group_by (bin) %>% summarise (meanCWA = mean (CWA, na.rm = TRUE)) 
lines (x = seq (0.5, 99.5, 1), 
       y = rollmean (meanCWA [['meanCWA']], k = 10, na.pad = TRUE), 
       col = tColours [['colour']] [5], lwd = 2, lty = 2)

# Add panel for 2.5 m
par (mar = c (1,5,1,1))
tempData <- anatomicalData %>% filter (YEAR == 2018, PLOT  %in% c (5, 1))
plot (x = tempData [['RRADDISTR']] [tempData [['sampleHeight']] == 2.5 & tempData [['PLOT']]== 5],
      y = tempData [['CWA']] [tempData [['sampleHeight']] == 2.5 & tempData [['PLOT']]== 5],
      col = addOpacity (tColours [['colour']] [5], 0.5), pch = 5, xlab = '', 
      ylab = expression (paste ('cell-wall area (',mu, m^2,')', sep = '')),
      xlim = c (0, 100), ylim = c (0, 1400), axes = FALSE)
points (x = tempData [['RRADDISTR']] [tempData [['sampleHeight']] == 2.5 & tempData [['PLOT']] == 1],
        y = tempData [['CWA']] [tempData [['sampleHeight']] == 2.5 & tempData [['PLOT']] == 1],
        col = addOpacity (tColours [['colour']] [1], 0.3), pch = 19)  
axis (side = 1, at = seq (0, 100, 10), labels = rep ('', 11))
axis (side = 2, at = seq (0, 1400, 400), las = 1)

# Add panel for 1.5 m
par (mar = c (1, 5, 1, 1))
tempData <- anatomicalData %>% filter (YEAR == 2018, PLOT  %in% c (5, 1))
plot (x = tempData [['RRADDISTR']] [tempData [['sampleHeight']] == 1.5 & tempData [['PLOT']]== 5],
      y = tempData [['CWA']] [tempData [['sampleHeight']] == 1.5 & tempData [['PLOT']]== 5],
      col = addOpacity (tColours [['colour']] [5], 0.5), pch = 5, xlab = '', 
      ylab = expression (paste ('cell-wall area (',mu, m^2,')', sep = '')),
      xlim = c (0, 100), ylim = c (0, 1400), axes = FALSE)
points (x = tempData [['RRADDISTR']] [tempData [['sampleHeight']] == 1.5 & tempData [['PLOT']] == 1],
        y = tempData [['CWA']] [tempData [['sampleHeight']] == 1.5 & tempData [['PLOT']] == 1],
        col = addOpacity (tColours [['colour']] [1], 0.3), pch = 19)  
axis (side = 1, at = seq (0, 100, 10), labels = rep ('', 11))
axis (side = 2, at = seq (0, 1400, 400), las = 1)

# Add panel for 1.5 m
par (mar = c (5, 5, 1, 1))
tempData <- anatomicalData %>% filter (YEAR == 2018, PLOT  %in% c (5, 1))
plot (x = tempData [['RRADDISTR']] [tempData [['sampleHeight']] == 0.5 & tempData [['PLOT']]== 5],
      y = tempData [['CWA']] [tempData [['sampleHeight']] == 0.5 & tempData [['PLOT']]== 5],
      col = addOpacity (tColours [['colour']] [5], 0.5), pch = 5, xlab = '', 
      ylab = expression (paste ('cell-wall area (',mu, m^2,')', sep = '')),
      xlim = c (0, 100), ylim = c (0, 1400), axes = FALSE)
points (x = tempData [['RRADDISTR']] [tempData [['sampleHeight']] == 0.5 & tempData [['PLOT']] == 1],
        y = tempData [['CWA']] [tempData [['sampleHeight']] == 0.5 & tempData [['PLOT']] == 1],
        col = addOpacity (tColours [['colour']] [1], 0.3), pch = 19)  
axis (side = 1, at = seq (0, 100, 10))
axis (side = 2, at = seq (0, 1400, 400), las = 1)

# Add column
legend (x = 75, y = 1350, legend = 'control', pch = 19, bg = 'transparent',
        col = addOpacity (tColours [['colour']] [1], 0.5), box.lty = 0)
legend (x = 90, y = 1350, legend = 'chilled', pch = 5,  bg = 'transparent',
        col = addOpacity (tColours [['colour']] [5], 0.5), box.lty = 0)
#========================================================================================
