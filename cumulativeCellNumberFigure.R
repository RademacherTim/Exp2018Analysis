#========================================================================================
# This script plot cumulative cell number across treatments and heights
#----------------------------------------------------------------------------------------

# Source anatomical data and dependencies
#----------------------------------------------------------------------------------------
READ <- FALSE; if (READ) source ('processAnatomicalData.R')
if (!exists ('tColours')) source ('plotingFunctions.R')
if (!existsFunction ('rollmean')) library ('zoo')

# Function to plot fraction formed at critical dates (e.g., before, during, and after)
#----------------------------------------------------------------------------------------
plotFractionFormed <- function (data) {
  colour <- '#999999'
  FF <- data %>% filter (PLOT == 5, period <= as_date ('2018-06-25')) %>% group_by (TREE) %>% 
    summarise (maxFF = max (RRADDISTR, na.rm = TRUE)) 
  rect (ytop = 1500, ybottom = -100, 
        xleft  = mean (FF [['maxFF']]) - se (FF [['maxFF']]),
        xright = mean (FF [['maxFF']]) + se (FF [['maxFF']]),
        col = addOpacity (colour, 0.3), lty = 0)
  abline (v = mean (FF [['maxFF']]), col = colour, lty = 2, lwd = 2)
  FF <- data %>% filter (PLOT == 1, period <= as_date ('2018-06-25')) %>% group_by (TREE) %>% 
    summarise (maxFF = max (RRADDISTR, na.rm = TRUE)) 
  rect (ytop = 1500, ybottom = -100, 
        xleft  = mean (FF [['maxFF']]) - se (FF [['maxFF']]),
        xright = mean (FF [['maxFF']]) + se (FF [['maxFF']]),
        col = addOpacity (colour, 0.3), lty = 0)
  abline (v = mean (FF [['maxFF']]), col = colour, lty = 1, lwd = 2)
  FF <- data %>% filter (PLOT == 5, period <= as_date ('2018-09-03')) %>% group_by (TREE) %>% 
    summarise (maxFF = max (RRADDISTR, na.rm = TRUE)) 
  rect (ytop = 1500, ybottom = -100, 
        xleft  = mean (FF [['maxFF']]) - se (FF [['maxFF']]),
        xright = mean (FF [['maxFF']]) + se (FF [['maxFF']]),
        col = addOpacity (colour, 0.3), lty = 0)
  abline (v = mean (FF [['maxFF']]), col = colour, lty = 2, lwd = 2)
  FF <- data %>% filter (PLOT == 1, period <= as_date ('2018-09-03')) %>% group_by (TREE) %>% 
    summarise (maxFF = max (RRADDISTR, na.rm = TRUE)) 
  rect (ytop = 1500, ybottom = -100, 
        xleft  = mean (FF [['maxFF']]) - se (FF [['maxFF']]),
        xright = mean (FF [['maxFF']]) + se (FF [['maxFF']]),
        col = addOpacity (colour, 0.3), lty = 0)
  abline (v = mean (FF [['maxFF']]), col = colour, lty = 1, lwd = 2)
}

# Function to plot running mean and se for each 
#----------------------------------------------------------------------------------------
plotRunningAverage <- function (data) {
  
  options (warn = -1)
  binnedData <- data %>% mutate (bin = cut (RRADDISTR, seq (0, 100, 5)))
  meanNCells <- binnedData %>% filter (PLOT == 1) %>% 
    group_by (bin) %>% summarise (meanNCells = mean (cumNCells, na.rm = TRUE),
                                  seNCells   = se (cumNCells))
  polygon (x = c (seq (0.5, 99.5, 5), 
                  seq (99.5, 0.5, -5)),
           y = c (meanNCells [['meanNCells']] + meanNCells [['seNCells']], 
                  rev (meanNCells [['meanNCells']] - meanNCells [['seNCells']])),
           lty = 0, col = addOpacity (tColours [['colour']] [1], 0.5))
  lines (x = seq (0.5, 99.5, 5), 
         #y = rollmean (meanNCells [['meanNCells']], k = 5, na.pad = TRUE), 
         y = meanNCells [['meanNCells']],
         col = tColours [['colour']] [1], lwd  = 2)
  meanNCells <- binnedData %>% filter (PLOT == 5) %>% 
    group_by (bin) %>% summarise (meanNCells = mean (cumNCells, na.rm = TRUE),
                                  seNCells   = se (cumNCells))
  meanNCells <- meanNCells [!is.na (meanNCells [['bin']]), ]
  indices <- which (is.na (meanNCells [['seNCells']]))
  meanNCells [['seNCells']] [indices] <- rowMeans (cbind (meanNCells [['seNCells']] [indices-1], 
                                                              meanNCells [['seNCells']] [indices+1]))
  polygon (x = c (seq (0.5, 99.5, 5), 
                  seq (99.5, 0.5, -5)),
           y = c (meanNCells [['meanNCells']] + meanNCells [['seNCells']], 
                  rev (meanNCells [['meanNCells']] - meanNCells [['seNCells']])),
           lty = 0, col = addOpacity (tColours [['colour']] [5], 0.5))
  lines (x = seq (0.5, 99.5, 5), 
         #y = rollmean (meanNCells [['meanNCells']], k = 10, na.pad = TRUE), 
         y = meanNCells [['meanNCells']],
         col = tColours [['colour']] [5], lwd = 2, lty = 2)
  options (warn = 0)
}

# Plot cell size over percentage ring width 
#----------------------------------------------------------------------------------------
#png (filename = './fig/cumulativeCellNumberOverPercentageRingWidth.png', width = 500, height = 600)
layout (matrix (1:4), height = c (1,1,1,1.3))

# loop over sampling heights
#----------------------------------------------------------------------------------------
for (h in c (4.0, 2.5, 1.5, 0.5)) {
  
  # determine panel marigns
  if  (h != 0.5) {
    par (mar = c (1, 5, 1, 1))
  } else {
    par (mar = c (5, 5, 1, 1))
  }
  
  # get relevant data 
  tempData <- anatomicalData %>% filter (YEAR == 2018, PLOT  %in% c (5, 1), sampleHeight == h)
  
  # create plot area
  plot (x = tempData [['RRADDISTR']] [tempData [['PLOT']] == 5],
        y = tempData [['cumNCells']] [tempData [['PLOT']] == 5],
        col = 'white', xlab = ifelse (h == 0.5, 'Percentage ring width (%)',''), 
        ylab = 'Cumulative cell number (n)',
        xlim = c (0, 100), ylim = c (0, 65), axes = FALSE)
  
  # Add the average portion formed before, during and after the experiment
  plotFractionFormed (data = tempData) 
  
  # Add individual points
  lines (x = tempData [['RRADDISTR']] [tempData [['PLOT']] == 5],
         y = tempData [['cumNCells']] [tempData [['PLOT']] == 5],
         col = addOpacity (tColours [['colour']] [5], 0.5), lty = 2)
  lines (x = tempData [['RRADDISTR']] [tempData [['PLOT']] == 1],
         y = tempData [['cumNCells']] [tempData [['PLOT']] == 1],
         col = addOpacity (tColours [['colour']] [1], 0.5))  
  axis (side = 1, at = seq (0, 100, 10), labels = rep ('', 11))
  axis (side = 2, at = seq (0, 60, 20), las = 1)
  
  # Add smoothed mean signal
  plotRunningAverage (data = tempData)
  
}

# Add column
legend (x = 6, y = 35, legend = c ('control','chilled'), pch = c (19,5), bg = 'transparent',
        col = addOpacity (tColours [['colour']] [c (1, 5)], 0.5), box.lty = 0)
legend (x = 0, y = 35, legend = rep ('', 2), lwd = 2, lty = 1:2,  bg = 'transparent',
        col = tColours [['colour']] [c (1, 5)], box.lty = 0)
dev.off ()
#========================================================================================
