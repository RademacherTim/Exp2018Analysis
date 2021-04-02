#========================================================================================
# This script plot cumulative cell-wall area across treatments and heights
#----------------------------------------------------------------------------------------

# Source anatomical data and dependencies
#----------------------------------------------------------------------------------------
READ <- FALSE; if (READ) source ('processAnatomicalData.R')
if (!exists ('tColours')) source ('plotingFunctions.R')
if (!existsFunction ('rollmean')) library ('zoo')

# Function to plot fraction formed at critical dates (e.g., before, during, and after)
#----------------------------------------------------------------------------------------
plotFractionFormed <- function (data, SHADING = FALSE) {
  colour <- '#999999'
  FF <- data %>% filter (PLOT == 5, period <= as_date ('2018-06-25')) %>% group_by (TREE) %>% 
    summarise (maxFF = max (RRADDISTR, na.rm = TRUE)) 
  if (SHADING) {
    rect (ytop = 1500, ybottom = -100, 
          xleft  = mean (FF [['maxFF']]) - se (FF [['maxFF']]),
          xright = mean (FF [['maxFF']]) + se (FF [['maxFF']]),
          col = addOpacity (colour, 0.3), lty = 0)
  }
  abline (v = mean (FF [['maxFF']]), col = colour, lty = 2, lwd = 2)
  FF <- data %>% filter (PLOT == 1, period <= as_date ('2018-06-25')) %>% group_by (TREE) %>% 
    summarise (maxFF = max (RRADDISTR, na.rm = TRUE)) 
  if (SHADING) {
    rect (ytop = 1500, ybottom = -100, 
          xleft  = mean (FF [['maxFF']]) - se (FF [['maxFF']]),
          xright = mean (FF [['maxFF']]) + se (FF [['maxFF']]),
          col = addOpacity (colour, 0.3), lty = 0)
  }
  abline (v = mean (FF [['maxFF']]), col = colour, lty = 1, lwd = 2)
  FF <- data %>% filter (PLOT == 5, period <= as_date ('2018-09-03')) %>% group_by (TREE) %>% 
    summarise (maxFF = max (RRADDISTR, na.rm = TRUE)) 
  if (SHADING) {
    rect (ytop = 1500, ybottom = -100, 
          xleft  = mean (FF [['maxFF']]) - se (FF [['maxFF']]),
          xright = mean (FF [['maxFF']]) + se (FF [['maxFF']]),
          col = addOpacity (colour, 0.3), lty = 0)
  }
  abline (v = mean (FF [['maxFF']]), col = colour, lty = 2, lwd = 2)
  FF <- data %>% filter (PLOT == 1, period <= as_date ('2018-09-03')) %>% group_by (TREE) %>% 
    summarise (maxFF = max (RRADDISTR, na.rm = TRUE)) 
  if (SHADING) {
    rect (ytop = 1500, ybottom = -100, 
          xleft  = mean (FF [['maxFF']]) - se (FF [['maxFF']]),
          xright = mean (FF [['maxFF']]) + se (FF [['maxFF']]),
          col = addOpacity (colour, 0.3), lty = 0)
  }
  abline (v = mean (FF [['maxFF']]), col = colour, lty = 1, lwd = 2)
}

# Function to plot running mean and se for each 
#----------------------------------------------------------------------------------------
plotRunningAverage <- function (data) {
  
  options (warn = -1)
  reso <- 5
  binnedData <- data %>% mutate (bin = cut (RRADDISTR, seq (0, 100, reso)))
  meanCumCWA <- binnedData %>% filter (PLOT == 1) %>% 
    group_by (bin) %>% summarise (meanCumCWA = mean (cumCWA, na.rm = TRUE),
                                  seCumCWA   = se (cumCWA))
  polygon (x = c (seq (reso/2, 100 - (reso/2), reso),
                  seq (100 - (reso/2), reso/2, -reso)),
           y = c (meanCumCWA [['meanCumCWA']] + meanCumCWA [['seCumCWA']], 
                  rev (meanCumCWA [['meanCumCWA']] - meanCumCWA [['seCumCWA']])) * 1e-6,
           lty = 0, col = addOpacity (tColours [['colour']] [1], 0.5))
  lines (x = seq (reso/2, 100 - (reso/2), reso), 
         #y = rollmean (meanCumCWA [['meanCumCWA']], k = 5, na.pad = TRUE), 
         y = meanCumCWA [['meanCumCWA']] * 1e-6,
         col = tColours [['colour']] [1], lwd  = 2)
  meanCumCWA <- binnedData %>% filter (PLOT == 5) %>% 
    group_by (bin) %>% summarise (meanCumCWA = mean (cumCWA, na.rm = TRUE),
                                  seCumCWA   = se (cumCWA))
  meanCumCWA <- meanCumCWA [!is.na (meanCumCWA [['bin']]), ]
  indices <- which (is.na (meanCumCWA [['seCumCWA']]))
  meanCumCWA [['seCumCWA']] [indices] <- rowMeans (cbind (meanCumCWA [['seCumCWA']] [indices-1], 
                                                          meanCumCWA [['seCumCWA']] [indices+1]))
  polygon (x = c (seq (reso/2, 100 - (reso/2), reso),
                  seq (100 - (reso/2), reso/2, -reso)),
           y = c (meanCumCWA [['meanCumCWA']] + meanCumCWA [['seCumCWA']], 
                  rev (meanCumCWA [['meanCumCWA']] - meanCumCWA [['seCumCWA']])) * 1e-6,
           lty = 0, col = addOpacity (tColours [['colour']] [5], 0.5))
  lines (x = seq (reso/2, 100 - (reso/2), reso),
         #y = rollmean (meanCumCWA [['meanCumCWA']], k = 10, na.pad = TRUE), 
         y = meanCumCWA [['meanCumCWA']] * 1e-6,
         col = tColours [['colour']] [5], lwd = 2, lty = 2)
  options (warn = 0)
}

# Plot cell size over percentage ring width 
#----------------------------------------------------------------------------------------
png (filename = './fig/cumulativeCWAOverPercentageRingWidth.png', width = 500, height = 700)
layout (matrix (1:6), height = c (1,1,1,1,1,1.3))

# loop over sampling heights
#----------------------------------------------------------------------------------------
for (h in c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)) {
  
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
        y = tempData [['cumCWA']] [tempData [['PLOT']] == 5] * 1e-6,
        col = 'white', xlab = ifelse (h == 0.5, 'Percentage ring width (%)',''), 
        ylab = expression (paste ('Cumulative cell-wall area (',mm^2,')', sep = '')),
        xlim = c (0, 100), ylim = c (0, 0.145), axes = FALSE)
  
  # Add the average portion formed before, during and after the experiment
  if (h %notin% 1:2) plotFractionFormed (data = tempData) 
  
  # Add individual points
  lines (x = tempData [['RRADDISTR']] [tempData [['PLOT']] == 5],
         y = tempData [['cumCWA']] [tempData [['PLOT']] == 5] * 1e-6,
         col = addOpacity (tColours [['colour']] [5], 0.5), lty = 2)
  lines (x = tempData [['RRADDISTR']] [tempData [['PLOT']] == 1],
         y = tempData [['cumCWA']] [tempData [['PLOT']] == 1] * 1e-6,
         col = addOpacity (tColours [['colour']] [1], 0.5))  
  if (h != 0.5) {
    axis (side = 1, at = seq (0, 100, 10), labels = rep ('', 11))
  } else {
    axis (side = 1, at = seq (0, 100, 10))
  }
  axis (side = 2, at = seq (0, 0.14, 0.02), las = 1)
  
  # Add smoothed mean signal
  plotRunningAverage (data = tempData)
  
}

# Add column
legend (x = 6, y = 0.14, legend = c ('control','chilled'), lwd = 1, lty = 1:2, bg = 'transparent',
        col = addOpacity (tColours [['colour']] [c (1, 5)], 0.5), box.lty = 0)
legend (x = 0, y = 0.14, legend = rep ('', 2), lwd = 2, lty = 1:2,  bg = 'transparent',
        col = tColours [['colour']] [c (1, 5)], box.lty = 0)
dev.off ()
#========================================================================================
