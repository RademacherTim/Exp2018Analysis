#========================================================================================
# This script plot median cell size variations between treatments and heights
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
  # set resolution for moving window
  reso <- 5
  binnedData <- data %>% mutate (bin = cut (RRADDISTR, seq (0, 100, reso)))
  meanRadWidth <- binnedData %>% filter (PLOT == 1) %>% 
    group_by (bin) %>% summarise (meanRadWidth = mean (cellRadWidth, na.rm = TRUE),
                                  seRadWidth   = se (cellRadWidth))
  polygon (x = c (seq (reso/2, 100 - (reso/2), reso), 
                  seq (100 - (reso/2), reso/2, -reso)),
           y = c (meanRadWidth [['meanRadWidth']] + meanRadWidth [['seRadWidth']], 
                  rev (meanRadWidth [['meanRadWidth']] - meanRadWidth [['seRadWidth']])),
           lty = 0, col = addOpacity (tColours [['colour']] [1], 0.5))
  lines (x = seq (reso/2, 100 - (reso/2), reso), 
         #y = rollmean (meanRadWidth [['meanRadWidth']], k = 10, na.pad = TRUE), 
         y = meanRadWidth [['meanRadWidth']],
         col = tColours [['colour']] [1], lwd  = 2)
  meanRadWidth <- binnedData %>% filter (PLOT == 5) %>% 
    group_by (bin) %>% summarise (meanRadWidth = mean (cellRadWidth, na.rm = TRUE),
                                  seRadWidth   = se (cellRadWidth))
  meanRadWidth <- meanRadWidth [!is.na (meanRadWidth [['bin']]), ]
  indices <- which (is.na (meanRadWidth [['seRadWidth']]))
  meanRadWidth [['seRadWidth']] [indices] <- rowMeans (cbind (meanRadWidth [['seRadWidth']] [indices-1], 
                                                    meanRadWidth [['seRadWidth']] [indices+1]), na.rm = TRUE)
  polygon (x = c (seq (reso/2, 100 - (reso/2), reso), 
                  seq (100 - (reso/2), reso/2, -reso)),
           y = c (meanRadWidth [['meanRadWidth']] + meanRadWidth [['seRadWidth']], 
                  rev (meanRadWidth [['meanRadWidth']] - meanRadWidth [['seRadWidth']])),
           lty = 0, col = addOpacity (tColours [['colour']] [5], 0.5))
  lines (x = seq (reso/2, 100 - (reso/2), reso), 
         #y = rollmean (meanRadWidth [['meanRadWidth']], k = 10, na.pad = TRUE), 
         y = meanRadWidth [['meanRadWidth']],
         col = tColours [['colour']] [5], lwd = 2, lty = 2)
  options (warn = 0)
}

# Plot cell size over percentage ring width 
#----------------------------------------------------------------------------------------
png (filename = './fig/meanCellSizeOverPercentageRingWidth.png', width = 500, height = 700)
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
        y = tempData [['cellRadWidth']] [tempData [['PLOT']] == 5],
        col = 'white', xlab = ifelse (h == 0.5, 'Percentage ring width (%)',''), 
        ylab = expression (paste ('Radial cell diameter (',mu, m,')', sep = '')),
        xlim = c (0, 100), ylim = c (0, 75), axes = FALSE)
  
  # Add the average portion formed before, during and after the experiment
  if (h %notin% 1:2) plotFractionFormed (data = tempData) 
  
  # Add individual points
  points (x = tempData [['RRADDISTR']] [tempData [['PLOT']] == 5],
          y = tempData [['cellRadWidth']] [tempData [['PLOT']] == 5],
          col = addOpacity (tColours [['colour']] [5], 0.2), pch = 5)
  points (x = tempData [['RRADDISTR']] [tempData [['PLOT']] == 1],
          y = tempData [['cellRadWidth']] [tempData [['PLOT']] == 1],
          col = addOpacity (tColours [['colour']] [1], 0.2), pch = 19)  
  if (h != 0.5) {
    axis (side = 1, at = seq (0, 100, 10), labels = rep ('', 11))
  } else {
    axis (side = 1, at = seq (0, 100, 10))
  }
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


# Plot violin plot of median cell-wall area for fraction of the ring grown before, 
# during and after chilling 
#----------------------------------------------------------------------------------------
anatoData <- anatomicalData  %>% 
  filter (YEAR %in% 2017:2018) %>% 
  mutate (treatment = ifelse (PLOT == 1, 'control', 'chilled')) %>%
  mutate (treatment = factor (treatment, levels = c ('control','chilled')))

# Make sure that all 2017 data points get the right label
anatoData <- anatoData %>%
  mutate (exPeriod = YEAR)
copy <- anatoData %>% filter (YEAR != 2017)
copy [['exPeriod']] [copy [['period']] >  as_date ('2018-09-03')] <- 'after' 
copy [['exPeriod']] [copy [['period']] <= as_date ('2018-09-03') &
                       copy [['period']] >  as_date ('2018-06-25')] <- 'during' 
copy [['exPeriod']] [copy [['period']] <= as_date ('2018-06-25')] <- 'before' 
anatoData <- rbind (anatoData, 
                    copy) %>%
  mutate (exPeriod = factor (exPeriod, 
                             levels = c ('before','during','after','2017','2018')))  %>% 
  mutate (sampleHeight = factor (sampleHeight, 
                                 levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)))

# plot median cell-wall area for control and chilled trees before the chilling 
png (filename = './fig/Exp2018ChillingMeanCellSize.png', 
     width = 750, height = 450)
g <- ggplot (anatoData) 
g + geom_violin (aes (x = treatment, y = cellRadWidth, fill = treatment), 
                 alpha = 0.3, trim = FALSE,
                 colour = '#aaaaaaaa', 
                 show.legend = FALSE, draw_quantiles = TRUE, na.rm = TRUE) + 
  scale_fill_manual (values = tColours [['colour']] [c (1, 5)], 
                     labels = c ('Control','Chilled')) + 
  geom_boxplot (aes (x = treatment, y = cellRadWidth, fill = treatment), alpha = 0.8,
                width = 0.3, colour = '#333333',
                outlier.size = 0, na.rm = TRUE) +
  scale_y_continuous (breaks = seq (0, 60, 20)) +
  labs (title = "Cell size ", 
        subtitle = expression (paste ('per 20 ',mu,'m tangential band', sep = '')),
        x = "Treatment",
        y = expression (paste ('Mean radial cell diameter (',mu,m,')', sep = ''))) + 
  coord_flip (ylim = c (0, 70), ) +
  facet_grid (sampleHeight ~ exPeriod) +
  theme_classic () +
  theme (legend.position = "none", panel.spacing = unit (1, 'lines'))#,
dev.off ()
#========================================================================================
