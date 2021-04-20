#========================================================================================
# This script plot median cell-wall area variations between treatments and heights
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
  
  reso <- 5
  binnedData <- data %>% mutate (bin = cut (RRADDISTR, seq (0, 100, reso)))
  meanCWA <- binnedData %>% filter (PLOT == 1) %>% 
    group_by (bin) %>% summarise (meanCWA = mean (CWA, na.rm = TRUE),
                                  seCWA   = se (CWA))
  polygon (x = c (seq (reso/2, 100 - (reso/2), reso), 
                  seq (100 - (reso/2), reso/2, -reso)),
           y = c (meanCWA [['meanCWA']] + meanCWA [['seCWA']], 
                  rev (meanCWA [['meanCWA']] - meanCWA [['seCWA']])),
           lty = 0, col = addOpacity (tColours [['colour']] [1], 0.5))
  lines (x = seq (reso/2, 100 - (reso/2), reso), 
         #y = rollmean (meanCWA [['meanCWA']], k = 10, na.pad = TRUE), 
         y = meanCWA [['meanCWA']],
         col = tColours [['colour']] [1], lwd  = 2)
  meanCWA <- binnedData %>% filter (PLOT == 5) %>% 
    group_by (bin) %>% summarise (meanCWA = mean (CWA, na.rm = TRUE),
                                  seCWA   = se (CWA))
  meanCWA <- meanCWA [!is.na (meanCWA [['bin']]), ]
  indices <- which (is.na (meanCWA [['seCWA']]))
  meanCWA [['seCWA']] [indices] <- rowMeans (cbind (meanCWA [['seCWA']] [indices-1], 
                                                    meanCWA [['seCWA']] [indices+1]))
  polygon (x = c (seq (reso/2, 100 - (reso/2), reso), 
                  seq (100 - (reso/2), reso/2, -reso)),
           y = c (meanCWA [['meanCWA']] + meanCWA [['seCWA']], 
                  rev (meanCWA [['meanCWA']] - meanCWA [['seCWA']])),
           lty = 0, col = addOpacity (tColours [['colour']] [5], 0.5))
  lines (x = seq (reso/2, 100 - (reso/2), reso), 
         #y = rollmean (meanCWA [['meanCWA']], k = 10, na.pad = TRUE), 
         y = meanCWA [['meanCWA']],
         col = tColours [['colour']] [5], lwd = 2, lty = 2)
}

# Plot cell-wall area (CWA) bover percentage ring width 
#----------------------------------------------------------------------------------------
png (filename = './fig/meanCWAOverPercentageRingWidth.png', width = 500, height = 700)
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
  
  # plot empty plot
  plot (x = tempData [['RRADDISTR']] [tempData [['PLOT']] == 5],
        y = tempData [['CWA']] [tempData [['PLOT']] == 5],
        col = 'white', xlab = ifelse (h ==0.5, 'Percentage ring width (%)',''), 
        ylab = expression (paste ('Cell-wall area (',mu, m^2,')', sep = '')),
        xlim = c (0, 100), ylim = c (0, 1400), axes = FALSE)

  # add the average portion formed before, during and after the experiment
  if (h %notin% 1:2) plotFractionFormed (data = tempData) 
  
  # add individual points
  points (x = tempData [['RRADDISTR']] [tempData [['PLOT']] == 5],
          y = tempData [['CWA']] [tempData [['PLOT']] == 5],
          col = addOpacity (tColours [['colour']] [5], 0.2), pch = 5)
  points (x = tempData [['RRADDISTR']] [tempData [['PLOT']] == 1],
          y = tempData [['CWA']] [tempData [['PLOT']] == 1],
          col = addOpacity (tColours [['colour']] [1], 0.2), pch = 19)  
  if (h != 0.5) {
    axis (side = 1, at = seq (0, 100, 10), labels = rep ('', 11))
  } else {
    axis (side = 1, at = seq (0, 100, 10))
  }
  axis (side = 2, at = seq (0, 1400, 400), las = 1)
  
  # Add smoothed mean signal
  plotRunningAverage (data = tempData)
}

# Add column
legend (x = 6, y = 1350, legend = c ('control','chilled'), pch = c (19,5), bg = 'transparent',
        col = addOpacity (tColours [['colour']] [c (1, 5)], 0.5), box.lty = 0)
legend (x = 0, y = 1350, legend = rep ('', 2), lwd = 2, lty = 1:2,  bg = 'transparent',
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
png (filename = './fig/Exp2018ChillingMeanCellWallArea.png', 
     width = 700, height = 350)
g <- ggplot (anatoData) 
g + geom_violin (aes (x = treatment, y = CWA, fill = treatment), 
                 alpha = 0.3, trim = FALSE,
                 colour = '#aaaaaaaa', 
                 show.legend = FALSE, draw_quantiles = TRUE, na.rm = TRUE) + 
  scale_fill_manual (values = tColours [['colour']] [c (1, 5)], 
                     labels = c ('Control','Chilled')) + 
  geom_boxplot (aes (x = treatment, y = CWA, fill = treatment), alpha = 0.8,
                width = 0.3, colour = '#333333',
                outlier.size = 0, na.rm = TRUE) +
  scale_y_continuous (breaks = seq (0, 1000, 500)) +
  labs (title = "Cell-wall area", 
        subtitle = expression (paste ('per 20 ',mu,'m tangential band', sep = '')),
        x = "Treatment",
        y = expression (paste ('Mean cell-wall area (',mu,m^2,')', sep = ''))) + 
  coord_flip (ylim = c (0, 1200), ) +
  facet_grid (sampleHeight ~ exPeriod) +
  theme_classic () +
  theme (legend.position = "none", panel.spacing = unit (1, 'lines'))
dev.off ()


# Summarise data to get cumulative ring width formed for each period
cumulativeSummary <- anatoData %>% group_by (TREE, treatment, sampleHeight, exPeriod) %>% 
  summarise (CWT = mean (CWTTAN, na.rm = TRUE),
             CWA = mean (CWA, na.rm = TRUE), .groups = 'keep') %>% ungroup ()

# Add rows for 2018 mean at sample heights where we can apportion fractionstmp <- anatoData %>% filter (YEAR == 2018) %>% group_by (TREE, treatment, sampleHeight) %>%
tmp <- anatoData %>% group_by (TREE, treatment, sampleHeight) %>% 
  summarise (CWT = mean (CWTTAN, na.rm = TRUE),
             CWA = mean (CWA, na.rm = TRUE), .groups = 'keep') %>% 
  add_column (exPeriod = '2018') %>% ungroup ()
cumulativeSummary <- rbind (cumulativeSummary, tmp)

# Arrange the tibble 
cumulativeSummary <- cumulativeSummary %>% arrange (TREE, sampleHeight, exPeriod) %>%
  mutate (exPeriod = factor (exPeriod, 
                             levels = c ('before','during','after','2017','2018')),
          sampleHeight = factor (sampleHeight, 
                                 levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)))

tp <- cumulativeSummary %>% group_by (treatment, sampleHeight, exPeriod) %>%
  summarise (meanCWT = mean (CWT, na.rm = TRUE),
             seCWT   = se   (CWT),
             meanCWA = mean (CWA, na.rm = TRUE),
             seCWA   = se   (CWA))

# Plot mean and standard error of the mean cell-wall thickness for various periods
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2018ChillingMeanCWT.png', 
     width = 700, height = 400)
layout (matrix (1:5, nrow = 1), widths = c (1.3, 1, 1, 1, 1))
# loop over sampling heights
#----------------------------------------------------------------------------------------
offset <- 0.08
for (d in c ('before','during','after','2017','2018')) {
  
  # determine panel marigns
  if  (d == 'before') {
    par (mar = c (5, 5, 1, 1))
  } else {
    par (mar = c (5, 1, 1, 1))
  }
  
  if (d %in% c ('before','during','after')) {
    xmin <- 2; xmax <- 6
  } else {
    xmin <- 2; xmax <- 6
  }
  
  con <- tp [['exPeriod']] == d & tp [['treatment']] == 'chilled'
  # create plot area
  plot (x = tp [['meanCWT']] [con],
        y = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
          ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
        ylab = ifelse (d == 'before', 'Sample height (m)',''), 
        col = 'white', 
        xlab = expression (paste ('Mean tangential cell-wall thickness (',mu,m,')', sep = '')),
        xlim = c (xmin, xmax), ylim = c (0, 4.2), axes = FALSE)
  segments (x0 = tp [['meanCWT']] [con] - tp [['seCWT']] [con],
            x1 = tp [['meanCWT']] [con] + tp [['seCWT']] [con],
            y0 = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
              ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
            col = tColours [['colour']] [ifelse (d == 'before', 4, 
                                                 ifelse (d == 'during', 5, 
                                                         ifelse (d == 'after', 6, 5)))], lwd = 3)
  points (x = tp [['meanCWT']] [con],
          y = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
            ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
          pch = 23, bg = 'white', cex = 1.8, lwd = 3, 
          col = tColours [['colour']] [ifelse (d == 'before', 4, 
                                               ifelse (d == 'during', 5, 
                                                       ifelse (d == 'after', 6, 5)))])
  con <- tp [['exPeriod']] == d & tp [['treatment']] == 'control'
  segments (x0 = tp [['meanCWT']] [con] - tp [['seCWT']] [con],
            x1 = tp [['meanCWT']] [con] + tp [['seCWT']] [con],
            y0 = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
              ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
            col = tColours [['colour']] [ifelse (d == 'before', 1, 
                                                 ifelse (d == 'during', 2, 
                                                         ifelse (d =='after', 3, 1)))], lwd = 3)
  points (x = tp [['meanCWT']] [con],
          y = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
            ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
          pch = 19, cex = 1.8, 
          col = tColours [['colour']] [ifelse (d == 'before', 1, 
                                               ifelse (d == 'during', 2, 
                                                       ifelse (d == 'after', 3, 1)))], 
          lwd = 3, bg = 'white')
  
  if (d != 'before') {
    #axis (side = 2, at = c (0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 4.0), labels = rep ('', 7))
  } else {
    axis (side = 2, at = c (0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 4.0), las = 1)
  }
  if (d %in% c ('before','during','after')) {
    axis (side = 1, at = seq (xmin, xmax, 2))
  } else {
    axis (side = 1, at = seq (xmin, xmax, 2))
  }
  
}
dev.off ()


# Plot mean and standard error of the mean cell-wall area for various periods
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2018ChillingMeanCWA.png', 
     width = 700, height = 400)
layout (matrix (1:5, nrow = 1), widths = c (1.3, 1, 1, 1, 1))
# loop over sampling heights
#----------------------------------------------------------------------------------------
offset <- 0.08
for (d in c ('before','during','after','2017','2018')) {
  
  # determine panel marigns
  if  (d == 'before') {
    par (mar = c (5, 5, 1, 1))
  } else {
    par (mar = c (5, 1, 1, 1))
  }
  
  if (d %in% c ('before','during','after')) {
    xmin <- 0; xmax <- 840
  } else {
    xmin <- 0; xmax <- 630
  }
  
  con <- tp [['exPeriod']] == d & tp [['treatment']] == 'chilled'
  # create plot area
  plot (x = tp [['meanCWA']] [con],
        y = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
          ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
        ylab = ifelse (d == 'before', 'Sample height (m)',''), 
        col = 'white', 
        xlab = expression (paste ('Mean cell-wall area (',mu,m^2,')', sep = '')),
        xlim = c (xmin, xmax), ylim = c (0, 4.2), axes = FALSE)
  segments (x0 = tp [['meanCWA']] [con] - tp [['seCWA']] [con],
            x1 = tp [['meanCWA']] [con] + tp [['seCWA']] [con],
            y0 = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
              ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
            col = tColours [['colour']] [ifelse (d == 'before', 4, 
                                                 ifelse (d == 'during', 5, 
                                                         ifelse (d == 'after', 6, 5)))], lwd = 3)
  points (x = tp [['meanCWA']] [con],
          y = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
            ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
          pch = 23, bg = 'white', cex = 1.8, lwd = 3, 
          col = tColours [['colour']] [ifelse (d == 'before', 4, 
                                               ifelse (d == 'during', 5, 
                                                       ifelse (d == 'after', 6, 5)))])
  con <- tp [['exPeriod']] == d & tp [['treatment']] == 'control'
  segments (x0 = tp [['meanCWA']] [con] - tp [['seCWA']] [con],
            x1 = tp [['meanCWA']] [con] + tp [['seCWA']] [con],
            y0 = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
              ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
            col = tColours [['colour']] [ifelse (d == 'before', 1, 
                                                 ifelse (d == 'during', 2, 
                                                         ifelse (d == 'after', 3, 1)))], 
            lwd = 3)
  points (x = tp [['meanCWA']] [con],
          y = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
            ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
          pch = 19, cex = 1.8, 
          col = tColours [['colour']] [ifelse (d == 'before', 1, 
                                               ifelse (d == 'during', 2,
                                                       ifelse (d == 'after', 3, 1)))], 
          lwd = 3, bg = 'white')
  
  if (d != 'before') {
    #axis (side = 2, at = c (0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 4.0), labels = rep ('', 7))
  } else {
    axis (side = 2, at = c (0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 4.0), las = 1)
  }
  if (d %in% c ('before','during','after')) {
    axis (side = 1, at = seq (xmin, xmax, 210))
  } else {
    axis (side = 1, at = seq (xmin, xmax, 210))
  }
  
}
dev.off ()
#========================================================================================
