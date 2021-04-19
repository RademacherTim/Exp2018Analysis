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
        xlim = c (0, 100), ylim = c (0, 0.045), axes = FALSE)
  
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
  axis (side = 2, at = seq (0, 0.04, 0.02), las = 1)
  
  # Add smoothed mean signal
  plotRunningAverage (data = tempData)
  
}
# Add column
legend (x = 6, y = 0.04, legend = c ('control','chilled'), lwd = 1, lty = 1:2, bg = 'transparent',
        col = addOpacity (tColours [['colour']] [c (1, 5)], 0.5), box.lty = 0)
legend (x = 0, y = 0.04, legend = rep ('', 2), lwd = 2, lty = 1:2,  bg = 'transparent',
        col = tColours [['colour']] [c (1, 5)], box.lty = 0)
dev.off ()

# Plot violin plot of cumulative cell-wall area for the 2017 ring and fractions of the 
# 2018 ring grown before, during and after chilling 
#----------------------------------------------------------------------------------------
anatoData <- anatomicalData %>%
  filter (YEAR %in% 2017:2018) %>% 
  mutate (treatment = ifelse (PLOT == 1, 'control', 'chilled')) %>%
  mutate (treatment = factor (treatment, levels = c ('control','chilled')))
anatoData <- anatoData %>%
  mutate (exPeriod = ifelse (period <= as_date ('2018-06-25'), 
                             'before', 
                             ifelse (period <= as_date ('2018-09-03'), 
                                     'during', 
                                     'after'))) 

# Make sure that all 2017 data points and 2018 data for 1.0 and 2.0m get the right label
anatoData [['exPeriod']] [anatoData [['YEAR']] == 2017] <- '2017' 
anatoData [['exPeriod']] [anatoData [['YEAR']] == 2018 & 
                          anatoData [['sampleHeight']] %in% 1:2] <- '2018'

# Summarise data to get cumulative CWA formed for each period
cumulativeSummary <- anatoData %>% group_by (TREE, treatment, sampleHeight, exPeriod) %>% 
  summarise (cumCWA = max (cumCWA, na.rm = TRUE)) %>% ungroup ()

# Add cumulative CWA increment for 2018
cumulativeSummary <- add_column (cumulativeSummary, cumCWAinc = NA) 
for (r in 1:dim (cumulativeSummary) [1]) {
  t <- cumulativeSummary [['TREE']] [r]
  h <- cumulativeSummary [['sampleHeight']] [r]
  
  if (cumulativeSummary [['exPeriod']] [r] %in% c ('2017','2018','before')) {
    cumulativeSummary [['cumCWAinc']] [r] <- cumulativeSummary [['cumCWA']] [r] 
  } else if (cumulativeSummary [['exPeriod']] [r] == 'during') {
    cumulativeSummary [['cumCWAinc']] [r] <- cumulativeSummary [['cumCWA']] [r] - 
      cumulativeSummary [['cumCWA']] [cumulativeSummary [['exPeriod']] == 'before' &
                                        cumulativeSummary [['sampleHeight']] == h &
                                        cumulativeSummary [['TREE']] == t]
  } else if (cumulativeSummary [['exPeriod']] [r] == 'after') {
    cumulativeSummary [['cumCWAinc']] [r] <- cumulativeSummary [['cumCWA']] [r] - 
      cumulativeSummary [['cumCWA']] [cumulativeSummary [['exPeriod']] == 'during' &
                                        cumulativeSummary [['sampleHeight']] == h &
                                        cumulativeSummary [['TREE']] == t]
  }
}

# Add rows for 2018 full increment at sample heights where we can apportion fractions
for (t in c (1:5, 11:15)) {
  for (h in c (0.5, 1.5, 2.5, 4.0)) {
    # Get treatment
    i <- ifelse (t %in% 1:5, 'chilled','control')
    
    # Get growth increment that had grown after chilling 
    cumCWAincrement <- cumulativeSummary %>% 
      filter (TREE == t, sampleHeight == h, exPeriod == 'after') %>% 
      select (cumCWA) %>% unlist ()
    
    # verify that there is a proportion that grew after chilling
    if (length (cumCWAincrement) == 0) {
      # Get growth increment that had grown during chilling 
      cumCWAincrement <- cumulativeSummary %>% 
        filter (TREE == t, sampleHeight == h, exPeriod == 'during') %>% 
        select (cumCWA) %>% unlist ()

      # Add row for after increment
      cumulativeSummary <- add_row (cumulativeSummary, 
                                    TREE = t, 
                                    treatment = i,
                                    sampleHeight = h, 
                                    exPeriod = 'after', 
                                    cumCWA = cumCWAincrement,
                                    cumCWAinc = 0)
    }
    
    # Add row for 2018 increment
    cumulativeSummary <- add_row (cumulativeSummary, 
                                  TREE = t, 
                                  treatment = i,
                                  sampleHeight = h, 
                                  exPeriod = '2018', 
                                  cumCWA = cumCWAincrement,
                                  cumCWAinc = cumCWAincrement)
    
  }
}

# Arrange the tibble 
cumulativeSummary <- cumulativeSummary %>% arrange (TREE, sampleHeight, exPeriod) %>%
  mutate (exPeriod = factor (exPeriod, 
                             levels = c ('before','during','after','2017','2018')),
          sampleHeight = factor (sampleHeight, 
                                 levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)))


# plot median cell-wall area for control and chilled trees before the chilling 
g <- ggplot (cumulativeSummary) 

png (filename = './fig/Exp2018ChillingCumulativeCellWallArea.png', width = 750, height = 450)
g + #geom_violin (aes (x = treatment, y = cumCWAinc, fill = treatment), 
    #             alpha = 0.3, trim = FALSE,
    #             colour = '#aaaaaaaa', 
    #             show.legend = FALSE, draw_quantiles = TRUE, na.rm = TRUE) + 
  scale_fill_manual (values = tColours [['colour']] [c (5, 1)], 
                    labels = c ('control','chilled')) + 
  geom_boxplot (aes (x = treatment, y = cumCWAinc, fill = treatment), alpha = 0.8,
                width = 0.5, colour = '#333333',
                outlier.size = 0, na.rm = TRUE) +
  scale_y_continuous (breaks = seq (0, 60000, 20000), 
                      labels = c ('0','20K','40K','60K')) +
  labs (title = "Cumulative cell-wall area", 
        subtitle = expression (paste ('per 20 ',mu,'m tangential band', sep = '')),
        x = "Treatment",
        y = expression (paste ('Cumulative cell-wall area increment (',mu,m^2,')', sep = ''))) + 
  coord_flip (ylim = c (0, 60000)) +
  facet_grid (sampleHeight ~ exPeriod) +
  theme_classic () +
  theme (legend.position = "none", panel.spacing = unit (1, 'lines'))#,
dev.off ()

layout ()
tp <- cumulativeSummary %>% group_by (treatment, sampleHeight, exPeriod) %>%
  summarise (meanCWAinc = mean (cumCWAinc, na.rm = TRUE),
             seCWAinc   = se   (cumCWAinc))
g <- ggplot (tp, aes (x = treatment, y = meanCWAinc, color = treatment)) + 
  geom_point (size = 2.5) +
  geom_errorbar (aes (ymin = meanCWAinc - seCWAinc, 
                      ymax = meanCWAinc + seCWAinc),
                 width = 0.0, size = 1) +
  facet_grid (sampleHeight ~ exPeriod, scales = 'free') +
  coord_flip (ylim = c (0, 60000)) +
  theme_classic () +
  theme (legend.position = 'none')
g
dev.off ()

# Plot mean and standard error of the mean cell-wall area increment for various periods
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2018ChillingCWAIncrement.png', 
     width = 700, height = 400)
layout (matrix (1:5, nrow = 1), widths = c (1.3, 1, 1, 1, 1))
# loop over sampling heights
#----------------------------------------------------------------------------------------
offset <- 0.05
for (d in c ('before','during','after','2017','2018')) {
  
  # determine panel marigns
  if  (d == 'before') {
    par (mar = c (5, 5, 1, 1))
  } else {
    par (mar = c (5, 1, 1, 1))
  }
  
  if (d %in% c ('before','during','after')) {
    xmax <- 20000
  } else {
    xmax <- 40000
  }
  
  con <- tp [['exPeriod']] == d & tp [['treatment']] == 'chilled'
  # create plot area
  plot (x = tp [['meanCWAinc']] [con],
        y = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
          ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
        ylab = ifelse (d == 'before', 'Sample height (m)',''), 
        col = 'white', 
        xlab = expression (paste ('Cumulative cell-wall area (',mu,m^2,')', sep = '')),
        xlim = c (0, xmax), ylim = c (0, 4.2), axes = FALSE)
  segments (x0 = tp [['meanCWAinc']] [con] - tp [['seCWAinc']] [con],
            x1 = tp [['meanCWAinc']] [con] + tp [['seCWAinc']] [con],
            y0 = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
              ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
            col = tColours [['colour']] [5], lwd = 3)
  points (x = tp [['meanCWAinc']] [con],
          y = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
            ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
          pch = 23, bg = 'white', cex = 1.8, lwd = 3, col = tColours [['colour']] [5])
  con <- tp [['exPeriod']] == d & tp [['treatment']] == 'control'
  segments (x0 = tp [['meanCWAinc']] [con] - tp [['seCWAinc']] [con],
            x1 = tp [['meanCWAinc']] [con] + tp [['seCWAinc']] [con],
            y0 = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
              ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
            col = tColours [['colour']] [1], lwd = 3)
  points (x = tp [['meanCWAinc']] [con],
          y = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
            ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
          pch = 19, cex = 1.8, col = tColours [['colour']] [1], lwd = 3, bg = 'white')
  
  if (d != 'before') {
    #axis (side = 2, at = c (0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 4.0), labels = rep ('', 7))
  } else {
    axis (side = 2, at = c (0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 4.0), las = 1)
  }
  if (d %in% c ('before','during','after')) {
    axis (side = 1, at = seq (0, xmax, 10000))
  } else {
    axis (side = 1, at = seq (0, xmax, 10000))
  }
  
}
dev.off ()

#========================================================================================
