#========================================================================================
# This script plot mean cell number variations between treatments and heights
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
  meanNCells <- binnedData %>% filter (PLOT == 1) %>% 
    group_by (bin) %>% summarise (meanNCells = mean (nCells, na.rm = TRUE),
                                  seCumCWA   = se (nCells))
  polygon (x = c (seq (reso/2, 100 - (reso/2), reso),
                  seq (100 - (reso/2), reso/2, -reso)),
           y = c (meanNCells [['meanNCells']] + meanNCells [['seCumCWA']], 
                  rev (meanNCells [['meanNCells']] - meanNCells [['seCumCWA']])),
           lty = 0, col = addOpacity (tColours [['colour']] [1], 0.5))
  lines (x = seq (reso/2, 100 - (reso/2), reso), 
         y = meanNCells [['meanNCells']],
         col = tColours [['colour']] [1], lwd  = 2)
  meanNCells <- binnedData %>% filter (PLOT == 5) %>% 
    group_by (bin) %>% summarise (meanNCells = mean (nCells, na.rm = TRUE),
                                  seCumCWA   = se (nCells))
  meanNCells <- meanNCells [!is.na (meanNCells [['bin']]), ]
  indices <- which (is.na (meanNCells [['seCumCWA']]))
  meanNCells [['seCumCWA']] [indices] <- rowMeans (cbind (meanNCells [['seCumCWA']] [indices-1], 
                                                          meanNCells [['seCumCWA']] [indices+1]))
  polygon (x = c (seq (reso/2, 100 - (reso/2), reso),
                  seq (100 - (reso/2), reso/2, -reso)),
           y = c (meanNCells [['meanNCells']] + meanNCells [['seCumCWA']], 
                  rev (meanNCells [['meanNCells']] - meanNCells [['seCumCWA']])),
           lty = 0, col = addOpacity (tColours [['colour']] [5], 0.5))
  lines (x = seq (reso/2, 100 - (reso/2), reso),
         y = meanNCells [['meanNCells']],
         col = tColours [['colour']] [5], lwd = 2, lty = 2)
  options (warn = 0)
}

# Plot cell-wall area (CWA) bover percentage ring width 
#----------------------------------------------------------------------------------------
png (filename = './fig/meanNCellOverPercentageRingWidth.png', width = 500, height = 700)
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
        y = tempData [['nCells']] [tempData [['PLOT']] == 5],
        col = 'white', xlab = ifelse (h == 0.5, 'Percentage ring width (%)',''), 
        ylab = 'Cell number (n)',
        xlim = c (0, 100), ylim = c (0, 2.0), axes = FALSE)
  
  # add the average portion formed before, during and after the experiment
  if (h %notin% 1:2) plotFractionFormed (data = tempData) 
  
  # add individual points
  points (x = tempData [['RRADDISTR']] [tempData [['PLOT']] == 5],
          y = tempData [['nCells']] [tempData [['PLOT']] == 5],
          col = addOpacity (tColours [['colour']] [5], 0.2), pch = 5)
  points (x = tempData [['RRADDISTR']] [tempData [['PLOT']] == 1],
          y = tempData [['nCells']] [tempData [['PLOT']] == 1],
          col = addOpacity (tColours [['colour']] [1], 0.2), pch = 19)  
  if (h != 0.5) {
    axis (side = 1, at = seq (0, 100, 10), labels = rep ('', 11))
  } else {
    axis (side = 1, at = seq (0, 100, 10))
  }
  axis (side = 2, at = seq (0, 2, 0.5), las = 1)
  
  # Add smoothed mean signal
  plotRunningAverage (data = tempData)
}

# Add column
legend (x = 6, y = 1350, legend = c ('control','chilled'), pch = c (19,5), bg = 'transparent',
        col = addOpacity (tColours [['colour']] [c (1, 5)], 0.5), box.lty = 0)
legend (x = 0, y = 1350, legend = rep ('', 2), lwd = 2, lty = 1:2,  bg = 'transparent',
        col = tColours [['colour']] [c (1, 5)], box.lty = 0)
dev.off 

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
#----------------------------------------------------------------------------------------
anatoData [['exPeriod']] [anatoData [['YEAR']] == 2017] <- '2017' 
anatoData [['exPeriod']] [anatoData [['YEAR']] == 2018 & 
                            anatoData [['sampleHeight']] %in% 1:2] <- '2018'

# Summarise data to get maximum cumulative number of cells formed for each period
#----------------------------------------------------------------------------------------
cumulativeSummary <- anatoData %>% group_by (TREE, treatment, sampleHeight, exPeriod) %>% 
  summarise (cumNCells = max (cumNCells, na.rm = TRUE), .groups = 'keep') %>% ungroup ()

# Add cumulative number of cell increment for 2018
#----------------------------------------------------------------------------------------
cumulativeSummary <- add_column (cumulativeSummary, cumNCellsinc = NA) 
for (r in 1:dim (cumulativeSummary) [1]) {
  t <- cumulativeSummary [['TREE']] [r]
  h <- cumulativeSummary [['sampleHeight']] [r]
  
  if (cumulativeSummary [['exPeriod']] [r] %in% c ('2017','2018','before')) {
    cumulativeSummary [['cumNCellsinc']] [r] <- cumulativeSummary [['cumNCells']] [r] 
  } else if (cumulativeSummary [['exPeriod']] [r] == 'during') {
    cumulativeSummary [['cumNCellsinc']] [r] <- cumulativeSummary [['cumNCells']] [r] - 
      cumulativeSummary [['cumNCells']] [cumulativeSummary [['exPeriod']] == 'before' &
                                    cumulativeSummary [['sampleHeight']] == h &
                                    cumulativeSummary [['TREE']] == t]
  } else if (cumulativeSummary [['exPeriod']] [r] == 'after') {
    cumulativeSummary [['cumNCellsinc']] [r] <- cumulativeSummary [['cumNCells']] [r] - 
      cumulativeSummary [['cumNCells']] [cumulativeSummary [['exPeriod']] == 'during' &
                                    cumulativeSummary [['sampleHeight']] == h &
                                    cumulativeSummary [['TREE']] == t]
  }
}

# Add rows for 2018 full increment at sample heights where we can apportion fractions
#----------------------------------------------------------------------------------------
for (t in c (1:5, 11:15)) {
  for (h in c (0.5, 1.5, 2.5, 4.0)) {
    # Get treatment
    i <- ifelse (t %in% 1:5, 'chilled','control')
    
    # Get growth increment that had grown after chilling 
    cumNCellsincrement <- cumulativeSummary %>% 
      filter (TREE == t, sampleHeight == h, exPeriod == 'after') %>% 
      select (cumNCells) %>% unlist ()
    
    # verify that there is a proportion that grew after chilling
    if (length (cumNCellsincrement) == 0) {
      # Get growth increment that had grown during chilling 
      cumNCellsincrement <- cumulativeSummary %>% 
        filter (TREE == t, sampleHeight == h, exPeriod == 'during') %>% 
        select (cumNCells) %>% unlist ()
      
      # Add row for after increment
      cumulativeSummary <- add_row (cumulativeSummary, 
                                    TREE = t, 
                                    treatment = i,
                                    sampleHeight = h, 
                                    exPeriod = 'after', 
                                    cumNCells = cumNCellsincrement,
                                    cumNCellsinc = 0)
    }
    
    # Add row for 2018 increment
    cumulativeSummary <- add_row (cumulativeSummary, 
                                  TREE = t, 
                                  treatment = i,
                                  sampleHeight = h, 
                                  exPeriod = '2018', 
                                  cumNCells = cumNCellsincrement,
                                  cumNCellsinc = cumNCellsincrement)
    
  }
}

# Add column for whether tree has a density fluctuation or not
#----------------------------------------------------------------------------------------
cumulativeSummary <- cumulativeSummary %>% add_column (densityFluctuation = NA)
for (r in 1:dim (cumulativeSummary) [1]) {
  t <- cumulativeSummary [['TREE']] [r]
  h <- cumulativeSummary [['sampleHeight']] [r]
  cumulativeSummary [['densityFluctuation']] [r] <- growingSeasonDates %>%
    filter (treeId == t, sampleHeight == h) %>% select (densityFluctuation) %>% 
    unlist ()
}

# Arrange the tibble 
#----------------------------------------------------------------------------------------
cumulativeSummary <- cumulativeSummary %>% arrange (TREE, sampleHeight, exPeriod) %>%
  mutate (exPeriod = factor (exPeriod, 
                             levels = c ('before','during','after','2017','2018')),
          sampleHeight = factor (sampleHeight, 
                                 levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)))

# Get average numbers for visual representation
#----------------------------------------------------------------------------------------
cumulativeSummary %>% 
  group_by (treatment, exPeriod) %>% 
  summarise (mean = mean (cumNCellsinc, na.rm = TRUE), se = se (cumNCellsinc))

# Get summary statistics by treatment
#----------------------------------------------------------------------------------------
tp <- cumulativeSummary %>% group_by (treatment, sampleHeight, exPeriod) %>%
  summarise (meanNCellsinc = mean (cumNCellsinc, na.rm = TRUE),
             seNCellsinc   = se   (cumNCellsinc),
             .groups = 'keep')

# Get summary stats for dividing into tree with and without Intra-annual density fluctuation
#----------------------------------------------------------------------------------------
tpWith <- cumulativeSummary %>% 
  group_by (treatment, sampleHeight, exPeriod, densityFluctuation) %>%
  summarise (meancumNCellsinc = mean (cumNCellsinc, na.rm = TRUE),
             secumNCellsinc   = se   (cumNCellsinc),
             .groups = 'keep')

# Plot mean and standard error of the mean cell-wall area increment for various periods
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2018ChillingNCellsIncrement.png', 
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
    xmax <- 27
  } else {
    xmax <- 50
  }
  
  con <- tp [['exPeriod']] == d & tp [['treatment']] == 'chilled'
  # create plot area
  plot (x = tp [['meanNCellsinc']] [con],
        y = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
          ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
        ylab = ifelse (d == 'before', 'Sample height (m)',''), 
        col = 'white', 
        xlab = expression (paste ('number of cells increment (n)', sep = '')),
        xlim = c (0, xmax), ylim = c (0, 4.2), axes = FALSE)
  
  # Plot summary statistics for all chilled trees
  segments (x0 = tp [['meanNCellsinc']] [con] - tp [['seNCellsinc']] [con],
            x1 = tp [['meanNCellsinc']] [con] + tp [['seNCellsinc']] [con],
            y0 = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
              ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
            col = tColours [['colour']] [ifelse (d == 'before', 4, 
                                                 ifelse (d == 'during', 5, 
                                                         ifelse (d == 'after', 6, 5)))], 
            lwd = 3)
  points (x = tp [['meanNCellsinc']] [con],
          y = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
            ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
          pch = 23, bg = 'white', cex = 1.8, lwd = 3, 
          col = tColours [['colour']] [ifelse (d == 'before', 4, 
                                               ifelse (d == 'during', 5, 
                                                       ifelse (d == 'after', 6, 5)))])
  
  # Plot summary stats for chilled trees with density fluctuation
  # con <- tpWith [['exPeriod']] == d &
  #   tpWith [['treatment']] == 'chilled' &
  #   tpWith [['densityFluctuation']]
  # segments (x0 = tpWith [['meancumNCellsinc']] [con] - tpWith [['secumNCellsinc']] [con],
  #           x1 = tpWith [['meancumNCellsinc']] [con] + tpWith [['secumNCellsinc']] [con],
  #           y0 = as.numeric (levels (tpWith [['sampleHeight']] [con])) [tpWith [['sampleHeight']] [con]] +
  #             ifelse (tpWith [['treatment']] [con] == 'chilled', -offset, offset),
  #           col = '#666666', lwd = 3)
  # points (x = tpWith[['meancumNCellsinc']] [con],
  #         y = as.numeric (levels (tpWith [['sampleHeight']] [con]))[tpWith [['sampleHeight']] [con]] +
  #           ifelse (tpWith [['treatment']] [con] == 'chilled', -offset, offset),
  #         pch = 24, bg = 'white', cex = 1.8, lwd = 3,
  #         col = '#666666')
 
  # Plot summary stats for chilled trees without density fluctuation
  # con <- tpWith [['exPeriod']] == d &
  #   tpWith [['treatment']] == 'chilled' &
  #   !(tpWith [['densityFluctuation']])
  # segments (x0 = tpWith [['meancumNCellsinc']] [con] - tpWith [['secumNCellsinc']] [con],
  #           x1 = tpWith [['meancumNCellsinc']] [con] + tpWith [['secumNCellsinc']] [con],
  #           y0 = as.numeric (levels (tpWith [['sampleHeight']] [con])) [tpWith [['sampleHeight']] [con]] +
  #             ifelse (tpWith [['treatment']] [con] == 'chilled', -offset, offset),
  #           col = '#666666', lwd = 3)
  # points (x = tpWith[['meancumNCellsinc']] [con],
  #         y = as.numeric (levels (tpWith [['sampleHeight']] [con]))[tpWith [['sampleHeight']] [con]] +
  #           ifelse (tpWith [['treatment']] [con] == 'chilled', -offset, offset),
  #         pch = 25, bg = 'white', cex = 1.8, lwd = 3,
  #         col = '#666666')
  
  # Plot summary statistics for all control trees
  con <- tp [['exPeriod']] == d & tp [['treatment']] == 'control'
  segments (x0 = tp [['meanNCellsinc']] [con] - tp [['seNCellsinc']] [con],
            x1 = tp [['meanNCellsinc']] [con] + tp [['seNCellsinc']] [con],
            y0 = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
              ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
            col = tColours [['colour']] [ifelse (d == 'before', 1,
                                                 ifelse (d == 'during', 2,
                                                         ifelse (d == 'after', 3, 1)))], 
            lwd = 3)
  points (x = tp [['meanNCellsinc']] [con],
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
    axis (side = 1, at = seq (0, xmax, 25))
  } else {
    axis (side = 1, at = seq (0, xmax, 25))
  }
  
}
dev.off ()
#========================================================================================
