#========================================================================================
# This script plot cumulative cell-wall area across treatments and heights
#----------------------------------------------------------------------------------------

# Source anatomical data and dependencies
#----------------------------------------------------------------------------------------
READ <- FALSE; if (READ) source ('processAnatomicalData.R')
if (!exists ('tColours')) source ('plotingFunctions.R')
if (!existsFunction ('rollmean')) library ('zoo')

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

# Summarise data to get cumulative ring width formed for each period
cumulativeSummary <- anatoData %>% group_by (TREE, treatment, sampleHeight, exPeriod) %>% 
  summarise (RW = max (RADDISTR.BAND, na.rm = TRUE)) %>% ungroup ()

# Add cumulative ring width volume increment for 2018
cumulativeSummary <- add_column (cumulativeSummary, RWinc = NA) 
for (r in 1:dim (cumulativeSummary) [1]) {
  t <- cumulativeSummary [['TREE']] [r]
  h <- cumulativeSummary [['sampleHeight']] [r]
  
  if (cumulativeSummary [['exPeriod']] [r] %in% c ('2017','2018','before')) {
    cumulativeSummary [['RWinc']] [r] <- cumulativeSummary [['RW']] [r] 
  } else if (cumulativeSummary [['exPeriod']] [r] == 'during') {
    cumulativeSummary [['RWinc']] [r] <- cumulativeSummary [['RW']] [r] - 
      cumulativeSummary [['RW']] [cumulativeSummary [['exPeriod']] == 'before' &
                                        cumulativeSummary [['sampleHeight']] == h &
                                        cumulativeSummary [['TREE']] == t]
  } else if (cumulativeSummary [['exPeriod']] [r] == 'after') {
    cumulativeSummary [['RWinc']] [r] <- cumulativeSummary [['RW']] [r] - 
      cumulativeSummary [['RW']] [cumulativeSummary [['exPeriod']] == 'during' &
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
    RWincrement <- cumulativeSummary %>% 
      filter (TREE == t, sampleHeight == h, exPeriod == 'after') %>% 
      select (RW) %>% unlist ()
    
    # verify that there is a proportion that grew after chilling
    if (length (RWincrement) == 0) {
      # Get growth increment that had grown during chilling 
      RWincrement <- cumulativeSummary %>% 
        filter (TREE == t, sampleHeight == h, exPeriod == 'during') %>% 
        select (RW) %>% unlist ()
      
      # Add row for after increment
      cumulativeSummary <- add_row (cumulativeSummary, 
                                    TREE = t, 
                                    treatment = i,
                                    sampleHeight = h, 
                                    exPeriod = 'after', 
                                    RW = RWincrement,
                                    RWinc = 0)
    }
    
    # Add row for 2018 increment
    cumulativeSummary <- add_row (cumulativeSummary, 
                                  TREE = t, 
                                  treatment = i,
                                  sampleHeight = h, 
                                  exPeriod = '2018', 
                                  RW = RWincrement,
                                  RWinc = RWincrement)
    
  }
}

# Arrange the tibble 
cumulativeSummary <- cumulativeSummary %>% arrange (TREE, sampleHeight, exPeriod) %>%
  mutate (exPeriod = factor (exPeriod, 
                             levels = c ('before','during','after','2017','2018')),
          sampleHeight = factor (sampleHeight, 
                                 levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)))


tp <- cumulativeSummary %>% group_by (treatment, sampleHeight, exPeriod) %>%
  summarise (meanRWinc = mean (RWinc, na.rm = TRUE),
             seRWinc   = se   (RWinc))

# Plot mean and standard error of the mean cell-wall area increment for various periods
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2018ChillingRWIncrement.png', 
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
    xmax <- 1800
  } else {
    xmax <- 2400
  }
  
  con <- tp [['exPeriod']] == d & tp [['treatment']] == 'chilled'
  # create plot area
  plot (x = tp [['meanRWinc']] [con],
        y = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
          ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
        ylab = ifelse (d == 'before', 'Sample height (m)',''), 
        col = 'white', 
        xlab = expression (paste ('ring width increment (',mu,m,')', sep = '')),
        xlim = c (0, xmax), ylim = c (0, 4.2), axes = FALSE)
  segments (x0 = tp [['meanRWinc']] [con] - tp [['seRWinc']] [con],
            x1 = tp [['meanRWinc']] [con] + tp [['seRWinc']] [con],
            y0 = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
              ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
            col = tColours [['colour']] [5], lwd = 3)
  points (x = tp [['meanRWinc']] [con],
          y = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
            ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
          pch = 23, bg = 'white', cex = 1.8, lwd = 3, col = tColours [['colour']] [5])
  con <- tp [['exPeriod']] == d & tp [['treatment']] == 'control'
  segments (x0 = tp [['meanRWinc']] [con] - tp [['seRWinc']] [con],
            x1 = tp [['meanRWinc']] [con] + tp [['seRWinc']] [con],
            y0 = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
              ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
            col = tColours [['colour']] [1], lwd = 3)
  points (x = tp [['meanRWinc']] [con],
          y = as.numeric (levels (tp [['sampleHeight']] [con]))[tp [['sampleHeight']] [con]] + 
            ifelse (tp [['treatment']] [con] == 'chilled', -offset, offset),
          pch = 19, cex = 1.8, col = tColours [['colour']] [1], lwd = 3, bg = 'white')
  
  if (d != 'before') {
    #axis (side = 2, at = c (0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 4.0), labels = rep ('', 7))
  } else {
    axis (side = 2, at = c (0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 4.0), las = 1)
  }
  if (d %in% c ('before','during','after')) {
    axis (side = 1, at = seq (0, xmax, 600))
  } else {
    axis (side = 1, at = seq (0, xmax, 600))
  }
  
}
dev.off ()

#========================================================================================
