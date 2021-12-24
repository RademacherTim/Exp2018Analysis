#========================================================================================
# script to compare growing season dates from ring width data of the 2018 experiment
#----------------------------------------------------------------------------------------

# dependencies
#----------------------------------------------------------------------------------------
if (!exists ('growingSeasonDates')) source ('extractGrowingSeasonDates.R')
library ('ppcor')

# read anatomical data
#----------------------------------------------------------------------------------------
microcoreData <- read_csv (file = './data/anatomicalData_HF_Exp2018_Rademacher.csv',
                           col_types = cols ())

# extract number of cells
#----------------------------------------------------------------------------------------
nCells <- microcoreData %>% filter (YEAR == 2018) %>% group_by (TREE, sampleHeight) %>% 
  summarise (nCells = max (cumNCells, na.rm = TRUE), .groups = 'drop')

# combine data sets
#----------------------------------------------------------------------------------------
growingSeasonDates <- cbind (growingSeasonDates, nCells [, 3])

# make scatter plot of start
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (x = growingSeasonDates$endOfGrowth [growingSeasonDates$treatment == 1], 
      y = growingSeasonDates$startOfGrowth [growingSeasonDates$treatment == 1], 
      col = tColours [['colour']] [1], pch = 19, xlim = c (190, 365), ylim = c (120, 170), 
      las = 1, xlab = 'End of growing season', ylab = 'Start of growing season')
controlFit <- lm (growingSeasonDates$startOfGrowth [growingSeasonDates$treatment == 1] ~ 
      growingSeasonDates$endOfGrowth [growingSeasonDates$treatment == 1])
abline (controlFit, col = tColours [['colour']] [1])
summary (controlFit) 
points (x = growingSeasonDates$endOfGrowth [growingSeasonDates$treatment == 5], 
        y = growingSeasonDates$startOfGrowth [growingSeasonDates$treatment == 5], 
        col = tColours [['colour']] [4], pch = 23)
chilledFit <- lm (growingSeasonDates$startOfGrowth [growingSeasonDates$treatment == 5] ~ 
                    growingSeasonDates$endOfGrowth [growingSeasonDates$treatment == 5])
abline (chilledFit, col = tColours [['colour']] [4])
summary (chilledFit) 

# 
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (x = growingSeasonDates$nCells [growingSeasonDates$treatment == 1], 
      y = growingSeasonDates$startOfGrowth [growingSeasonDates$treatment == 1], 
      col = tColours [['colour']] [1], pch = 19, xlim = c (0, 50), ylim = c (120, 170), 
      las = 1, xlab = 'Number of cells', ylab = 'Start of growing season')
controlFit <- lm (growingSeasonDates$startOfGrowth [growingSeasonDates$treatment == 1] ~ 
                    growingSeasonDates$nCells [growingSeasonDates$treatment == 1])
abline (controlFit, col = tColours [['colour']] [1])
summary (controlFit) 
points (x = growingSeasonDates$nCells [growingSeasonDates$treatment == 5], 
        y = growingSeasonDates$startOfGrowth [growingSeasonDates$treatment == 5], 
        col = tColours [['colour']] [4], pch = 23)
chilledFit <- lm (growingSeasonDates$startOfGrowth [growingSeasonDates$treatment == 5] ~ 
                    growingSeasonDates$nCells [growingSeasonDates$treatment == 5])
abline (chilledFit, col = tColours [['colour']] [4])
summary (chilledFit) 

# 
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (x = growingSeasonDates$nCells [growingSeasonDates$treatment == 1], 
      y = growingSeasonDates$endOfGrowth [growingSeasonDates$treatment == 1], 
      col = tColours [['colour']] [1], pch = 19, xlim = c (0, 50), ylim = c (190, 365), 
      las = 1, xlab = 'Number of cells', ylab = 'End of growing season')
controlFit <- lm (growingSeasonDates$endOfGrowth [growingSeasonDates$treatment == 1] ~ 
                    growingSeasonDates$nCells [growingSeasonDates$treatment == 1])
abline (controlFit, col = tColours [['colour']] [1])
summary (controlFit) 
points (x = growingSeasonDates$nCells [growingSeasonDates$treatment == 5], 
        y = growingSeasonDates$endOfGrowth [growingSeasonDates$treatment == 5], 
        col = tColours [['colour']] [4], pch = 23)
chilledFit <- lm (growingSeasonDates$endOfGrowth [growingSeasonDates$treatment == 5] ~ 
                    growingSeasonDates$nCells [growingSeasonDates$treatment == 5])
abline (chilledFit, col = tColours [['colour']] [4])
summary (chilledFit) 

pcor (growingSeasonDates %>% filter (!is.na (endOfGrowth)) %>% 
        dplyr::select (endOfGrowth, startOfGrowth, nCells))
