#========================================================================================
# Script to read and process the cellular anatomy data for the 2017 experiment at Harvard 
# Forest.
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')
library ('scam')

# Get original working directory
#----------------------------------------------------------------------------------------
originalDir <- getwd ()

# Change working directory to read anatomical data
#----------------------------------------------------------------------------------------
setwd ('/media/tim/dataDisk/PlantGrowth/data/microcores/woodAnatomy/Exp2018/')

# Read the data per 20 micron tangential slices for the samples collected on the 2018-11-15
#----------------------------------------------------------------------------------------
anatomicalData <- read.table (file = '20muband_ALL_2018.11.15.txt', header = TRUE)

# Change sample height to the actual height in meters above ground and a sample date
#----------------------------------------------------------------------------------------
anatomicalData <- anatomicalData %>% 
  mutate (sampleHeight = case_when ((POS == 'I')   ~ 0.5, (POS == 'II.5')  ~ 1.0,
                                    (POS == 'II')  ~ 1.5, (POS == 'III.5') ~ 2.0,
                                    (POS == 'III') ~ 2.5, (POS == 'IV')    ~ 4.0)) %>%
  add_column (sampleDate = as_date ('2018-11-15'))

# Add the data for 20 micrometer tangential slices for samples collected on 2019-10-24
#----------------------------------------------------------------------------------------
temp <- read.table (file = '20muband_ALL_2019.10.24.txt', header = TRUE)

# Change sample height to the actual height in meters above ground and a sample date
#----------------------------------------------------------------------------------------
temp <- temp %>% 
  mutate (sampleHeight = case_when ((POS == 'I')   ~ 0.5, (POS == 'II.5')  ~ 1.0,
                                    (POS == 'II')  ~ 1.5, (POS == 'III.5') ~ 2.0,
                                    (POS == 'III') ~ 2.5, (POS == 'IV')    ~ 4.0)) %>%
  add_column (sampleDate = as_date ('2019-10-24'))

# Join dates for anatomical data
#----------------------------------------------------------------------------------------
anatomicalData <- rbind (anatomicalData, temp)

# Read ring width data
#----------------------------------------------------------------------------------------
setwd ('/home/tim/projects/PlantGrowth/Exp2018Analysis/')
source ('readRingWidths.R')

# Add new column to data (all rings started growing after the sample on the 5th of May 2018)
#----------------------------------------------------------------------------------------
anatomicalData <- add_column (anatomicalData, period = NA)

# Loop over for each row in the anatomical data to associate it with a growth period
#----------------------------------------------------------------------------------------
for (r in 1:dim (anatomicalData) [1]) {
  
  # If not 2018, skip. Only have temporal resolution to for 2018 with multiple samples
  #--------------------------------------------------------------------------------------
  if (anatomicalData [['YEAR']] [r] != 2018) next
  if (anatomicalData [['sampleHeight']] [r] %in% 1:2) next
  
  # Get tree ID, treatment and sample height of the sample
  #--------------------------------------------------------------------------------------
  treeID <- as.numeric (substr (anatomicalData [['TREE']] [r], 1, 2))
  h <- anatomicalData [['sampleHeight']] [r]
  t <- anatomicalData [['PLOT']] [r]
  
  # Select only relevant ring width data and add day of year column to fit GAM 
  #--------------------------------------------------------------------------------------
  tempData <- ringWidths %>% 
    filter (treeId == treeID, sampleHeight == h) %>% 
    select (sampleDate, RWI2018) %>%
    mutate (doy = lubridate::yday (sampleDate)) 
  
  # Assume that the ring measured in 2019 is the end of the year growth
  #--------------------------------------------------------------------------------------
  if (sum (tempData [['sampleDate']] == as_date ('2019-10-24'), na.rm = TRUE) > 0) 
  {
    tempData [['sampleDate']] [tempData [['sampleDate']] == as_date ('2019-10-24')] <- as_date ('2018-12-31')
    tempData [['doy']] [tempData [['sampleDate']] == as_date ('2018-12-31')] <- yday ('2018-12-31')
  }
  
  # Fit general additive model to growth data
  #--------------------------------------------------------------------------------------
  fit.gam <- scam (RWI2018 ~ s (doy, k = 8, bs = 'mpi'), 
                   data = tempData, 
                   family = quasipoisson)
  
  # get the maximal value of the GAM for the year (to correct the predicted values to 
  # growth fractions) 
  #------------------------------------------------------------------------------------
  maxRWI <- max (exp (predict (fit.gam, newdata = data.frame (doy = c (1:365)))))
  
  # associate tangential band with a period of growth using the general additive model
  #--------------------------------------------------------------------------------------
  if (!is.na (anatomicalData [['RRADDISTR']] [r])) {
    #print (paste ('Tree',treeID,' height',h,' line',i))
    
    # find the best guess of the day of the year when this tangential band grew 
    # within a 0.1% error on the fraction growth
    #------------------------------------------------------------------------------------
    error <- 10000 # set error to a large number to start with
    iDoy <- 180 # start halfway through the year
    while (error < -0.1 | error > 0.1) {
      growthFraction <- exp (predict (fit.gam, newdata = data.frame (doy = iDoy))) / maxRWI
      if ((growthFraction * 100) > anatomicalData [['RRADDISTR']] [r]) {
        if (iDoy != 180) {
          if (direction == 'later') break
        }
        iDoy <- iDoy - 1
        direction <- 'earlier'
      } else {
        if (iDoy != 180) {
          if (direction == 'earlier') break
        }
        iDoy <- iDoy + 1
        direction <- 'later'
      }
      error <- (growthFraction * 100) - anatomicalData [['RRADDISTR']] [r]
    }
    
    # Associate the estimated date of growth with the tangential band
    anatomicalData [['period']] [r] <- as_date (iDoy, origin = '2018-01-01')
  }
  
  # print (paste ('Tree',treeID,' height',h,' line',i, 
  #               anatomicalData [['period']] [r], iDoy, 
  #               growthFraction* 100, anatomicalData [['RRADDISTR']] [r]))
}

# Clean unnecessary variables from loop
#----------------------------------------------------------------------------------------
rm (r, h, t, treeID, iDoy, tempData, direction, error, maxRWI, PLOT, fit.gam)

# Add cell width column to data
#----------------------------------------------------------------------------------------
anatomicalData <- add_column (anatomicalData, cellRadWidth = NA)
anatomicalData <- add_column (anatomicalData, cellTanWidth = NA)

# Calculate zonal average tangential and radial cell width (microns)
#----------------------------------------------------------------------------------------
anatomicalData [['cellRadWidth']] <- anatomicalData [['DRAD']] + 2 * anatomicalData [['CWTTAN']]
anatomicalData [['cellTanWidth']] <- anatomicalData [['DTAN']] + 2 * anatomicalData [['CWTRAD']]

# Estimate the number of cell in each sector
#----------------------------------------------------------------------------------------
anatomicalData <- add_column (anatomicalData, nCells = 20.0 / 
                                                       anatomicalData [['cellRadWidth']])

# Provide column with cumulative cell-wall area
#----------------------------------------------------------------------------------------
#anatomicalData <- anatomicalData %>% 
#  group_by (TREE, sampleHeight, YEAR, sampleDate) %>% 
#  mutate (cumCWA    = cumsum (CWA),
#          cumNCells = cumsum (nCells)) 

# Add cumulative number of cells column
#----------------------------------------------------------------------------------------
#anatomicalData <- anatomicalData %>% 
#  group_by (TREE, sampleHeight, YEAR, sampleDate) %>% 
#  mutate (cumNCells = cumsum (nCells))

# Switch back to original working directory
#----------------------------------------------------------------------------------------
setwd (originalDir); rm (originalDir)
#========================================================================================