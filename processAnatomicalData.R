#========================================================================================
# Script to read and process the cellular anatomy data for the 2017 experiment at Harvard 
# Forest.
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')
library ('rjson')

# Get original working directory
#----------------------------------------------------------------------------------------
originalDir <- getwd ()

# Change working directory to read anatomical data
#----------------------------------------------------------------------------------------
setwd ('/media/tim/dataDisk/PlantGrowth/data/microcores/woodAnatomy/Exp2018/')

# Read the data per 20 micron tangential slices
#----------------------------------------------------------------------------------------
anatomicalData <- read.table (file = '20muband_ALL.txt', header = TRUE)

# Change sample height to the actual height in meters above ground
#----------------------------------------------------------------------------------------
anatomicalData <- anatomicalData %>% 
  mutate (sampleHeight = case_when ((POS == 'I')   ~ 0.5, (POS == 'II') ~ 1.5,
                                    (POS == 'III') ~ 2.5, (POS == 'IV') ~ 4.0,))

# Change working directory
#----------------------------------------------------------------------------------------
#setwd ('/media/tim/dataDisk/PlantGrowth/data/allometry/Exp2018/')

# Read the allometric data
#----------------------------------------------------------------------------------------
# allometricData <- read_excel (path = paste0 (getwd (),'/allometricDataExp2018.xlsx'), 
#                               sheet = 'allometricData', 
#                               na = "NA")

# Read ring width data
#----------------------------------------------------------------------------------------
setwd ('/home/tim/projects/PlantGrowth/Exp2018Analysis/')
source ('readRingWidths.R')

# Calculate the fractions of ring formed
#----------------------------------------------------------------------------------------
for (r in 1:dim (ringWidths) [1]) {
  
  # Find the reference index
  con <- ringWidths [['treeId']] == ringWidths [['treeId']] [r] & 
    ringWidths [['sampleHeight']] == ringWidths [['sampleHeight']] [r] 
  referenceIndex <- ringWidths [['RWI2018']] [con & ringWidths [['sampleDate']] == as_date ('2018-11-15')]
  
  # Calculate fraction of ring grown at sample date
  ringWidths [['fraction']] [r] <- ringWidths [['RWI2018']] [r] / referenceIndex
}

# Add new column to data (all rings started growing after the sample on the 5th of May 2018)
#----------------------------------------------------------------------------------------
anatomicalData <- add_column (anatomicalData, period = as_date ('2018-05-01'))

# Loop over for each row in the anatomical data to associate it with a growth period
#----------------------------------------------------------------------------------------
for (i in 1:dim (anatomicalData) [1]) {
  
  # If not 2018, skip. Only have temporal resolution to for 2018 with multiple samples
  #--------------------------------------------------------------------------------------
  if (anatomicalData [['YEAR']] [i] != 2018) next
  
  # Get tree ID, treatment and sample height of the sample
  #--------------------------------------------------------------------------------------
  treeID       <- as.numeric (substr (anatomicalData [['TREE']] [i], 1, 2))
  sampleHeight <- anatomicalData [['sampleHeight']] [i]
  treatment    <- anatomicalData [['PLOT']] [i]
  
  # associate fractional position with a period
  #--------------------------------------------------------------------------------------
  if (!is.na (anatomicalData [['RRADDISTR']] [i])) {
    
    con <- ringWidths [['treeId']] == treeID &
      ringWidths [['sampleHeight']] == sampleHeight
    
    # Get all dates and growth fractions
    fractions <- ringWidths %>% filter (con) %>% select (sampleDate, fraction) %>% 
      filter (sampleDate != as_date ('2019-10-24')) %>% arrange (sampleDate)
    
    # Get index of fraction that is the lattest date at which the fraction grown is still lower than the fraction of the ring
    j <- min (which (fractions [['fraction']] >= anatomicalData [['RRADDISTR']] [i] / 100), na.rm = TRUE)
    
    # Determine date of that fraction as period of growth
    anatomicalData [['period']] [i] <- fractions [['sampleDate']] [j]
    # print (sprintf ('Period: %s, growth: %s, and fractions: %s and %s',
    #                 anatomicalData [['period']] [i], anatomicalData [['RRADDISTR']] [i],
    #                 round (fractions [['fraction']] [c (j-1)], 2),
    #                 round (fractions [['fraction']] [c (j)],  2)))
 
    # TR - I could linearly interpolate between sampling date to get more precise 
    #      estimate of tme of formation for each sector
  }
}

# Clean unnecessary variables from loop
#----------------------------------------------------------------------------------------
rm (referenceIndex, i, j, r, con, fractions, sampleHeight, treatment, treeID)

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
anatomicalData <- anatomicalData %>% group_by (TREE, sampleHeight, YEAR) %>% 
  mutate (cumCWA = cumsum (CWA)) 

# Switch back to original working directory
#----------------------------------------------------------------------------------------
setwd (originalDir); rm (originalDir)
#========================================================================================