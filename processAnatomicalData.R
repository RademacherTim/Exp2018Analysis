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
  sampleHeight <- anatomicalData [['POS']] [i]
  treatment    <- anatomicalData [['PLOT']] [i]
  
  # associate fractional position with a period
  #--------------------------------------------------------------------------------------
  if (!is.na (anatomicalData [['RRADDISTR']] [i])) {
    
    con <- ringWidths [['treeId']] == treeID &
      ringWidths [['sampleHeight']] == sampleHeight
    
    # Check whether 
    if () 
      
      
    if (!is.na (fJul) & !is.na (fAug) & !is.na (fOct)) {
      if (data [['RRADDISTR']] [i] / 100 <= fJul) {
        periodDate <- as_date ('2017-07-03')
        lowerFraction <- 0
        upperFraction <- fJul
      } else if (data [['RRADDISTR']] [i] / 100 <= fAug) {
        periodDate <- as_date ('2017-08-09')
        lowerFraction <- fJul
        upperFraction <- fAug
      } else if (data [['RRADDISTR']] [i] / 100 <= fOct) {
        periodDate <- as_date ('2017-10-09')
        lowerFraction <- fAug
        upperFraction <- fOct
      } else {
        periodDate <- as_date ('2017-11-03')
        lowerFraction <- fOct
        upperFraction <- 1
      }
    } else if (treeID == 16 & sampleHeight == 'B') {
      if (data [['RRADDISTR']] [i] / 100 <= fJul) {
        periodDate <- as_date ('2017-07-03')
      } else if (data [['RRADDISTR']] [i] / 100 <= fAug) {
        periodDate <- as_date ('2017-08-09')
      }
    }
  } else {
    fraction <- data [['RADDISTR.BAND']] [i] / data [['MRW']] [i]
    if (!is.na (fJul) & !is.na (fAug) & !is.na (fOct)) {
      if (fraction <= fJul) {
        periodDate <- as_date ('2017-07-03')
      } else if (fraction  <= fAug) {
        periodDate <- as_date ('2017-08-09')
      } else if (fraction  <= fOct) {
        periodDate <- as_date ('2017-10-09')
      } else {
        periodDate <- as_date ('2017-11-03')
      }
    }
  }
  # Fill in period date
  #--------------------------------------------------------------------------------------
  data [['period']] [i] <- periodDate
  
  # Determine start date of the period
  #--------------------------------------------------------------------------------------
  if (periodDate == as_date ('2017-07-03')) {
    startDate <- as_date ('2017-03-01') 
  } else if (periodDate == as_date ('2017-08-09')) {
    startDate <- as_date ('2017-03-01') 
  } else if (periodDate == as_date ('2017-10-09')) {
    startDate <- as_date ('2017-08-09') 
  } else if (periodDate == as_date ('2017-08-09')) {
    startDate <- as_date ('2017-03-01') 
  }
  
  # Determine the date time increment
  #--------------------------------------------------------------------------------------
  inc <- periodDate - startDate
  
  # Add a linear estimate of the date of formation
  #--------------------------------------------------------------------------------------
  data [['formationDate']] [i] <- startDate + (data [['RRADDISTR']] [i] / 100.0 ) / (upperFraction - lowerFraction) * inc
}

# Clean unnecessary variables from loop
#----------------------------------------------------------------------------------------
rm (i, fAug, fJul, fOct, fraction, sampleHeight, treatment, treeID)

# Add cell width column to data
#----------------------------------------------------------------------------------------
data <- add_column (data, cellRadWidth = NA)
data <- add_column (data, cellTanWidth = NA)

# Calculate zonal average tangential and radial cell width (microns)
#----------------------------------------------------------------------------------------
data [['cellRadWidth']] <- data [['DRAD']] + 2 * data [['CWTTAN']]
data [['cellTanWidth']] <- data [['DTAN']] + 2 * data [['CWTRAD']]

# Rename column POS to height
#----------------------------------------------------------------------------------------
data <- rename (data, height = POS, treatment = PLOT, year = YEAR)
data [['tree']] <- as.numeric (substr (data [['TREE']], 1, 2))
data [['treatment']] <- as.numeric (substr (data [['treatment']], 2, 2)) 
data <- select (data, -TREE)

# Estimate the number of cell in each sector
#----------------------------------------------------------------------------------------
data <- add_column (data, nCells = 20.0 / data [['cellRadWidth']])

# Provide column with cumulative cell wall arrange_all
#----------------------------------------------------------------------------------------
data <- data %>% group_by (tree, height, year) %>% mutate (cumCWA = cumsum (CWA)) 

# Switch back to original working directory
#----------------------------------------------------------------------------------------
setwd (originalDir); rm (originalDir)
#========================================================================================