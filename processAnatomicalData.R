#========================================================================================
# Script to process the cellular anatomy data for the 2018 experiment at Harvard Forest, 
# which was generated from thin-sections of samples collected on the 2018-11-15 and
# 2019-10-24 using ROXAS. ROXAS outputs were converted to median values of 20 
# micrometer-wide tangential bands, which are publicly available on the Harvard Forest 
# data archive as part of the experiments data set.
#
# Data repository url:
# Code repository url:
#
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('scam')) library ('scam')

# Get original working directory
#----------------------------------------------------------------------------------------
originalDir <- getwd ()

# Read ring width data generated using WIAD and anatomical data generated using ROXAS
#----------------------------------------------------------------------------------------
setwd ('/home/tim/projects/PlantGrowth/Exp2018Analysis/')
source ('readAnatomicalData.R')
source ('readRingWidths.R')

# Before any processing, we need to remove data from samples without full 2018 ring
#----------------------------------------------------------------------------------------
anatomicalData <- anatomicalData %>% 
  dplyr::filter (!(TREE == 4 & sampleDate == as_date ('2018-11-15') & sampleHeight == 2.5)) %>%
  dplyr::filter (PLOT != 4)

# Add new column to data (all rings started growing after the sample on the 5th of May 2018)
#----------------------------------------------------------------------------------------
anatomicalData <- add_column (anatomicalData, period = NA)

# set growing season threshold as 5 and 95% percent of annual growth having occured 
#----------------------------------------------------------------------------------------
threshold <- 0.05

# Set whether processing happens verbose or not
#----------------------------------------------------------------------------------------
VERBOSE <- FALSE

# Loop over for each row in the anatomical data to associate it with a growth period
#----------------------------------------------------------------------------------------
for (r in 1:dim (anatomicalData) [1]) {
  
  # If not 2018, skip. Only have temporal resolution for 2018 with multiple samples
  #--------------------------------------------------------------------------------------
  if (anatomicalData [['YEAR']] [r] != 2018) next
  
  # Get tree ID, treatment and sample height of the sample
  #------------------------------------------------------------------------------------
  treeID <- as.numeric (substr (anatomicalData [['TREE']] [r], 1, 2))
  h <- anatomicalData [['sampleHeight']] [r]
  t <- anatomicalData [['PLOT']] [r]
  
  # Select only relevant ring width data and add day of year column to fit GAM 
  #--------------------------------------------------------------------------------------
  tempData <- ringWidths %>% 
    filter (treeId == treeID, sampleHeight == h) %>% 
    select (sampleDate, RWI2018) %>%
    mutate (doy = lubridate::yday (sampleDate)) 
  
  # Is this a sample from 1 or 2m, where we only hav two samples
  #--------------------------------------------------------------------------------------
  if (anatomicalData [['sampleHeight']] [r] %in% 1:2) {
  
    next # I still need to work on measuring the ring width, if I really want to apportion 
    # this with a simple fraction 
    
  # Sample from a height with frequent sample to fit a General Additive model
  #--------------------------------------------------------------------------------------  
  } else {
    
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
  }
  
  if (VERBOSE) {
    print (paste ('Tree',treeID,' height',h,' line',r, 
                  anatomicalData [['period']] [r], iDoy, 
                  growthFraction* 100, anatomicalData [['RRADDISTR']] [r]))
  }
}

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
anatomicalData <- anatomicalData %>% add_column (cumNCells = NA, cumCWA = NA)
FIRSTCWA    <- TRUE
FIRSTNCELLS <- TRUE
for (r in 1:dim (anatomicalData) [1]) {
  
  # If year is not equal to 2018 skip to next iteration
  if (anatomicalData [['YEAR']] [r] != 2018) next
  
  # For the first sector in a ring
  if (r == 1 | FIRSTCWA) { 
    if (!is.na (anatomicalData [['CWA']] [r])) {
      # Just use the mean cell-wall area per cell and the number of cells in the sector
      anatomicalData [['cumCWA']] [r] <- anatomicalData [['CWA']] [r] * anatomicalData [['nCells']] [r] 
      
      # Switch FIRST off to move on once a non-NA value was entered 
      FIRSTCWA <- FALSE
    } else {
      FIRSTCWA <- TRUE
    }
  # for any sector following the first sector  
  } else {
    # Check whether this is a different radial profile from the previous row?
    differentProfile <- 
      anatomicalData [['YEAR']]         [r] != anatomicalData [['YEAR']]         [r-1] |
      anatomicalData [['TREE']]         [r] != anatomicalData [['TREE']]         [r-1] |
      anatomicalData [['sampleHeight']] [r] != anatomicalData [['sampleHeight']] [r-1] |
      anatomicalData [['sampleDate']]   [r] != anatomicalData [['sampleDate']]   [r-1]
    # If it is a different radial profile 
    if (differentProfile) {
      # and if the mean cell-wall area is not NA
      if (!is.na (anatomicalData [['CWA']] [r])) {
        # determine cumulative cell-wall area by multiplying mean cell-wall area per cell times 
        # number of cells
        anatomicalData [['cumCWA']] [r] <- anatomicalData [['CWA']] [r] * anatomicalData [['nCells']] [r]
      # else make sure that the next row is treated as first sector 
      } else {
        FIRSTCWA <- TRUE
      }
    # if it is the same radial profile as the previous row
    } else {
      
      # and if the cell-wall area per cell is not NA
      if (!is.na (anatomicalData [['CWA']] [r])) {
        
        # determine add the cumulative cell-wall area increment for this sector 
        # to the cumulative cell-wall area of the previous sector
        anatomicalData [['cumCWA']] [r] <- anatomicalData [['CWA']] [r] * 
          anatomicalData [['nCells']] [r] + anatomicalData [['cumCWA']] [r-1]
      # if this sector has not cell-wall area just use the previouses sector's 
      # cumulative cell-wall area
      } else {
        anatomicalData [['cumCWA']] [r] <- anatomicalData [['cumCWA']] [r-1]
      }
    }
  }
  
  # Is this the first sector of the radial profile?
  if (r == 1 | FIRSTNCELLS) { 
    
    # Is the number of cells in the sector not NA
    if (!is.na (anatomicalData [['nCells']] [r])) {
      
      # for first sectors of each radial profile, the cumulative number of cells 
      # is the number of cells in this sector
      anatomicalData [['cumNCells']] [r] <- anatomicalData [['nCells']] [r]
      
      # Switch FIRST off to move on once a non-NA value was entered 
      FIRSTNCELLS <- FALSE
    } else {
      # Leave FIRSTCELLS on, so assure that starting counts only once there are numbers to be counted
      FIRSTNCELLS <- TRUE
    }
  } else {
    # Is this a different profile from the previous row?
    differentProfile <- 
      anatomicalData [['YEAR']]         [r] != anatomicalData [['YEAR']]         [r-1] |
      anatomicalData [['TREE']]         [r] != anatomicalData [['TREE']]         [r-1] |
      anatomicalData [['sampleHeight']] [r] != anatomicalData [['sampleHeight']] [r-1] |
      anatomicalData [['sampleDate']]   [r] != anatomicalData [['sampleDate']]   [r-1]
    if (differentProfile) {
      if (!is.na (anatomicalData [['nCells']] [r])) {
        anatomicalData [['cumNCells']] [r] <- anatomicalData [['nCells']] [r] 
      } else {
        FIRSTCWA <- TRUE
      }
    } else {
      if (!is.na (anatomicalData [['nCells']] [r])) {
        anatomicalData [['cumNCells']] [r] <- anatomicalData [['nCells']] [r] + 
          anatomicalData [['cumNCells']] [r-1]
      } else {
        anatomicalData [['cumNCells']] [r] <- anatomicalData [['cumNCells']] [r-1]
      }
    }
  }
}

# Switch back to original working directory
#----------------------------------------------------------------------------------------
setwd (originalDir); rm (originalDir)

# Clean unnecessary variables from loop
#----------------------------------------------------------------------------------------
rm (r, h, t, sColours, tColours, differentProfile, growthFraction, treeID, iDoy, 
    tempData, threshold, direction, error, maxRWI, fit.gam, VERBOSE)
#========================================================================================