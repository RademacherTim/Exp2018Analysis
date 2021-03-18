#========================================================================================
# Script to read the cellular anatomy data for the 2018 experiment at Harvard Forest 
# which was generated from thin-sections of samples collected on the 2018-11-15 
# and 2019-10-24 using ROXAS. ROXAS outputs were converted to median values of 20 
# micrometer-wide tangential bands, which are publicly available on the Harvard Forest 
# data archive as part of the experiments data set.
#
# Data repository url:
# Code repository url:
#
# Author: Tim Rademacher (trademacher@fas.harvard.edu)
#
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('add_column')) library ('tidyverse')
if (!existsFunction ('as_date')) library ('lubridate')

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

# Reset original working directory
#----------------------------------------------------------------------------------------
setwd (originalDir)

# Clean up
#----------------------------------------------------------------------------------------
rm (originalDir, temp)
#========================================================================================