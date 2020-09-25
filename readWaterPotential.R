#========================================================================================
# script to read the needle and branch water potential data for the 2018 chilling 
# experiment on the white pines at Harvard Forest.
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')

# read water potential data file
#----------------------------------------------------------------------------------------
phi <- read_csv (file = './data/waterPotentialMeasurementsExp2018.csv', 
                 col_types = cols ())

#========================================================================================
