#==================================================================================================
# Script to cross-date the Exp2018 pines with Neil's long-term chronology
#--------------------------------------------------------------------------------------------------

# Load dependencies
#--------------------------------------------------------------------------------------------------
library ('dplR')


# Read Neils rwl file
#--------------------------------------------------------------------------------------------------
HF_PIST <- read.rwl (fname = '/media/tim/dataDisk/PlantGrowth/data/incrementCores/masterChronologies/HF_PIST_All.rwl',
                     format = 'tucson')


# Plot all raw ring widths for all individual trees from Neil's chronology'
#--------------------------------------------------------------------------------------------------
plot (HF_PIST, plot.type = 'spag')
  
# Detrend series
#--------------------------------------------------------------------------------------------------
HF_PIST.rwi <- detrend (rwl = HF_PIST, method = 'Spline')

# Calculate mean chronology
#--------------------------------------------------------------------------------------------------
HF_PIST.crn <- chron (HF_PIST.rwi, prefix = 'HF')

# Plot mean chronology
#--------------------------------------------------------------------------------------------------
plot (HF_PIST.crn, add.spline = TRUE, nyrs = 20, ylim = c (0.5, 1.5))

#==================================================================================================
