# Script to read in the TRIAD rin width measurements
#----------------------------------------------------------------------------------------

# Comments:
#    - sample 03 I has only the 2018 and 2017 ring complete. Maybe there is a second sample we could cut?


# Load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')
library ('rjson')

# Set working directory to read json files for the follow-up microcores
#----------------------------------------------------------------------------------------
setwd ('/media/tim/dataDisk/PlantGrowth/data/microcores/woodAnatomy/Exp2018/ringWidthTRIAD/')

# List all json output file from TRIAD
#----------------------------------------------------------------------------------------
jsonFiles <- list.files (path = './', pattern = '.json')

# Create tibble with ring measurements from the 2018 micrcore 
#----------------------------------------------------------------------------------------
ringWidths <- tibble (treeId = numeric (), treatment = numeric (), sampleDate = as_date (NA),
                      sampleHeight = numeric (), Y2019 = numeric (), 
                      Y2018 = numeric (), Y2017 = numeric (), Y2016 = numeric (), 
                      Y2015 = numeric (), Y2014 = numeric (), Y2013 = numeric (), 
                      Y2012 = numeric (), Y2011 = numeric (), Y2010 = numeric ())#, 
                      # Y2009 = numeric (), Y2008 = numeric (), Y2007 = numeric (), 
                      # Y2006 = numeric (), Y2005 = numeric (), Y2004 = numeric (), 
                      # Y2003 = numeric (), Y2002 = numeric (), Y2001 = numeric (), 
                      # Y2000 = numeric (), Y1999 = numeric (), Y1998 = numeric ())
k <- 0
# Loop over json files and read them
#----------------------------------------------------------------------------------------
for (j in 1: length (jsonFiles)) {
  
  # Read in TRIAD outputs
  #--------------------------------------------------------------------------------------
  temp <- fromJSON (file = jsonFiles [j]) 
  treeID <- as.numeric (substr (temp [['sampleID']], 1, 2))
  sampleHeight <- temp [['sampleHeight']]
  sampleDate <- as_date (temp [['sampleDate']])
  len <- length (temp [['markerData']])
  t <- as.numeric (temp [['plotID']]) # treatment
  growingSeason <- temp [['sampleYearGrowth']]
  
  # For now jump measurements from November 2018 or compressed trees
  #--------------------------------------------------------------------------------------
  if (sampleDate == '2018-11-15') next
  if (sampleDate == '2019-10-24' & growingSeason != 'all') {
    stop (paste0 ('Error with growing season',treeID,growingSeason))
  } else if (sampleDate == '2019-06-19' & growingSeason != 'some') {
    stop (paste0 ('Error with growing season',treeID,growingSeason))
  }
  if (t == 4) next
  k <- k + 1
  #print (c (len, treeID, sampleHeight, sampleDate, t))
  
  # Check that sample Height were entered correctly
  #--------------------------------------------------------------------------------------
  sampleH2 <- substr (temp [['sampleID']], 4, 6)
  if ((sampleH2 == 'I'   & sampleHeight != 0.5) |
      (sampleH2 == 'II'  & sampleHeight != 1.5) | 
      (sampleH2 == 'III' & sampleHeight != 2.5) | 
      (sampleH2 == 'IV'  & sampleHeight != 4.0)) {
    stop (paste0 ('Error with sample height',treeID,sampleH2)) 
  }
  
  # Extract growth measurement, associated years and types of markers
  #--------------------------------------------------------------------------------------
  for (i in 1:(len-1)) {
    if (i == 1) { 
      types  <- unlist (temp [['markerData']] [i]) [['type']] [[1]]
      years  <- unlist (temp [['markerData']] [i]) [['year']] [[1]]
      growth <- ifelse ('growth' %in% names (unlist (temp [['markerData']] [i])),
                        unlist (temp [['markerData']] [i]) [['growth']] [[1]], NA)
    } else {
      types  <- c (types, unlist (temp [['markerData']] [i]) [['type']] [[1]])
      years  <- c (years, unlist (temp [['markerData']] [i]) [['year']] [[1]])
      growth <- c (growth, ifelse ('growth' %in% names (unlist (temp [['markerData']] [i])),
                                   unlist (temp [['markerData']] [i]) [['growth']] [[1]], NA))
    }
  }
  #print (years)
  
  # Wrangle data
  #--------------------------------------------------------------------------------------
  growth <- as.numeric (growth [types %in% c ('Normal','Missing') &
                                years <= 2019 & !is.na (years)])
  years  <- as.numeric (years  [types %in% c ('Normal','Missing') &
                                years <= 2019 & !is.na (years)])
  if (years [1] == 2019) {
    growth <- c (growth, rep (NA, 31-length (growth)))
    years  <- c (years,  seq (years [length (years)]-1, 1988))
  } else {
    growth <- c (NA, growth, rep (NA, 30-length (growth)))
    years  <- c (2019, years,  seq (years [length (years)]-1, 1988))
  }
  print (growth)
  print (years)
  
  # add to tibble with all growth for all years years
  #--------------------------------------------------------------------------------------
  ringWidths <- ringWidths %>% add_row (treeId = treeID,
                                        treatment = t,
                                        sampleHeight = sampleHeight,
                                        sampleDate = sampleDate,
                                        Y2019 = growth [years == 2019],
                                        Y2018 = growth [years == 2018],
                                        Y2017 = growth [years == 2017],
                                        Y2016 = growth [years == 2016],
                                        Y2015 = growth [years == 2015],
                                        Y2014 = growth [years == 2014],
                                        Y2013 = growth [years == 2013],
                                        Y2012 = growth [years == 2012],
                                        Y2011 = growth [years == 2011],
                                        Y2010 = growth [years == 2010])

}  # end json file loop


# Standardise ring width using the 2015 ring
#----------------------------------------------------------------------------------------
ringWidths <- ringWidths %>% mutate (RWI2019 = Y2019 / Y2015,
                                     RWI2018 = Y2018 / Y2015,
                                     RWI2017 = Y2017 / Y2015)

# Summarise growth
#----------------------------------------------------------------------------------------
summaryData <- ringWidths %>% group_by (treatment, sampleDate, sampleHeight) %>% 
  summarise (meanY19 = mean (Y2019, na.rm = TRUE),
             seY19   = se   (Y2019),
             meanY18 = mean (Y2018, na.rm = TRUE),
             seY18   = se   (Y2018),
             meanY17 = mean (Y2017, na.rm = TRUE),
             seY17   = se   (Y2017),
             meanY16 = mean (Y2016, na.rm = TRUE),
             seY16   = se   (Y2016),
             meanY15 = mean (Y2015, na.rm = TRUE),
             seY15   = se   (Y2015),
             meanRWI2019 = mean (RWI2019, na.rm = TRUE),
             seRWI2019   = se (RWI2019),
             meanRWI2018 = mean (RWI2018, na.rm = TRUE),
             seRWI2018   = se (RWI2018),
             meanRWI2017 = mean (RWI2017, na.rm = TRUE),
             seRWI2017   = se (RWI2017)) 

boxplot (RWI2019 ~ treatment + sampleDate + sampleHeight, ringWidths,
         las = 1, xaxt = 'n', xlab = '')

# Pivot data into long format
#----------------------------------------------------------------------------------------
data2018 <- dataFollowUp %>% pivot_longer ()

# Clean unnecessary variables from loop
#----------------------------------------------------------------------------------------
rm (temp, treeID, t, i, j, k, jsonFiles, sampleDate, sampleHeight, growth, types, years, 
    con, len)
