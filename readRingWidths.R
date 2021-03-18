#========================================================================================
# Script to read in the ring width measurements performed with WIAD on the thin-sections
# of microcores of White pines at Harvard Forest from throughout the 2018 growing seaons,
# as part of the chilling and compression experiment. All data is publicly available on 
# the Harvard Forest Data Archive.
#
# Data repository url:
# Code repository url:
#
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
if (!exists ('tibble'))   library ('tidyverse')
if (!exists ('as_date'))  library ('lubridate')
if (!exists ('fromJSON')) library ('rjson')
if (!exists ('tColours')) source ('plotingFunctions.R')

# get orginal working directory
#----------------------------------------------------------------------------------------
originalDir <- getwd ()

# Set working directory to read json files for the follow-up microcores
#----------------------------------------------------------------------------------------
setwd ('/media/tim/dataDisk/PlantGrowth/data/microcores/woodAnatomy/Exp2018/ringWidthsWIAD/corrected/')

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
VERBOSE <- FALSE
# Loop over json files and read them
#----------------------------------------------------------------------------------------
for (j in 1: length (jsonFiles)) {
  
  # Read in TRIAD outputs
  #--------------------------------------------------------------------------------------
  temp <- rjson::fromJSON (file = jsonFiles [j]) 
  treeID <- as.numeric (substr (temp [['sampleID']], 1, 2))
  sampleHeight <- temp [['sampleHeight']]
  sampleDate <- as_date (temp [['sampleDate']])
  len <- length (temp [['markerData']])
  t <- as.numeric (temp [['plotID']]) # treatment
  growingSeason <- temp [['sampleYearGrowth']]
  
  # check metadata
  #--------------------------------------------------------------------------------------
  if (temp$status != 'Confirmed')      stop ('Error: Metadata status was not confirmed!')  
  if (temp$species != 'Pinus strobus') stop ('Error: Species is not Pinus strobus!') 
  if (temp$sampleYearGrowth %notin% c ('none','some','all')) {
    stop ('Error: Sample year growth is not "some" or "all"!')  
  }
  if (temp$sampleDPI !=  38100)        stop ('Error: DPI is not 57596!')  
  if (!temp$barkFirst [[1]])           stop ('Error: Bark was not first!')  
  if (temp$siteLocID != 'BigChill')    stop ('Error: Site location ID was not "BigChill"!')
  if (temp$plotID %notin% c (1,4,5))   stop ('Error: Plot ID is not correct.')
  if (temp$collection != 'Experiment 2018')  stop ('Error: Collection is not "Experiment 2018"')
  if ((temp$plotID == '5' & as.numeric (substr (temp$sampleID, 1, 2)) %notin% 1:5) |
      (temp$plotID == '4' & as.numeric (substr (temp$sampleID, 1, 2)) %notin% 6:10) |
      (temp$plotID == '1' & as.numeric (substr (temp$sampleID, 1, 2)) %notin% 11:15)){
    stop ('Error: Treatment is not correct!')
  }
  
  # Check that the growing season measurements are correct
  #--------------------------------------------------------------------------------------
  if ((sampleDate == '2019-10-24' | sampleDate == '2018-11-15') & growingSeason != 'all') {
    stop (paste0 ('Error with growing season',treeID,growingSeason))
  } else if (sampleDate == '2018-05-01' & growingSeason != 'none') {
    stop (paste0 ('Error with growing season ',treeID,growingSeason))
  } else if ((sampleDate == '2018-06-14' | sampleDate == '2018-06-19' | 
              sampleDate == '2018-07-05' | sampleDate == '2018-07-19' | 
              sampleDate == '2018-08-02' | sampleDate == '2018-08-16' | 
              sampleDate == '2018-08-30' | sampleDate == '2018-09-06' | 
              sampleDate == '2018-09-27') & growingSeason != 'some') {
    stop (paste0 ('Error with growing season ',treeID,growingSeason))
  }
  #if (t == 4) next
  k <- k + 1
  #print (c (len, treeID, sampleHeight, format (sampleDate, '%Y-%m-%d'), t))
  
  # Check that sample Height were entered correctly
  #--------------------------------------------------------------------------------------
  sampleH2 <- substr (temp [['sampleID']], 4, 6)
  if ((sampleH2 == 'I'   & sampleHeight != 0.5) |
      (sampleH2 == 'II'  & sampleHeight != 1.5) | 
      (sampleH2 == 'III' & sampleHeight != 2.5) | 
      (sampleH2 == 'IV'  & sampleHeight != 4.0)) {
    stop (paste0 ('Error with sample height ',treeID,sampleH2)) 
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
  } else if (years [1] == 2018) {
    growth <- c (NA, growth, rep (NA, 30-length (growth)))
    years  <- c (2019, years,  seq (years [length (years)]-1, 1988))
  } else if (years [1] == 2017) {
    growth <- c (NA, NA, growth, rep (NA, 29-length (growth)))
    years  <- c (2019, 2018, years,  seq (years [length (years)]-1, 1988))
  }
  #print (growth)
  #print (years)
  
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

  if (VERBOSE) {
    if (treeID == 13 & sampleHeight == 1.5) {
    print (paste (jsonFiles [j], treeID, sampleHeight, sampleDate, 
                  growth [years == 2017], growth [years == 2018], 
                  growth [years == 2018] / growth [years == 2017]))
    }
  }
}  # end json file loop

# Add lines for no growth in the beginning of January and May for all slides. All slides 
# were only visually inspected but showed not growth in the first samples from 2018-05-01
#----------------------------------------------------------------------------------------
for (i in 1:15) {
  for (h in c (0.5, 1.5, 2.5, 4.0)) {
    # Condition to extract relevant data
    con <- ringWidths [['treeId']] == i & 
      ringWidths [['sampleHeight']] == h 
    
    # Extract treatment
    t <- unique (ringWidths [['treatment']] [con])
    
    # Check that there is not an actual measurement for beginning of may
    if (dim (filter (ringWidths, con, sampleDate == as_date ('2018-05-01'))) [1] < 1) {
      ringWidths <- ringWidths %>% add_row (treeId = i, treatment = t, 
                                            sampleDate = as_date ('2018-05-01'),
                                            sampleHeight = h, Y2019 = NA, Y2018 = 0, 
                                            Y2017 = 1, Y2016 = 1, Y2015 = 1, Y2014 = 1, 
                                            Y2013 = 1, Y2012 = 1, Y2011 = 1)
    } else {
      ringWidths [['Y2018']] [con & ringWidths [['sampleDate']] == as_date ('2018-05-01')] <- 0 # For the measured image I indicated that the growing season did not start yet, WIAD does not create a ring, but it should be 0 
    }
    
    # add measurement for 2018-01-01
    ringWidths <- ringWidths %>% add_row (treeId = i, treatment = t, 
                                          sampleDate = as_date ('2018-01-01'), 
                                          sampleHeight = h, Y2019 = NA, Y2018 = 0, 
                                          Y2017 = 1, Y2016 = 1, Y2015 = 1, Y2014 = 1, 
                                          Y2013 = 1, Y2012 = 1, Y2011 = 1)
  }
}

# Arrange in advancing order by date, tree, sampling height
#----------------------------------------------------------------------------------------
ringWidths <- ringWidths %>% arrange (sampleDate, treeId, sampleHeight)

# Remove images (or leave them as NA, if comented out):
#        05 II 2018-07-05
#----------------------------------------------------------------------------------------
ringWidths [ringWidths [['treeId']] == 5 & 
            ringWidths [['sampleHeight']] == 1.5 & 
            ringWidths [['sampleDate']] %in% as_date (c ('2018-07-19','2018-08-02','2018-08-16')), 'Y2018'] <- NA
ringWidths [ringWidths [['treeId']] == 5 & 
            ringWidths [['sampleHeight']] == 2.5 & 
            ringWidths [['sampleDate']] %in% as_date (c ('2018-08-30')), 'Y2018'] <- NA


# Standardise ring width using the 2015 ring
#----------------------------------------------------------------------------------------
ringWidths <- ringWidths %>% mutate (RWI2019 = Y2019 / Y2017,
                                     RWI2018 = Y2018 / Y2017,
                                     RWI2019_16 = Y2019 / Y2016,
                                     RWI2018_16 = Y2018 / Y2016,
                                     RWI2017_16 = Y2017 / Y2016,
                                     RWI2019_15 = Y2019 / Y2015,
                                     RWI2018_15 = Y2018 / Y2015,
                                     RWI2017_15 = Y2017 / Y2015,
                                     RWI2016_15 = Y2016 / Y2015)

# Summarise growth
#----------------------------------------------------------------------------------------
summaryData <- ringWidths %>% group_by (treatment, sampleDate, sampleHeight) %>% 
  dplyr::summarise (meanY19 = mean (Y2019, na.rm = TRUE),
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
             meanRWI2019_16 = mean (RWI2019_16, na.rm = TRUE),
             seRWI2019_16   = se (RWI2019_16),
             meanRWI2018_16 = mean (RWI2018_16, na.rm = TRUE),
             seRWI2018_16   = se (RWI2018_16),
             meanRWI2017_16 = mean (RWI2017_16, na.rm = TRUE),
             seRWI2017_16   = se (RWI2017_16),
             meanRWI2019_15 = mean (RWI2019_15, na.rm = TRUE),
             seRWI2019_15   = se (RWI2019_15),
             meanRWI2018_15 = mean (RWI2018_15, na.rm = TRUE),
             seRWI2018_15   = se (RWI2018_15),
             meanRWI2017_15 = mean (RWI2017_15, na.rm = TRUE),
             seRWI2017_15   = se (RWI2017_15),
             meanRWI2016_15 = mean (RWI2016_15, na.rm = TRUE),
             seRWI2016_15   = se (RWI2016_15)) 

# Plot relative ring width for a tree over time 
#----------------------------------------------------------------------------------------
PLOT <- 'FALSE'
if (PLOT) {
  layout (matrix (1:15, nrow = 3, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1), 
          heights = c (1, 1, 1.3))
    for (i in 1:15) {
    
    # Condition to extract relevant data
    con <- ringWidths [['treeId']] == i & ringWidths [['sampleHeight']] == 0.5
    
    # Extract treatment
    t <- unique (ringWidths [['treatment']] [con]) 
    
    # Plot data for 0.5 m
    if (i %% 5 == 1 & i <= 10) {
      par (mar = c (1, 5, 1, 1))
    } else if (i %% 5 == 1 & i > 10) {
      par (mar = c (5, 5, 1, 1))
    } else if (i <= 10) {
      par (mar = c (1, 1, 1, 1))
    } else if (i > 10) {
      par (mar = c (5, 1, 1, 1))
    }
    plot (x = ringWidths [['sampleDate']] [con],
          y = ringWidths [['RWI2018']] [con],
          xlab = '', ylab = '', las = 1, typ = 'p', pch = 25,
          col = tColours [['colour']] [tColours [['treatment']] == ifelse (t == 1, 'control',ifelse (t == 4, 'double compressed', 'chilled'))],
          xlim = as_date (c ('2018-01-01','2019-02-01')), ylim = c (0, 2.7), axes = FALSE)
    axis (side = 1, at = as_date (c ('2018-02-01','2018-03-01','2018-04-01','2018-05-01','2018-06-01','2018-07-01','2018-08-01','2018-09-01','2018-10-01','2018-11-01','2018-12-01')), 
          labels = c ('F','M','A','M','J','J','A','S','O','N','D'))
    axis (side = 2, las = 1)
    axis (side = 1, at = as_date ('2019-01-15'), labels = 'C')
    if (i %% 5 == 1) mtext (text = 'Growth increment index', side = 2, line = 3, cex = 0.8)
    if (i > 10)      mtext (text = 'Date', side = 1, line = 3, cex = 0.8)
    
    # Add panel descriptor
    text (x = as_date ('2018-12-10'),
          y = 2.55,
          labels = i, cex = 1.4)
    
    # Add data for 1.5m
    con <- ringWidths [['treeId']] == i & ringWidths [['sampleHeight']] == 1.5
    points (x = ringWidths [['sampleDate']] [con],
            y = ringWidths [['RWI2018']] [con], pch = 23,
            col = tColours [['colour']] [tColours [['treatment']] == ifelse (t == 1, 'control',ifelse (t == 4, 'double compressed', 'chilled'))])
    
    # Add data for 2.5m
    con <- ringWidths [['treeId']] == i & ringWidths [['sampleHeight']] == 2.5
    points (x = ringWidths [['sampleDate']] [con],
            y = ringWidths [['RWI2018']] [con], pch = 24,
            col = tColours [['colour']] [tColours [['treatment']] == ifelse (t == 1, 'control',ifelse (t == 4, 'double compressed', 'chilled'))])
    
    # Add data for 4.0m
    con <- ringWidths [['treeId']] == i & ringWidths [['sampleHeight']] == 4.0
    points (x = ringWidths [['sampleDate']] [con],
            y = ringWidths [['RWI2018']] [con], pch = 21,
            col = tColours [['colour']] [tColours [['treatment']] == ifelse (t == 1, 'control',ifelse (t == 4, 'double compressed', 'chilled'))])
   
    # Add the 2018 growth increment index for the from the 2019 sample
    con <- ringWidths [['treeId']] == i & ringWidths [['sampleDate']] == as_date ('2019-10-24')
    points (x = rep (as_date ('2019-01-15'), 4),
            y = ringWidths [['RWI2018']] [con], pch = c (25, 23, 24, 21),
            col = tColours [['colour']] [tColours [['treatment']] == ifelse (t == 1, 'control',ifelse (t == 4, 'double compressed', 'chilled'))])
  }

  # Add a legend 
  #--------------------------------------------------------------------------------------
  legend (x = as_date ('2018-02-01'),
          y = 2.6, box.lty = 0, pch = c (21, 24, 23, 25), 
          legend = c ('4.0m','2.5m','1.5m','0.5m'))
}

# reset original working directory
#----------------------------------------------------------------------------------------
setwd (originalDir)

# Clean unnecessary variables from loop
#----------------------------------------------------------------------------------------
rm (PLOT, temp, treeID, t, i, j, k, jsonFiles, sampleDate, sampleHeight, growth, types, 
    years, con, len, summaryData, growingSeason, sampleH2, yPositions, h, originalDir, 
    VERBOSE)
#========================================================================================