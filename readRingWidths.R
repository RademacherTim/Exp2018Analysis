# Script to read in the TRIAD rin width measurements
#----------------------------------------------------------------------------------------

# Comments:
#    - sample 03 I, II has only the 2018 and 2017 ring complete. Maybe there is a second sample we could cut?
#    - sample 07 III has a density anomaly in 2018 in the slide from 2019-09-24


# To-do:
#    - Check whether outliers are measurements issues
#    - fit growth curves


# Load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')
library ('rjson')

# create %notin% funtion
#----------------------------------------------------------------------------------------
`%notin%` <- Negate (`%in%`)

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
  
  # check metadata
  #--------------------------------------------------------------------------------------
  if (temp$status != 'Confirmed')      stop ('Error: Metadata status was not confirmed!')  
  if (temp$species != 'Pinus strobus') stop ('Error: Species is not Pinus strobus!') 
  if (temp$sampleYearGrowth %notin% c ('none','some','all')) {
    stop ('Error: Sample year growth is not "some" or "all"!')  
  }
  if (temp$sampleDPI !=  38100)        stop ('Error: DPI is not 38100!')  
  if (!temp$barkFirst [[1]])           stop ('Error: Bark was not first!')  
  if (temp$siteLocID != 'BigChill')    stop ('Error: Site location ID was not "BigChill"!')
  if (temp$plotID %notin% c (1,4,5))   stop ('Error: Plot ID is not correct.')
  if (temp$collection != 'Experiment 2018')  stop ('Error: Collection is not "Experiment 2018"')
  if ((temp$plotID == '5' & as.numeric (substr (temp$sampleID, 1, 2)) %notin% 1:5) |
      (temp$plotID == '4' & as.numeric (substr (temp$sampleID, 1, 2)) %notin% 6:10) |
      (temp$plotID == '1' & as.numeric (substr (temp$sampleID, 1, 2)) %notin% 11:15)){
    stop ('Error: Treatment is not correct!')
  }
  
  # For now jump measurements from November 2018 or compressed trees
  #--------------------------------------------------------------------------------------
  if ((sampleDate == '2019-10-24' | sampleDate == '2018-11-15') & growingSeason != 'all') {
    stop (paste0 ('Error with growing season',treeID,growingSeason))
  } else if ((sampleDate == '2018-06-19' | sampleDate == '2018-07-19' | sampleDate == '2018-09-06') & growingSeason != 'some') {
    stop (paste0 ('Error with growing season',treeID,growingSeason))
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

}  # end json file loop

# Add 60 lines for no growth in the beginning of May
#----------------------------------------------------------------------------------------
for (i in 1:15) {
  for (h in c (0.5, 1.5, 2.5, 4.0)) {
    # Condition to extract relevant data
    con <- ringWidths [['treeId']] == i & ringWidths [['sampleHeight']] == h
    
    # Extract treatment
    t <- unique (ringWidths [['treatment']] [con]) 
    ringWidths <- ringWidths %>% add_row (treeId = i, treatment = t, 
                                          sampleDate = as_date ('2018-05-01'),
                                          sampleHeight = h, Y2019 = NA, Y2018 = 0, 
                                          Y2017 = 1, Y2016 = 1, Y2015 = 1, Y2014 = 1, 
                                          Y2013 = 1, Y2012 = 1, Y2011 = 1)    
  }
}

# Arrange in advacning order by date, tree, sampling height
#----------------------------------------------------------------------------------------
ringWidths <- ringWidths %>% arrange (sampleDate, treeId, sampleHeight)

# Reset working directory
#----------------------------------------------------------------------------------------
setwd ('~/projects/PlantGrowth/Exp2018Analysis/')

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

# Plot ring width in 2018 and 2019in two plots next to each other
# layout (matrix (1:3, byrow = TRUE, nrow = 1), widths = c (1.15, 1, 1))
# par (mar = c (5, 5, 1, 1))
# barplot (height = summaryData [['meanRWI22017']] [summaryData [['sampleDate']] == as_date ('2019-10-24')] [c (1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12)],
#          las = 1, xlab = 'Ring width fraction', ylab = 'Height (m)', axes = FALSE, horiz = TRUE, main = '', 
#          col = addOpacity (rep (tColours [['colour']] [c (1, 4, 5)], 4), 0.9),
#          border = 0, space = c (rep (c (2, 1, 1), 3), 3, 1, 1), xlim = c (0, 1.3), ylim = c (0, 33))
# axis (side = 1, at = seq (0, 1, by = 0.5))
# axis (side = 2, at = 4.5,  labels = c (0.5), las = 1)
# axis (side = 2, at = c (11.5),  labels = c (1.5), las = 1)
# axis (side = 2, at = c (18.5), labels = c (2.5), las = 1)
# axis (side = 2, at = c (26.5), labels = c (4.0), las = 1)
# text (x = 0, y = 31, '2017', pos = 4, cex = 1.6)
# 
# par (mar = c (5, 1, 1, 1))
# barplot (height = summaryData [['meanRWI22018']] [summaryData [['sampleDate']] == as_date ('2019-10-24')] [c (1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12)],
#          las = 1, xlab = 'Ring width fraction', ylab = 'Height (m)', axes = FALSE, horiz = TRUE, main = '', 
#          col = addOpacity (rep (tColours [['colour']] [c (1, 4, 5)], 4), 0.2),
#          border = 0, space = c (rep (c (2, 1, 1), 3), 3, 1, 1), xlim = c (0, 1.3), ylim = c (0, 33))
# barplot (height = summaryData [['meanRWI22018']] [summaryData [['sampleDate']] == as_date ('2018-11-15')] [c (1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12)],
#          las = 1, xlab = '', ylab = '', axes = FALSE, horiz = TRUE, main = '', 
#          col = addOpacity (rep (tColours [['colour']] [c (1, 4, 5)], 4), 0.4),
#          border = 0, space = c (rep (c (2, 1, 1), 3), 3, 1, 1), add = TRUE)
# barplot (height = summaryData [['meanRWI22018']] [summaryData [['sampleDate']] == as_date ('2018-09-06')] [c (1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12)],
#          las = 1, xlab = '', ylab = '', axes = FALSE, horiz = TRUE, main = '', 
#          col = addOpacity (rep (tColours [['colour']] [c (1, 4, 5)], 4), 0.6),
#          border = 0, space = c (rep (c (2, 1, 1), 3), 3, 1, 1), add = TRUE)
# barplot (height = summaryData [['meanRWI22018']] [summaryData [['sampleDate']] == as_date ('2018-07-19')] [c (1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12)],
#          las = 1, xlab = '', ylab = '', axes = FALSE, horiz = TRUE, main = '', 
#          col = addOpacity (rep (tColours [['colour']] [c (1, 4, 5)], 4), 0.8),
#          border = 0, space = c (rep (c (2, 1, 1), 3), 3, 1, 1), add = TRUE)
# barplot (height = summaryData [['meanRWI22018']] [summaryData [['sampleDate']] == as_date ('2018-06-19')] [c (1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12)],
#          las = 1, xlab = '', ylab = '', axes = FALSE, horiz = TRUE, main = '', 
#          col = addOpacity (rep (tColours [['colour']] [c (1, 4, 5)], 4), 1.0),
#          border = 0, space = c (rep (c (2, 1, 1), 3), 3, 1, 1), add = TRUE)
# axis (side = 1, at = seq (0, 1, by = 0.5))
# text (x = 0, y = 31, '2018', pos = 4, cex = 1.6)
# 
# # Add second graph for 2019 ring widths
# par (mar =  c (5, 1, 1, 2))
# barplot (height = summaryData [['meanRWI22019']] [summaryData [['sampleDate']] == as_date ('2019-10-24')] [c (1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12)],
#          las = 1, xlab = 'Ring width fraction', ylab = '', axes = FALSE, horiz = TRUE, main = '', 
#          col = addOpacity (rep (tColours [['colour']] [c (1, 4, 5)], 4), 0.3),
#          border = 0, space = c (rep (c (2, 1, 1), 3), 3, 1, 1), xlim = c (0, 1.3), ylim = c (0, 33))
# axis (side = 1, at = seq (0, 1, by = 0.5))
# text (x = 0, y = 31, '2019', pos = 4, cex = 1.6)
# legend (x = 0.29, y = 33.5, pch = 0, legend = c ('chilled','compressed','control'), cex = 1, 
#         col = 'white', box.lty = 0, bg = 'transparent')
# legend (x = 0.66, y = 32.5, pch = 15, legend = '', cex = 2.0, 
#         col = addOpacity (tColours [['colour']] [1], 0.9), box.lty = 0, bg = 'transparent')
# legend (x = 0.66, y = 33.5, pch = 15, legend = '', cex = 2.0, 
#         col = addOpacity (tColours [['colour']] [4], 0.9), box.lty = 0, bg = 'transparent')
# legend (x = 0.66, y = 34.5, pch = 15, legend = '', cex = 2.0, 
#         col = addOpacity (tColours [['colour']] [5], 0.9), box.lty = 0, bg = 'transparent')
# 
# legend (x = 0.82, y = 32.5, pch = 15, legend = '', cex = 2.0, 
#         col = addOpacity (tColours [['colour']] [1], 0.6), box.lty = 0, bg = 'transparent')
# legend (x = 0.82, y = 33.5, pch = 15, legend = '', cex = 2.0, 
#         col = addOpacity (tColours [['colour']] [4], 0.6), box.lty = 0, bg = 'transparent')
# legend (x = 0.82, y = 34.5, pch = 15, legend = '', cex = 2.0, 
#         col = addOpacity (tColours [['colour']] [5], 0.6), box.lty = 0, bg = 'transparent')
# 
# 
# legend (x = 0.99, y = 32.5, pch = 15, legend = '', cex = 2.0, 
#         col = addOpacity (tColours [['colour']] [1], 0.6), box.lty = 0, bg = 'transparent')
# legend (x = 0.99, y = 33.5, pch = 15, legend = '', cex = 2.0, 
#         col = addOpacity (tColours [['colour']] [4], 0.3), box.lty = 0, bg = 'transparent')
# legend (x = 0.99, y = 34.5, pch = 15, legend = '', cex = 2.0, 
#         col = addOpacity (tColours [['colour']] [5], 0.3), box.lty = 0, bg = 'transparent')
# text (x = 0.74, y = 33.9, cex = 0.9, 'before')
# text (x = 0.90, y = 33.8, cex = 0.9, 'during')
# text (x = 1.07, y = 33.9, cex = 0.9, 'after')

# Plot relative ring width for a tree over time 
#----------------------------------------------------------------------------------------
layout (matrix (1:15, nrow = 3, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1))
for (i in 1:15) {
  
  # Condition to extract relevant data
  con <- ringWidths [['treeId']] == i & ringWidths [['sampleHeight']] == 0.5
  
  # Extract treatment
  t <- unique (ringWidths [['treatment']] [con]) 
  
  # Plot data for 0.5 m
  par (mar = c (5, 5, 1, 1))
  plot (x = ringWidths [['sampleDate']] [con],
        y = ringWidths [['RWI2018']] [con],
        xlab = 'date', ylab = 'Growth increment index', las = 1, typ = 'p', pch = 25,
        col = tColours [['colour']] [tColours [['treatment']] == ifelse (t == 1, 'control',ifelse (t == 4, 'double compressed', 'chilled'))],
        xlim = as_date (c ('2018-01-01','2019-01-01')), ylim = c (0, 2.7))
  
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
        
}

# Pivot data into long format
#----------------------------------------------------------------------------------------
data2018 <- dataFollowUp %>% pivot_longer ()

# Clean unnecessary variables from loop
#----------------------------------------------------------------------------------------
rm (temp, treeID, t, i, j, k, jsonFiles, sampleDate, sampleHeight, growth, types, years, 
    con, len)
