# Script to read in the TRIAD rin width measurements
#----------------------------------------------------------------------------------------

# Comments:
#    - sample 03 I has only the 2018 and 2017 ring complete. Maybe there is a second sample we could cut?


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
  if (temp$sampleYearGrowth %notin% c ('some','all')) stop ('Error: Sample year growth is not "some" or "all"!')  
  if (temp$sampleDPI !=  38100)        stop ('Error: DPI is not 38100!')  
  if (!temp$barkFirst [[1]])           stop ('Error: Bark was not first!')  
  if (temp$siteLocID != 'BigChill')    stop ('Error: Site location ID was not "BigChill"!')
  if (temp$plotID %notin% c (1,4,5))   stop ('Error: Plot ID is not correct.')
  if (temp$collection != 'Experiment 2018')  stop ('Error: Collection is not "Experiment 2018"')

  # For now jump measurements from November 2018 or compressed trees
  #--------------------------------------------------------------------------------------
  if (sampleDate == '2018-11-15') next
  if (sampleDate == '2019-10-24' & growingSeason != 'all') {
    stop (paste0 ('Error with growing season',treeID,growingSeason))
  } else if ((sampleDate == '2018-06-19' | sampleDate == '2018-09-06') & growingSeason != 'some') {
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


# Standardise ring width using the 2015 ring
#----------------------------------------------------------------------------------------
ringWidths <- ringWidths %>% mutate (RWI12019 = Y2019 / Y2015,
                                     RWI12018 = Y2018 / Y2015,
                                     RWI12017 = Y2017 / Y2015,
                                     RWI22019 = Y2019 / Y2017,
                                     RWI22018 = Y2018 / Y2017,
                                     RWI22017 = Y2017 / Y2017)

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
             meanRWI12019 = mean (RWI12019, na.rm = TRUE),
             seRWI12019   = se (RWI12019),
             meanRWI12018 = mean (RWI12018, na.rm = TRUE),
             seRWI12018   = se (RWI12018),
             meanRWI12017 = mean (RWI12017, na.rm = TRUE),
             seRWI12017   = se (RWI12017),
             meanRWI22019 = mean (RWI22019, na.rm = TRUE),
             seRWI22019   = se (RWI22019),
             meanRWI22018 = mean (RWI22018, na.rm = TRUE),
             seRWI22018   = se (RWI22018),
             meanRWI22017 = mean (RWI22017, na.rm = TRUE),
             seRWI22017   = se (RWI22017)) 

# Plot ring width in 2018 and 2019in two plots next to each other
layout (matrix (1:3, byrow = TRUE, nrow = 1), widths = c (1.15, 1, 1))
par (mar = c (5, 5, 1, 1))
barplot (height = summaryData [['meanRWI12017']] [summaryData [['sampleDate']] == as_date ('2019-10-24')] [c (1, 5, 2, 6, 3, 7, 4, 8)],
         las = 1, xlab = 'Ring width fraction', ylab = 'Height (m)', axes = FALSE, horiz = TRUE, main = '', 
         col = addOpacity (rep (c (tColours [['colour']] [1], tColours[['colour']] [5]), 4), 0.9),
         border = 0, space = c (rep (c (2, 1), 3), 3, 1), xlim = c (0, 1.2), ylim = c (0, 26))
axis (side = 1, at = seq (0, 1, by = 0.5))
axis (side = 2, at = c (3.5),  labels = c (0.5), las = 1)
axis (side = 2, at = c (8.5),  labels = c (1.5), las = 1)
axis (side = 2, at = c (13.5), labels = c (2.5), las = 1)
axis (side = 2, at = c (19.5), labels = c (4.0), las = 1)
text (x = 0, y = 25, '2017', pos = 4, cex = 1.6)

par (mar = c (5, 1, 1, 1))
barplot (height = summaryData [['meanRWI12018']] [summaryData [['sampleDate']] == as_date ('2019-10-24')] [c (1, 5, 2, 6, 3, 7, 4, 8)],
         las = 1, xlab = 'Ring width fraction', ylab = 'Height (m)', axes = FALSE, horiz = TRUE, main = '', 
         col = addOpacity (rep (c (tColours [['colour']] [1], tColours[['colour']] [5]), 4), 0.3),
         border = 0, space = c (rep (c (2, 1), 3), 3, 1), xlim = c (0, 1.2), ylim = c (0, 26))
barplot (height = summaryData [['meanRWI12018']] [summaryData [['sampleDate']] == as_date ('2018-09-06')] [c (1, 5, 2, 6, 3, 7, 4, 8)],
         las = 1, xlab = '', ylab = '', axes = FALSE, horiz = TRUE, main = '', 
         col = addOpacity (rep (c (tColours [['colour']] [1], tColours[['colour']] [5]), 4), 0.6),
         border = 0, space = c (rep (c (2, 1), 3), 3, 1), add = TRUE)
barplot (height = summaryData [['meanRWI12018']] [summaryData [['sampleDate']] == as_date ('2018-06-19')] [c (1, 5, 2, 6, 3, 7, 4, 8)],
         las = 1, xlab = '', ylab = '', axes = FALSE, horiz = TRUE, main = '', 
         col = addOpacity (rep (c (tColours [['colour']] [1], tColours[['colour']] [5]), 4), 0.9),
         border = 0, space = c (rep (c (2, 1), 3), 3, 1), add = TRUE)
axis (side = 1, at = seq (0, 1, by = 0.5))
text (x = 0, y = 25, '2018', pos = 4, cex = 1.6)

# Add second graph for 2019 ring widths
par (mar =  c (5, 1, 1, 2))
barplot (height = summaryData [['meanRWI12019']] [summaryData [['sampleDate']] == as_date ('2019-10-24')] [c (1, 5, 2, 6, 3, 7, 4, 8)],
         las = 1, xlab = 'Ring width fraction', ylab = '', axes = FALSE, horiz = TRUE, main = '', 
         col = addOpacity (rep (c (tColours [['colour']] [1], tColours[['colour']] [5]), 4), 0.3),
         border = 0, space = c (rep (c (2, 1), 3), 3, 1), xlim = c (0, 1.2), ylim = c (0, 26))
axis (side = 1, at = seq (0, 1, by = 0.5))
text (x = 0, y = 25, '2019', pos = 4, cex = 1.6)
legend (x = 0.42, y = 25.5, pch = 0, legend = c ('chilled','control'), cex = 1, 
        col = 'white', box.lty = 0, bg = 'transparent')
legend (x = 0.66, y = 25.5, pch = 15, legend = '', cex = 2.0, 
        col = addOpacity (tColours [['colour']] [1], 0.9), box.lty = 0, bg = 'transparent')
legend (x = 0.66, y = 26.5, pch = 15, legend = '', cex = 2.0, 
        col = addOpacity (tColours [['colour']] [5], 0.9), box.lty = 0, bg = 'transparent')
legend (x = 0.82, y = 25.5, pch = 15, legend = '', cex = 2.0, 
        col = addOpacity (tColours [['colour']] [1], 0.6), box.lty = 0, bg = 'transparent')
legend (x = 0.82, y = 26.5, pch = 15, legend = '', cex = 2.0, 
        col = addOpacity (tColours [['colour']] [5], 0.6), box.lty = 0, bg = 'transparent')
legend (x = 0.99, y = 25.5, pch = 15, legend = '', cex = 2.0, 
        col = addOpacity (tColours [['colour']] [1], 0.3), box.lty = 0, bg = 'transparent')
legend (x = 0.99, y = 26.5, pch = 15, legend = '', cex = 2.0, 
        col = addOpacity (tColours [['colour']] [5], 0.3), box.lty = 0, bg = 'transparent')
text (x = 0.74, y = 25.9, cex = 0.9, 'before')
text (x = 0.90, y = 25.8, cex = 0.9, 'during')
text (x = 1.07, y = 25.9, cex = 0.9, 'after')

# Pivot data into long format
#----------------------------------------------------------------------------------------
data2018 <- dataFollowUp %>% pivot_longer ()

# Clean unnecessary variables from loop
#----------------------------------------------------------------------------------------
rm (temp, treeID, t, i, j, k, jsonFiles, sampleDate, sampleHeight, growth, types, years, 
    con, len)
