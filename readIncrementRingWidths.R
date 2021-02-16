# Script to read in the TRIAD rin width measurements
#----------------------------------------------------------------------------------------

# Start from clean slate
#----------------------------------------------------------------------------------------
#rm (list = ls ())

# Load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')
library ('rjson')
source ('plotingFunctions.R')

# create %notin% funtion
#----------------------------------------------------------------------------------------
`%notin%` <- Negate (`%in%`)

# Set working directory to read json files for the follow-up microcores
#----------------------------------------------------------------------------------------
setwd ('/media/tim/dataDisk/PlantGrowth/data/incrementCores/ringWidths/Exp2018/')

# List all json output file from TRIAD
#----------------------------------------------------------------------------------------
jsonFiles <- list.files (path = './', pattern = '.json')

# Create tibble with ring measurements from the 2018 micrcore 
#----------------------------------------------------------------------------------------
incrementRingWidths <- tibble (treeId = numeric (), treatment = numeric (), 
  sampleDate = as_date (NA), sampleHeight = numeric (), 
  Y2020 = numeric (), Y2019 = numeric (), Y2018 = numeric (), Y2017 = numeric (), 
  Y2016 = numeric (), Y2015 = numeric (), Y2014 = numeric (), Y2013 = numeric (), 
  Y2012 = numeric (), Y2011 = numeric (), Y2010 = numeric (), Y2009 = numeric (), 
  Y2008 = numeric (), Y2007 = numeric (), Y2006 = numeric (), Y2005 = numeric (), 
  Y2004 = numeric (), Y2003 = numeric (), Y2002 = numeric (), Y2001 = numeric (), 
  Y2000 = numeric (), Y1999 = numeric (), Y1998 = numeric (), Y1997 = numeric (), 
  Y1996 = numeric (), Y1995 = numeric (), Y1994 = numeric (), Y1993 = numeric (), 
  Y1992 = numeric (), Y1991 = numeric (), Y1990 = numeric (), Y1989 = numeric (), 
  Y1988 = numeric (), Y1987 = numeric (), Y1986 = numeric (), Y1985 = numeric (), 
  Y1984 = numeric (), Y1983 = numeric (), Y1982 = numeric (), Y1981 = numeric (), 
  Y1980 = numeric (), Y1979 = numeric (), Y1978 = numeric (), Y1977 = numeric (), 
  Y1976 = numeric (), Y1975 = numeric (), Y1974 = numeric (), Y1973 = numeric (), 
  Y1972 = numeric (), Y1971 = numeric (), Y1970 = numeric (), Y1969 = numeric (), 
  Y1968 = numeric (), Y1967 = numeric (), Y1966 = numeric (), Y1965 = numeric (), 
  Y1964 = numeric (), Y1963 = numeric (), Y1962 = numeric (), Y1961 = numeric (), 
  Y1960 = numeric (), Y1959 = numeric (), Y1958 = numeric (), Y1957 = numeric (), 
  Y1956 = numeric (), Y1955 = numeric (), Y1954 = numeric (), Y1953 = numeric (), 
  Y1952 = numeric (), Y1951 = numeric (), Y1950 = numeric (), Y1949 = numeric (), 
  Y1948 = numeric (), Y1947 = numeric (), Y1946 = numeric (), Y1945 = numeric (), 
  Y1944 = numeric (), Y1943 = numeric (), Y1942 = numeric (), Y1941 = numeric (), 
  Y1940 = numeric (), Y1939 = numeric (), Y1938 = numeric (), Y1937 = numeric (), 
  Y1936 = numeric (), Y1935 = numeric (), Y1934 = numeric (), Y1933 = numeric (), 
  Y1932 = numeric (), Y1931 = numeric (), Y1930 = numeric (), Y1929 = numeric (), 
  Y1928 = numeric (), Y1927 = numeric (), Y1926 = numeric (), Y1925 = numeric (), 
  Y1924 = numeric (), Y1923 = numeric (), Y1922 = numeric (), Y1921 = numeric (), 
  Y1920 = numeric (), Y1919 = numeric (), Y1918 = numeric (), Y1917 = numeric (), 
  Y1916 = numeric (), Y1915 = numeric (), Y1914 = numeric (), Y1913 = numeric (), 
  Y1912 = numeric (), Y1911 = numeric (), Y1910 = numeric (), Y1909 = numeric (), 
  Y1908 = numeric (), Y1907 = numeric (), Y1906 = numeric (), Y1905 = numeric (), 
  Y1904 = numeric (), Y1903 = numeric (), Y1902 = numeric (), Y1901 = numeric (), 
  Y1900 = numeric ())

k <- 0
# Loop over json files and read them
#----------------------------------------------------------------------------------------
for (j in 1: length (jsonFiles)){
  
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
  if (temp$sampleDPI %notin%  c (2400, 3200)) stop ('Error: DPI is not 2400 or 3200!')  
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
  if (sampleDate == '2020-08-04' & growingSeason != 'some') {
    stop (paste0 ('Error with growing season',treeID,growingSeason))
  }
  k <- k + 1
  #print (c (len, treeID, sampleHeight, format (sampleDate, '%Y-%m-%d'), t))
  
  # Check that sample Height were entered correctly
  #--------------------------------------------------------------------------------------
  profileID <- as.numeric (substr (temp [['sampleID']], 4, 6))
  if (profileID %notin% 1:4) {
    stop (paste0 ('Error with profile ID ',treeID,profileID)) 
  }
  
  # Sort out the ones that have not been checked yet for now
  #--------------------------------------------------------------------------------------
  if ((treeID %in% c (9, 13, 15)) | 
      (treeID %in% c (5, 10, 12, 14) & profileID == 1) | 
      (treeID %in% c (8, 12)         & profileID == 2)) next
  
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
  
  # Divide into two profiles if there is a pith marker
  #--------------------------------------------------------------------------------------
  nProfiles <- 1
  if ('Pith' %in% types) {
    nProfiles <- 2
    oldestRingIndex <- which (types == 'Pith')
    growth2 <- growth [(oldestRingIndex + 1):len]
    years2  <- years [(oldestRingIndex + 1):len]
    types2  <- types [(oldestRingIndex + 1):len]
    growth <- growth [1:oldestRingIndex]
    years  <- years [1:oldestRingIndex]
    types  <- types [1:oldestRingIndex]
  }
  
  # Wrangle data
  #--------------------------------------------------------------------------------------
  growth <- as.numeric (growth [types %in% c ('Normal','Missing','Pith') & !is.na (years)])
  years  <- as.numeric (years  [types %in% c ('Normal','Missing','Pith') & !is.na (years)])
  if (nProfiles == 2) {
    growth2 <- as.numeric (growth2 [types2 %in% c ('Normal','Missing') & !is.na (years2)])
    years2  <- as.numeric (years2  [types2 %in% c ('Normal','Missing') & !is.na (years2)])
  }
  if (years [1] == 2020) {
    growth <- c (growth, rep (NA, 119-length (growth)))
    years  <- c (years,  seq (years [length (years)]-1, 1900))
    if (nProfiles == 2) {
      # Determine the most recent year in second profile
      mostRecentYear <- max (years2)
      oldestYear <- min (years2)
      growth2 <- c (rep (NA, 2020-mostRecentYear+1), rev (growth2), rep (NA, 119-length (growth2)))
      years2  <- c (seq (2020, mostRecentYear+1), rev (years2),  seq (oldestYear-1, 1900))
    }
  }# else if (years [1] == 2017) {
  #  growth <- c (NA, growth, rep (NA, 118-length (growth)))
  #  years  <- c (2018, years,  seq (years [length (years)]-1, 1900))
  #}
  #print (growth)
  #print (years)
  
  # add to tibble with all growth for all years years
  #--------------------------------------------------------------------------------------
  incrementRingWidths <- incrementRingWidths %>% 
    add_row (treeId = treeID, treatment = t, sampleHeight = sampleHeight,
             sampleDate = sampleDate,
             Y2020 = growth [years == 2020], Y2019 = growth [years == 2019],
             Y2018 = growth [years == 2018], Y2017 = growth [years == 2017],
             Y2016 = growth [years == 2016], Y2015 = growth [years == 2015],
             Y2014 = growth [years == 2014], Y2013 = growth [years == 2013],
             Y2012 = growth [years == 2012], Y2011 = growth [years == 2011],
             Y2010 = growth [years == 2010], Y2009 = growth [years == 2009],
             Y2008 = growth [years == 2008], Y2007 = growth [years == 2007],
             Y2006 = growth [years == 2006], Y2005 = growth [years == 2005],
             Y2004 = growth [years == 2004], Y2003 = growth [years == 2003],
             Y2002 = growth [years == 2002], Y2001 = growth [years == 2001],
             Y2000 = growth [years == 2000], Y1999 = growth [years == 1999],
             Y1998 = growth [years == 1998], Y1997 = growth [years == 1997],
             Y1996 = growth [years == 1996], Y1995 = growth [years == 1995],
             Y1994 = growth [years == 1994], Y1993 = growth [years == 1993],
             Y1992 = growth [years == 1992], Y1991 = growth [years == 1991],
             Y1990 = growth [years == 1990], Y1989 = growth [years == 1989],
             Y1988 = growth [years == 1988], Y1987 = growth [years == 1987],
             Y1986 = growth [years == 1986], Y1985 = growth [years == 1985],
             Y1984 = growth [years == 1984], Y1983 = growth [years == 1983],
             Y1982 = growth [years == 1982], Y1981 = growth [years == 1981],
             Y1980 = growth [years == 1980], Y1979 = growth [years == 1979],
             Y1978 = growth [years == 1978], Y1977 = growth [years == 1977],
             Y1976 = growth [years == 1976], Y1975 = growth [years == 1975],
             Y1974 = growth [years == 1974], Y1973 = growth [years == 1973],
             Y1972 = growth [years == 1972], Y1971 = growth [years == 1971],
             Y1970 = growth [years == 1970], Y1969 = growth [years == 1969],
             Y1968 = growth [years == 1968], Y1967 = growth [years == 1967],
             Y1966 = growth [years == 1966], Y1965 = growth [years == 1965],
             Y1964 = growth [years == 1964], Y1963 = growth [years == 1963],
             Y1962 = growth [years == 1962], Y1961 = growth [years == 1961],
             Y1960 = growth [years == 1960], Y1959 = growth [years == 1959],
             Y1958 = growth [years == 1958], Y1957 = growth [years == 1957],
             Y1956 = growth [years == 1956], Y1955 = growth [years == 1955],
             Y1954 = growth [years == 1954], Y1953 = growth [years == 1953],
             Y1952 = growth [years == 1952], Y1951 = growth [years == 1951],
             Y1950 = growth [years == 1950], Y1949 = growth [years == 1949],
             Y1948 = growth [years == 1948], Y1947 = growth [years == 1947],
             Y1946 = growth [years == 1946], Y1945 = growth [years == 1945],
             Y1944 = growth [years == 1944], Y1943 = growth [years == 1943],
             Y1942 = growth [years == 1942], Y1941 = growth [years == 1941],
             Y1940 = growth [years == 1940], Y1939 = growth [years == 1939],
             Y1938 = growth [years == 1938], Y1937 = growth [years == 1937],
             Y1936 = growth [years == 1936], Y1935 = growth [years == 1935],
             Y1934 = growth [years == 1934], Y1933 = growth [years == 1933],
             Y1932 = growth [years == 1932], Y1931 = growth [years == 1931],
             Y1930 = growth [years == 1930], Y1929 = growth [years == 1929],
             Y1928 = growth [years == 1928], Y1927 = growth [years == 1927],
             Y1926 = growth [years == 1926], Y1925 = growth [years == 1925],
             Y1924 = growth [years == 1924], Y1923 = growth [years == 1923],
             Y1922 = growth [years == 1922], Y1921 = growth [years == 1921],
             Y1920 = growth [years == 1920], Y1919 = growth [years == 1919],
             Y1918 = growth [years == 1918], Y1917 = growth [years == 1917],
             Y1916 = growth [years == 1916], Y1915 = growth [years == 1915],
             Y1914 = growth [years == 1914], Y1913 = growth [years == 1913],
             Y1912 = growth [years == 1912], Y1911 = growth [years == 1911],
             Y1910 = growth [years == 1910], Y1909 = growth [years == 1909],
             Y1908 = growth [years == 1908], Y1907 = growth [years == 1907],
             Y1906 = growth [years == 1906], Y1905 = growth [years == 1905],
             Y1904 = growth [years == 1904], Y1903 = growth [years == 1903],
             Y1902 = growth [years == 1902], Y1901 = growth [years == 1901],
             Y1900 = growth [years == 1900])
  
  # Add secon profile if it exists
  #--------------------------------------------------------------------------------------
  if (nProfiles == 2) {
    incrementRingWidths <- incrementRingWidths %>% 
      add_row (treeId = treeID, treatment = t, sampleHeight = sampleHeight,
               sampleDate = sampleDate,
               Y2020 = growth2 [years2 == 2020], Y2019 = growth2 [years2 == 2019],
               Y2018 = growth2 [years2 == 2018], Y2017 = growth2 [years2 == 2017],
               Y2016 = growth2 [years2 == 2016], Y2015 = growth2 [years2 == 2015],
               Y2014 = growth2 [years2 == 2014], Y2013 = growth2 [years2 == 2013],
               Y2012 = growth2 [years2 == 2012], Y2011 = growth2 [years2 == 2011],
               Y2010 = growth2 [years2 == 2010], Y2009 = growth2 [years2 == 2009],
               Y2008 = growth2 [years2 == 2008], Y2007 = growth2 [years2 == 2007],
               Y2006 = growth2 [years2 == 2006], Y2005 = growth2 [years2 == 2005],
               Y2004 = growth2 [years2 == 2004], Y2003 = growth2 [years2 == 2003],
               Y2002 = growth2 [years2 == 2002], Y2001 = growth2 [years2 == 2001],
               Y2000 = growth2 [years2 == 2000], Y1999 = growth2 [years2 == 1999],
               Y1998 = growth2 [years2 == 1998], Y1997 = growth2 [years2 == 1997],
               Y1996 = growth2 [years2 == 1996], Y1995 = growth2 [years2 == 1995],
               Y1994 = growth2 [years2 == 1994], Y1993 = growth2 [years2 == 1993],
               Y1992 = growth2 [years2 == 1992], Y1991 = growth2 [years2 == 1991],
               Y1990 = growth2 [years2 == 1990], Y1989 = growth2 [years2 == 1989],
               Y1988 = growth2 [years2 == 1988], Y1987 = growth2 [years2 == 1987],
               Y1986 = growth2 [years2 == 1986], Y1985 = growth2 [years2 == 1985],
               Y1984 = growth2 [years2 == 1984], Y1983 = growth2 [years2 == 1983],
               Y1982 = growth2 [years2 == 1982], Y1981 = growth2 [years2 == 1981],
               Y1980 = growth2 [years2 == 1980], Y1979 = growth2 [years2 == 1979],
               Y1978 = growth2 [years2 == 1978], Y1977 = growth2 [years2 == 1977],
               Y1976 = growth2 [years2 == 1976], Y1975 = growth2 [years2 == 1975],
               Y1974 = growth2 [years2 == 1974], Y1973 = growth2 [years2 == 1973],
               Y1972 = growth2 [years2 == 1972], Y1971 = growth2 [years2 == 1971],
               Y1970 = growth2 [years2 == 1970], Y1969 = growth2 [years2 == 1969],
               Y1968 = growth2 [years2 == 1968], Y1967 = growth2 [years2 == 1967],
               Y1966 = growth2 [years2 == 1966], Y1965 = growth2 [years2 == 1965],
               Y1964 = growth2 [years2 == 1964], Y1963 = growth2 [years2 == 1963],
               Y1962 = growth2 [years2 == 1962], Y1961 = growth2 [years2 == 1961],
               Y1960 = growth2 [years2 == 1960], Y1959 = growth2 [years2 == 1959],
               Y1958 = growth2 [years2 == 1958], Y1957 = growth2 [years2 == 1957],
               Y1956 = growth2 [years2 == 1956], Y1955 = growth2 [years2 == 1955],
               Y1954 = growth2 [years2 == 1954], Y1953 = growth2 [years2 == 1953],
               Y1952 = growth2 [years2 == 1952], Y1951 = growth2 [years2 == 1951],
               Y1950 = growth2 [years2 == 1950], Y1949 = growth2 [years2 == 1949],
               Y1948 = growth2 [years2 == 1948], Y1947 = growth2 [years2 == 1947],
               Y1946 = growth2 [years2 == 1946], Y1945 = growth2 [years2 == 1945],
               Y1944 = growth2 [years2 == 1944], Y1943 = growth2 [years2 == 1943],
               Y1942 = growth2 [years2 == 1942], Y1941 = growth2 [years2 == 1941],
               Y1940 = growth2 [years2 == 1940], Y1939 = growth2 [years2 == 1939],
               Y1938 = growth2 [years2 == 1938], Y1937 = growth2 [years2 == 1937],
               Y1936 = growth2 [years2 == 1936], Y1935 = growth2 [years2 == 1935],
               Y1934 = growth2 [years2 == 1934], Y1933 = growth2 [years2 == 1933],
               Y1932 = growth2 [years2 == 1932], Y1931 = growth2 [years2 == 1931],
               Y1930 = growth2 [years2 == 1930], Y1929 = growth2 [years2 == 1929],
               Y1928 = growth2 [years2 == 1928], Y1927 = growth2 [years2 == 1927],
               Y1926 = growth2 [years2 == 1926], Y1925 = growth2 [years2 == 1925],
               Y1924 = growth2 [years2 == 1924], Y1923 = growth2 [years2 == 1923],
               Y1922 = growth2 [years2 == 1922], Y1921 = growth2 [years2 == 1921],
               Y1920 = growth2 [years2 == 1920], Y1919 = growth2 [years2 == 1919],
               Y1918 = growth2 [years2 == 1918], Y1917 = growth2 [years2 == 1917],
               Y1916 = growth2 [years2 == 1916], Y1915 = growth2 [years2 == 1915],
               Y1914 = growth2 [years2 == 1914], Y1913 = growth2 [years2 == 1913],
               Y1912 = growth2 [years2 == 1912], Y1911 = growth2 [years2 == 1911],
               Y1910 = growth2 [years2 == 1910], Y1909 = growth2 [years2 == 1909],
               Y1908 = growth2 [years2 == 1908], Y1907 = growth2 [years2 == 1907],
               Y1906 = growth2 [years2 == 1906], Y1905 = growth2 [years2 == 1905],
               Y1904 = growth2 [years2 == 1904], Y1903 = growth2 [years2 == 1903],
               Y1902 = growth2 [years2 == 1902], Y1901 = growth2 [years2 == 1901],
               Y1900 = growth2 [years2 == 1900])
    
  } 
}  # end json file loop

# Arrange in advancing order by date, tree, sampling height
#----------------------------------------------------------------------------------------
incrementRingWidths <- incrementRingWidths %>% arrange (sampleDate, treeId, sampleHeight)

# Reset working directory
#----------------------------------------------------------------------------------------
setwd ('~/projects/PlantGrowth/Exp2018Analysis/')

# Standardise ring width using the 2015 ring
#----------------------------------------------------------------------------------------
incrementRingWidths <- incrementRingWidths %>% mutate (RWI2019 = Y2019 / Y2017,
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
summaryData <- incrementRingWidths %>% group_by (treatment, sampleDate, sampleHeight) %>% 
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

# Plot all ring width series 
#----------------------------------------------------------------------------------------
par (mfrow = c (1, 1))
par (mar = c (5, 5, 1, 1))
plot (x = 1900:2020,
      y = incrementRingWidths [1, 125:5] / 1000,
      typ = 'l', xlab = 'year', ylab = expression (paste ('ring width (',mm,')', sep = '')),
      ylim = c (0, 12), las = 1, col = 'white')
# Add average ring width
lines (x = 1900:2020,
       y = colMeans (incrementRingWidths [, 125:5], na.rm = TRUE) / 1000, 
       lwd = 3, col = 'darkred')
for (i in 1:36) {
  lines (x = 1900:2020, y = incrementRingWidths [i, 125:5] / 1000, lwd = 0.5)
}

# Clean unnecessary variables from loop
#----------------------------------------------------------------------------------------
rm (temp, treeID, t, i, j, k, jsonFiles, sampleDate, sampleHeight, growth, types, years, 
    con, len, summaryData, growingSeason, sampleH2, yPositions, h)

