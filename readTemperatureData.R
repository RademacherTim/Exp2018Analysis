#========================================================================================
# Script to read the temperature data from the project chill shed and the phloem of all 
# trees at 1.0 and 2.0 m for the 2018 chilling experiment on the white pines.
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')

# read temperature data file
#----------------------------------------------------------------------------------------
tempData <- read_csv (file = './data/temperatureData_HF_Exp2018.csv', na = "NA",
                      col_types = cols (datetime   = col_datetime (),
                                        record     = col_integer (),
                                        u.battery  = col_double (),
                                        t.panel    = col_double (),
                                        t.oak.1p5m = col_double (),
                                        t.01.l4    = col_double (),
                                        t.01.l3    = col_double (),
                                        t.01.l2    = col_double (),
                                        t.01.l1    = col_double (),
                                        t.02.l4    = col_double (),
                                        t.02.l3    = col_double (),
                                        t.02.l2    = col_double (),
                                        t.02.l1    = col_double (),
                                        t.03.l4    = col_double (),
                                        t.03.l3    = col_double (),
                                        t.03.l2    = col_double (),
                                        t.03.l1    = col_double (),
                                        t.04.l4    = col_double (),
                                        t.04.l3    = col_double (),
                                        t.04.l2    = col_double (),
                                        t.04.l1    = col_double (),
                                        t.05.l4    = col_double (),
                                        t.05.l3    = col_double (),
                                        t.05.l2    = col_double (),
                                        t.05.l1    = col_double (),
                                        t.end.of.line.01.blue.tape = col_double (),
                                        t.end.of.line.03.red.tape  = col_double (),
                                        t.line.backflow = col_double (),
                                        t.line.outflow  = col_double (),
                                        t.air.2p0m = col_double (),
                                        t.01.2p0m  = col_double (),
                                        t.01.1p5m  = col_double (),
                                        t.01.1p0m  = col_double (),
                                        t.02.2p0m  = col_double (),
                                        t.02.1p5m  = col_double (),
                                        t.02.1p0m  = col_double (),
                                        t.03.2p0m  = col_double (),
                                        t.03.1p5m  = col_double (),
                                        t.03.1p0m  = col_double (),
                                        t.04.2p0m  = col_double (),
                                        t.04.1p5m  = col_double (),
                                        t.04.1p0m  = col_double (),
                                        t.05.2p0m  = col_double (),
                                        t.05.1p5m  = col_double (),
                                        t.05.1p0m  = col_double (),
                                        t.06.1p5m  = col_double (),
                                        t.07.1p5m  = col_double (),
                                        t.07.1p5m.air = col_double (),
                                        t.08.1p5m     = col_double (),
                                        t.08.1p5m.air = col_double (),
                                        t.10.1p5m     = col_double ()))

# average temperature data over 15-minute intervals
#----------------------------------------------------------------------------------------
tempData <- tempData %>% 
  add_row (datetime = as_datetime ('2018-04-19 14:30:00'), .before = 1) %>%
  group_by (datetime = cut (datetime, breaks = '15 min', ordered_result = TRUE)) %>% 
  summarise (u.battery  = mean (u.battery,  na.rm = TRUE),
             t.panel    = mean (t.panel,    na.rm = TRUE),
             t.oak.1p5m = mean (t.oak.1p5m, na.rm = TRUE),
             t.01.l4    = mean (t.01.l4,    na.rm = TRUE),
             t.01.l3    = mean (t.01.l3,    na.rm = TRUE),
             t.01.l2    = mean (t.01.l2,    na.rm = TRUE),
             t.01.l1    = mean (t.01.l1,    na.rm = TRUE),
             t.02.l4    = mean (t.02.l4,    na.rm = TRUE),
             t.02.l3    = mean (t.02.l3,    na.rm = TRUE),
             t.02.l2    = mean (t.02.l2,    na.rm = TRUE),
             t.02.l1    = mean (t.02.l1,    na.rm = TRUE),
             t.03.l4    = mean (t.03.l4,    na.rm = TRUE),
             t.03.l3    = mean (t.03.l3,    na.rm = TRUE),
             t.03.l2    = mean (t.03.l2,    na.rm = TRUE),
             t.03.l1    = mean (t.03.l1,    na.rm = TRUE),
             t.04.l4    = mean (t.04.l4,    na.rm = TRUE),
             t.04.l3    = mean (t.04.l3,    na.rm = TRUE),
             t.04.l2    = mean (t.04.l2,    na.rm = TRUE),
             t.04.l1    = mean (t.04.l1,    na.rm = TRUE),
             t.05.l4    = mean (t.05.l4,    na.rm = TRUE),
             t.05.l3    = mean (t.05.l3,    na.rm = TRUE),
             t.05.l2    = mean (t.05.l2,    na.rm = TRUE),
             t.05.l1    = mean (t.05.l1,    na.rm = TRUE),
             t.end.of.line.01 = mean (t.end.of.line.01.blue.tape, na.rm = TRUE),
             t.end.of.line.03 = mean (t.end.of.line.03.red.tape,  na.rm = TRUE),
             t.line.backflow  = mean (t.line.backflow, na.rm = TRUE),
             t.line.outflow   = mean (t.line.outflow,  na.rm = TRUE),
             t.air.2p0m = mean (t.air.2p0m, na.rm = TRUE),
             t.01.2p0m  = mean (t.01.2p0m, na.rm = TRUE),
             t.01.1p5m  = mean (t.01.1p5m, na.rm = TRUE),
             t.01.1p0m  = mean (t.01.1p0m, na.rm = TRUE),
             t.02.2p0m  = mean (t.02.2p0m, na.rm = TRUE),
             t.02.1p5m  = mean (t.02.1p5m, na.rm = TRUE),
             t.02.1p0m  = mean (t.02.1p0m, na.rm = TRUE),
             t.03.2p0m  = mean (t.03.2p0m, na.rm = TRUE),
             t.03.1p5m  = mean (t.03.1p5m, na.rm = TRUE),
             t.03.1p0m  = mean (t.03.1p0m, na.rm = TRUE),
             t.04.2p0m  = mean (t.04.2p0m, na.rm = TRUE),
             t.04.1p5m  = mean (t.04.1p5m, na.rm = TRUE),
             t.04.1p0m  = mean (t.04.1p0m, na.rm = TRUE),
             t.05.2p0m  = mean (t.05.2p0m, na.rm = TRUE),
             t.05.1p5m  = mean (t.05.1p5m, na.rm = TRUE),
             t.05.1p0m  = mean (t.05.1p0m, na.rm = TRUE),
             t.06.1p5m  = mean (t.06.1p5m, na.rm = TRUE),
             t.07.1p5m  = mean (t.07.1p5m, na.rm = TRUE),
             t.07.1p5m.air = mean (t.07.1p5m.air, na.rm = TRUE),
             t.08.1p5m     = mean (t.08.1p5m,     na.rm = TRUE),
             t.08.1p5m.air = mean (t.08.1p5m.air, na.rm = TRUE),
             t.10.1p5m     = mean (t.10.1p5m,     na.rm = TRUE))

# convert datetime back from factor to datetime
#----------------------------------------------------------------------------------------
tempData <- tempData [-1, ]
tempData [['datetime']] <- as_datetime (tempData [['datetime']]) 
 
# read Fisher station temperature data from Harvard Forest Data Archive and add it to the 
# tibble
#----------------------------------------------------------------------------------------
LOCAL <- TRUE
if (LOCAL) {
 tmp <- read_csv (file = './data/hf001-10-15min-m-airt-only.csv',
                  col_types = cols ())
} else {
 fileURL <- url ('https://harvardforest.fas.harvard.edu/data/p00/hf001/hf001-10-15min-m.csv')
 tmp <- read_csv (file = fileURL, col_types = cols (
   f.airt = col_character (),
   f.prec = col_character (),
   f.rh   = col_character (),
   f.dewp = col_character (),
   f.slrr = col_character (),
   f.parr = col_character (),
   f.bar  = col_character (),
   f.wspd = col_character (),
   f.wres = col_character (),
   f.wdir = col_character (),
   f.wdev = col_character (),
   f.gspd = col_character (),
   f.s10t = col_character ())
 ) %>% select (datetime, airt) %>% filter (datetime > min (tempData [['datetime']], na.rm = TRUE), 
                                           datetime < max (tempData [['datetime']], na.rm = TRUE)) 
 # Save a local copy 
 #--------------------------------------------------------------------------------------
 write_csv (tmp, './data/hf001-10-15min-m-airt-only.csv')
 LOCAL <- TRUE
}
 
# Add the 15 minutes air temperature data to the 
#----------------------------------------------------------------------------------------
tempData <- right_join (tempData, tmp) %>% rename (t.air.1p5m = airt)
#========================================================================================