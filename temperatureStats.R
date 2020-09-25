#========================================================================================
# This script tests for differences between treatments and sampling heights in 
# phloem temperature for the 2018 phloem chilling and compression experiment on white 
# pine at Harvard Forest. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lme4')
library ('tidyverse')
library ('lubridate')
source ('plotingFunctions.R')

# read processed respiration data
#----------------------------------------------------------------------------------------
source ('./readTemperatureData.R')

# average temperature data over one hour intervals
#----------------------------------------------------------------------------------------
tempData <- tempData %>% 
  group_by (datetime = cut (datetime, breaks = '1 hour')) %>% 
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
tempData [['datetime']] <- as_datetime (tempData [['datetime']])

# check how often the datalogger battery voltage dropped below 11.0 watts at some point
#----------------------------------------------------------------------------------------
res <- sum (tempData [['u.battery']] < 11.0, na.rm = TRUE)
rm (res)

# select only relevant temperature variables
#----------------------------------------------------------------------------------------
tempData <- tempData %>% select (datetime, t.oak.1p5m, t.air.2p0m, t.01.2p0m, t.01.1p5m, 
                                 t.01.1p0m, t.02.2p0m, t.02.1p5m, t.02.1p0m, t.03.2p0m, 
                                 t.03.1p5m, t.03.1p0m, t.04.2p0m, t.04.1p5m, t.04.1p0m, 
                                 t.05.2p0m, t.05.1p5m, t.05.1p0m, t.06.1p5m, t.07.1p5m, 
                                 t.08.1p5m, t.10.1p5m)

# select date span with continuous measurements
#----------------------------------------------------------------------------------------
tempData <- filter (tempData, datetime > as_datetime ('2018-06-01'),
                              datetime < as_datetime ('2018-10-01'))

# average daily values 
#----------------------------------------------------------------------------------------
dailyAverage <- tempData %>% group_by (datetime = cut (datetime, breaks = '1 day')) %>% 
  summarise (t.oak.1p5m = mean (t.oak.1p5m, na.rm = TRUE),
             t.air.1p5m = mean (t.air.2p0m, na.rm = TRUE), # renamed temperature to average the two air temperatures
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
             t.08.1p5m  = mean (t.08.1p5m, na.rm = TRUE),
             t.10.1p5m  = mean (t.10.1p5m, na.rm = TRUE))

# convert datetime back to datetime
#----------------------------------------------------------------------------------------
dailyAverage <- dailyAverage %>% mutate (datetime = as_datetime (datetime))

# get start and end dates for each treatment
#----------------------------------------------------------------------------------------
startDate <- criticalDates (group = 5, asDate = FALSE, startOnly = TRUE) 
endDate   <- criticalDates (group = 5, asDate = FALSE, endOnly   = TRUE)

# wrangle data into long format
#----------------------------------------------------------------------------------------
dailyAverage <- dailyAverage %>% 
  pivot_longer (cols = !datetime, names_to =  c ('tree','height'), 
                names_prefix = 't.', 
                names_pattern = '(.*)\\.(.*)', 
                values_to = 'temp') 

# add column based on period (e.g., before, during, and after treatment)
#----------------------------------------------------------------------------------------
period <- ifelse (dailyAverage [['datetime']] < startDate, 'before', 
                  ifelse (dailyAverage [['datetime']] > endDate, 'after','during'))
periodAlt <- ifelse (dailyAverage [['datetime']] < startDate | 
                     dailyAverage [['datetime']] > endDate,'non-chilling','chilling')
dailyAverage <- dailyAverage %>% mutate (period, periodAlt)

# add a treatment group
#----------------------------------------------------------------------------------------
treatment <-  ifelse (dailyAverage [['tree']] %in% c ('01','02','03','04','05'), 'chilled', 
                      ifelse (dailyAverage [['tree']] %in% c ('06','07','08','10'), 'phloem',
                              'air'))
dailyAverage <- mutate (dailyAverage, treatment)

# drop rows with no temperature reading
#----------------------------------------------------------------------------------------
dailyAverage <- dailyAverage %>% filter (!is.nan (temp), !is.na (temp))

# wrangle fixed effects into reasonably-leveled factors
#----------------------------------------------------------------------------------------
dailyAverage <- dailyAverage %>% 
  mutate (period    = factor (period,    levels = c ('during','after','before')),
          periodAlt = factor (periodAlt, levels = c ('chilling','non-chilling')),
          height    = factor (height,    levels = c ('2p0m','1p0m','1p5m')),
          treatment = factor (treatment, levels = c ('chilled','phloem','air')),
          datetime  = factor (datetime),
          tree      = factor (tree))

# compare temperature during the non-chilling period
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = temp ~ (tree|height) + datetime + period:height:treatment,
            data = dailyAverage,
            REML = TRUE)

# calculate time at desired temperatures (below 5.0 degree Celsius) for the phloem at 1.0 
# and 2.0 m in chilled trees throughout chilling
#----------------------------------------------------------------------------------------
temp <- tempData %>% filter (datetime > startDate, datetime < endDate) %>% 
  select (datetime, t.01.2p0m, t.01.1p0m, t.02.2p0m, t.02.1p0m, t.03.2p0m, t.03.1p0m, 
          t.04.2p0m, t.04.1p0m, t.05.2p0m, t.05.1p0m) %>% 
  pivot_longer (cols = !datetime, 
                names_to =  c ('tree','height'), 
                names_prefix = 't.', 
                names_pattern = '(.*)\\.(.*)', 
                values_to = 'temp') %>%
  filter (!is.na (temp), !is.nan (temp)) %>%
  group_by (temp < 5.0) 
sum (temp [['temp < 5']] [temp [['height']] == '1p0m'], na.rm = TRUE) / 
  length (temp [['temp < 5']] [temp [['height']] == '1p0m']) * 100.0
sum (temp [['temp < 5']] [temp [['height']] == '2p0m'], na.rm = TRUE) / 
  length (temp [['temp < 5']] [temp [['height']] == '2p0m']) * 100.0
#========================================================================================