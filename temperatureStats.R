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
hourlyData <- tempData %>% 
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
             t.end.of.line.01 = mean (t.end.of.line.01, na.rm = TRUE),
             t.end.of.line.03 = mean (t.end.of.line.03,  na.rm = TRUE),
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
             t.10.1p5m     = mean (t.10.1p5m,     na.rm = TRUE),
             t.air.1p5m    = mean (t.air.1p5m,    na.rm = TRUE))

# convert datetime back from factor to datetime
#----------------------------------------------------------------------------------------
hourlyData [['datetime']] <- as_datetime (hourlyData [['datetime']])

# check how often the datalogger battery voltage dropped below 11.0 watts at some point
#----------------------------------------------------------------------------------------
res <- sum (hourlyData [['u.battery']] < 11.0, na.rm = TRUE)
rm (res)

# select only relevant temperature variables
#----------------------------------------------------------------------------------------
hourlyData <- hourlyData %>% 
  select (-u.battery, -t.panel, -t.end.of.line.01, -t.end.of.line.03, -t.line.backflow, 
          -t.line.outflow)

# select date span with continuous measurements
#----------------------------------------------------------------------------------------
hourlyData <- filter (hourlyData, datetime > as_datetime ('2018-06-01'),
                                  datetime < as_datetime ('2018-10-01'))

# average daily values 
#----------------------------------------------------------------------------------------
dailyAverage <- hourlyData %>% group_by (datetime = cut (datetime, breaks = '1 day')) %>% 
  summarise (t.oak.1p5m = mean (t.oak.1p5m, na.rm = TRUE),
             t.air.2p0m = mean (t.air.2p0m, na.rm = TRUE), 
             t.air.1p5m = mean (t.air.1p5m, na.rm = TRUE), 
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

# estimate temperature effect of chilling (e.g., before, during and after chilling)
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = temp ~ (tree|height) + datetime + period:height:treatment,
            data = dailyAverage,
            REML = TRUE)

# average night-time values 
#----------------------------------------------------------------------------------------
nightTimeAverage <- hourlyData %>% 
  filter (lubridate::hour (datetime) >= 22 | lubridate::hour (datetime) <= 6) %>% 
  mutate (midday = if_else (as.numeric (format  (datetime, "%H")) >= 12, as_date (datetime), as_date (datetime)-1)) %>%
  group_by (midday) %>% 
  summarise (t.oak.1p5m = mean (t.oak.1p5m, na.rm = TRUE),
             t.air.2p0m = mean (t.air.2p0m, na.rm = TRUE), 
             t.air.1p5m = mean (t.air.1p5m, na.rm = TRUE), 
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
  
# wrangle data into long format
#----------------------------------------------------------------------------------------
nightTimeAverage <- nightTimeAverage %>% 
    pivot_longer (cols = !midday, names_to =  c ('tree','height'), 
                  names_prefix = 't.', 
                  names_pattern = '(.*)\\.(.*)', 
                  values_to = 'temp') 
  
# add column based on period (e.g., before, during, and after treatment)
#----------------------------------------------------------------------------------------
period <- ifelse (nightTimeAverage [['midday']] < startDate, 'before', 
                  ifelse (nightTimeAverage [['midday']] > endDate, 'after','during'))
nightTimeAverage <- nightTimeAverage %>% mutate (period)
  
# add a treatment group
#----------------------------------------------------------------------------------------
treatment <-  ifelse (nightTimeAverage [['tree']] %in% c ('01','02','03','04','05'), 'chilled', 
                      ifelse (nightTimeAverage [['tree']] %in% c ('06','07','08','10'), 'phloem',
                              'air'))
nightTimeAverage <- mutate (nightTimeAverage, treatment)
  
# drop rows with no temperature reading
#----------------------------------------------------------------------------------------
nightTimeAverage <- nightTimeAverage %>% filter (!is.nan (temp), !is.na (temp))
  
# wrangle fixed effects into reasonably-leveled factors
#----------------------------------------------------------------------------------------
nightTimeAverage <- nightTimeAverage %>% 
  mutate (period    = factor (period,    levels = c ('during','after','before')),
          height    = factor (height,    levels = c ('2p0m','1p0m','1p5m')),
          treatment = factor (treatment, levels = c ('chilled','phloem','air')),
          datetime  = factor (midday),
          tree      = factor (tree))
  
# estimate night time temperature effect of chilling (e.g., before, during and after chilling)
#----------------------------------------------------------------------------------------
M2 <- lmer (formula = temp ~ (tree|height) + datetime + period:height:treatment,
            data = nightTimeAverage,
            REML = TRUE)

# average day-time values 
#----------------------------------------------------------------------------------------
dayTimeAverage <- hourlyData %>% 
  filter (lubridate::hour (datetime) >= 10 | lubridate::hour (datetime) <= 18) %>% 
  group_by (datetime = cut (datetime, breaks = '1 day')) %>%
  summarise (t.oak.1p5m = mean (t.oak.1p5m, na.rm = TRUE),
             t.air.2p0m = mean (t.air.2p0m, na.rm = TRUE), 
             t.air.1p5m = mean (t.air.1p5m, na.rm = TRUE), 
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
dayTimeAverage <- dayTimeAverage %>% mutate (datetime = as_datetime (datetime))

# wrangle data into long format
#----------------------------------------------------------------------------------------
dayTimeAverage <- dayTimeAverage %>% 
  pivot_longer (cols = !datetime, names_to =  c ('tree','height'), 
                names_prefix = 't.', 
                names_pattern = '(.*)\\.(.*)', 
                values_to = 'temp') 

# add column based on period (e.g., before, during, and after treatment)
#----------------------------------------------------------------------------------------
period <- ifelse (dayTimeAverage [['datetime']] < startDate, 'before', 
                  ifelse (dayTimeAverage [['datetime']] > endDate, 'after','during'))
dayTimeAverage <- dayTimeAverage %>% mutate (period)

# add a treatment group
#----------------------------------------------------------------------------------------
treatment <-  ifelse (dayTimeAverage [['tree']] %in% c ('01','02','03','04','05'), 'chilled', 
                      ifelse (dayTimeAverage [['tree']] %in% c ('06','07','08','10'), 'phloem',
                              'air'))
dayTimeAverage <- mutate (dayTimeAverage, treatment)

# drop rows with no temperature reading
#----------------------------------------------------------------------------------------
dayTimeAverage <- dayTimeAverage %>% filter (!is.nan (temp), !is.na (temp))

# wrangle fixed effects into reasonably-leveled factors
#----------------------------------------------------------------------------------------
dayTimeAverage <- dayTimeAverage %>% 
  mutate (period    = factor (period,    levels = c ('during','after','before')),
          height    = factor (height,    levels = c ('2p0m','1p0m','1p5m')),
          treatment = factor (treatment, levels = c ('chilled','phloem','air')),
          datetime  = factor (datetime),
          tree      = factor (tree))

# estimate temperature effect of chilling (e.g., before, during and after chilling)
#----------------------------------------------------------------------------------------
M3 <- lmer (formula = temp ~ (tree|height) + datetime + period:height:treatment,
            data = dayTimeAverage,
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

# calculate mean temperature during chilling
#----------------------------------------------------------------------------------------
temp <- tempData %>% filter (datetime > startDate, datetime < endDate) %>% 
  select (datetime, t.01.2p0m, t.01.1p0m, t.02.2p0m, t.02.1p0m, t.03.2p0m, t.03.1p0m, 
          t.04.2p0m, t.04.1p0m, t.05.2p0m, t.05.1p0m) %>% 
  pivot_longer (cols = !datetime, 
                names_to =  c ('tree','height'), 
                names_prefix = 't.', 
                names_pattern = '(.*)\\.(.*)', 
                values_to = 'temp') %>%
  filter (!is.na (temp), !is.nan (temp)) 
mean (temp [['temp']] [temp [['height']] == '1p0m'], na.rm = TRUE)
sd   (temp [['temp']] [temp [['height']] == '1p0m'], na.rm = TRUE)
se   (temp [['temp']] [temp [['height']] == '1p0m'])
mean (temp [['temp']] [temp [['height']] == '2p0m'], na.rm = TRUE)
sd   (temp [['temp']] [temp [['height']] == '2p0m'], na.rm = TRUE)
se   (temp [['temp']] [temp [['height']] == '2p0m'])

# calculate average phloem temperature for each tree (i.e., which trees was chilled most 
# successfuly)
#----------------------------------------------------------------------------------------
temp <- tempData %>% filter (datetime > startDate, datetime < endDate) %>% 
  select (datetime, t.01.2p0m, t.01.1p0m, t.02.2p0m, t.02.1p0m, t.03.2p0m, t.03.1p0m, 
          t.04.2p0m, t.04.1p0m, t.05.2p0m, t.05.1p0m) %>% 
  pivot_longer (cols = !datetime, 
                names_to =  c ('tree','height'), 
                names_prefix = 't.', 
                names_pattern = '(.*)\\.(.*)', 
                values_to = 'temp') %>%
  filter (!is.na (temp), !is.nan (temp)) %>% group_by (tree) %>% 
  summarise (meanTemp = mean (temp, na.rm = TRUE), 
             seTemp = sd (temp, na.rm = TRUE),
             .groups = 'keep')

# Get midday temperatures on the 25th of August 2018
#----------------------------------------------------------------------------------------
temp <- tempData %>% filter (datetime > startDate, datetime < endDate) %>% 
  select (datetime, t.01.2p0m, t.01.1p0m, t.01.1p5m, t.02.2p0m, 
          t.02.1p5m, t.02.1p0m, t.03.2p0m, t.03.1p5m, t.03.1p0m, t.04.2p0m, t.04.1p5m, 
          t.04.1p0m, t.05.2p0m, t.05.1p5m, t.05.1p0m, t.air.1p5m) %>% 
  pivot_longer (cols = !datetime, 
                names_to =  c ('tree','height'), 
                names_prefix = 't.', 
                names_pattern = '(.*)\\.(.*)', 
                values_to = 'temp') %>%
  filter (!is.na (temp), !is.nan (temp)) %>%
  filter (datetime >= as_datetime ('2018-08-25 11:00:00'),
          datetime <= as_datetime ('2018-08-25 13:00:00')) %>% 
  mutate (dTemp = temp - 24.5) %>% # Because the air temperature was 24.5 C at midday
  filter (tree != 'air') %>% 
  mutate (treatment = ifelse (as.numeric (tree) <= 5, 5, 1)) %>%
  group_by (treatment, height) %>%
  summarise (meanDTemp = mean (dTemp), seDTemp = se (dTemp), .groups = 'drop')

# clean-up 
#----------------------------------------------------------------------------------------
rm (M1, M2, M3, dailyAverage, dayTimeAverage, nightTimeAverage, temp)
#========================================================================================