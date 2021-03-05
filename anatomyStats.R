#========================================================================================
# This script tests for differences between treatments and sampling heights in 
# anatomical traits of the resulting ring.
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lme4')
library ('caret')
library ('tidyverse')
library ('lubridate')
source ('plotingFunctions.R')

# load ring width data from thin-sections measured with WIAD
#----------------------------------------------------------------------------------------
source ('readRingWidths.R')
 
# load ring width from increment cores
#----------------------------------------------------------------------------------------
source ('readIncrementRingWidths.R')

# load anatomical data from thin-sections
#----------------------------------------------------------------------------------------
source ('processAnatomicalData.R')

# Compare differences in ring widths from all samples attributing its variability to tree, 
# sampling height and year of formation
#----------------------------------------------------------------------------------------
# TR - NB: This currently includes the compressed trees
tempData <- ringWidths %>% filter (sampleDate != as_date ('2018-05-01')) %>% 
  filter (treatment %in% c (1, 4, 5)) %>%
  select (c (1:4, 7:14)) %>% 
  pivot_longer (cols = 5:12, names_prefix = 'Y', names_to = 'year', values_to = 'RW') %>%
  mutate (treeId       = factor (treeId),
          treatment    = factor (treatment),
          sampleDate   = factor (sampleDate),
          sampleHeight = factor (sampleHeight),
          year         = factor (year))

# Backward stepwise regression to compare multiple linear regression models 
full.model <- lm (RW ~ year + treeId + sampleHeight + sampleDate, data = tempData)
BIC (full.model)
best.model <- lm (RW ~ year + treeId, data = tempData)
BIC (best.model)
mini.model <- lm (RW ~ year, data = tempData)
BIC (mini.model)
mini.model <- lm (RW ~ treeId, data = tempData)
BIC (mini.model)

# Are there clear differences in growth before treatment onset based on ring widths of the 
# previous eight years (i.e., 2010 to 2017)
#----------------------------------------------------------------------------------------
M2 <- lmer (formula = RW ~ (treeId | sampleHeight) + year + treatment, 
            data = tempData,
            REML = TRUE)
# The chilled trees grew a little bit (0.3 mm) less than the control while the compressed 
# trees grew 0.1mm more than the control trees on average over the previous eight years.

tempData

# TR Still need to adapt all of the below

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

#========================================================================================