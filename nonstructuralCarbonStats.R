#========================================================================================
# This script tests for differences between treatments and sampling heights in 
# wood soluble sugar and starch concentrations for the 2018 phloem chilling and 
# compression experiment on white pine at Harvard Forest. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lme4')
library ('tidyverse')
library ('lubridate')
source ('./plotingFunctions.R')

# source processed data
#----------------------------------------------------------------------------------------
source ('./readNSCData.R') 

# select only necessary columns and wrangle data to contain factors
#----------------------------------------------------------------------------------------
stemData2018 <- stemData2018 %>% 
  select (DateOfSampleCollection, treeID, sampleHeight, treatment, 
          ConcentrationSugarPerDW, ConcentrationStarchPerDW) %>% 
  rename (date = DateOfSampleCollection, tree = treeID, height = sampleHeight,
          sugar = ConcentrationSugarPerDW, starch = ConcentrationStarchPerDW) %>%
  mutate (date = as_date (date))

# get start and end dates for each treatment
#----------------------------------------------------------------------------------------
startDate <- criticalDates (group = 5, asDate = FALSE, startOnly = TRUE) 
endDate   <- criticalDates (group = 5, asDate = FALSE, endOnly   = TRUE)

# add column based on period (e.g., before, during, and after treatment)
#----------------------------------------------------------------------------------------
period <- ifelse (stemData2018 [['date']] < startDate, 'before', 
                  ifelse (stemData2018 [['date']] > endDate, 'after','during'))
periodAlt <- ifelse (stemData2018 [['date']] < startDate | 
                     stemData2018 [['date']] > endDate,'non-chilling','chilling')
stemData2018 <- stemData2018 %>% mutate (period, periodAlt)

# convert variable to factors
#----------------------------------------------------------------------------------------
stemData2018 <- stemData2018 %>%
  mutate (date      = factor (date),
          tree      = factor (tree),
          height    = factor (height,    levels = 3:1),
          treatment = factor (treatment, levels = c (5, 4, 1)),
          period    = factor (period,    levels = c ('during','after','before')),
          periodAlt = factor (periodAlt, levels = c ('chilling','non-chilling')))


# fit mixed effects model to wood sugar concentrations with tree and height as random 
# effects to account for idiosyncratic differences due to factors such as variations 
# in exact azimuth or systematic difference between trees
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = sugar ~ (tree | height) + date + period:treatment:height, 
            data = stemData2018,
            REML = TRUE)
summary (M1)

# fit mixed effects model to wood starch concentrations with tree and height as random 
# effects to account for idiosyncratic differences due to factors such as variations 
# in exact azimuth or systematic difference between trees
#----------------------------------------------------------------------------------------
M2 <- lmer (formula = starch ~ (tree | height) + date + period:treatment:height, 
            data = stemData2018,
            REML = TRUE)
summary (M2)

#========================================================================================