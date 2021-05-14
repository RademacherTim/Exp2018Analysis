#========================================================================================
# Script to determine treatment effect on wood formation and anatomy for the 2018 
# experiment at Harvard Forest, which was generated from thin-sections of samples 
# collected on the 2018-11-15 and 2019-10-24 using ROXAS. ROXAS outputs were converted 
# to median values of 20 micrometer-wide tangential bands, which are publicly available 
# on the Harvard Forest data archive as part of the experiments data set.
#
# Data repository url:
# Code repository url:
#
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('lmer')) library ('lme4')
if (!existsFunction ('%>%')) library ('tidyverse')

# source anatomical data
#----------------------------------------------------------------------------------------
if (!exists ('anatomicalData')) source ('processAnatomicalData.R')

# Did one treatment cause an increases or decrease in ring width taking into account 
# inter-annual variation between the treatment groups?
#----------------------------------------------------------------------------------------
tmpData <- anatomicalData %>% 
  filter (PLOT %in% c (1, 5)) %>%
  group_by (YEAR, PLOT, TREE, sampleHeight, sampleDate) %>%  
  summarise (maxRW = max (MRW), .groups = 'keep') %>%
  mutate (year = factor (YEAR, levels = c (2018:1989)),
          treatment = factor (PLOT, levels = c (1, 5)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight, levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)),
          sampleDate = factor (sampleDate))
mod <- lmer (maxRW ~ (1 | treeId) + (1 | sampleDate) + year + treatment + treatment:sampleHeight, 
             data = tmpData, 
             REML = TRUE)
summary (mod); rm (tmpData)

# Did one treatment cause a change in ring width if we assume that chilling had no overall 
# effect on growth?
#----------------------------------------------------------------------------------------
mod <- lmer (maxRW ~ (1 | treeId) + (1 | sampleDate) + year  + treatment:sampleHeight, 
             data = tmpData, 
             REML = TRUE)
summary (mod)

# Did one treatment cause a change in ring width at various sampling height if we only 
# compare growth to the previous year?
#----------------------------------------------------------------------------------------
tmpData <- tmpData %>% filter (YEAR %in% 2017:2018)
mod <- lmer (maxRW ~ (1 | treeId) + (1 | sampleDate) + year + treatment:sampleHeight, 
             data = tmpData, 
             REML = TRUE)
summary (mod); rm (tmpData)

# TR- N.B.: Something does not make sense with 2017 having varying cell number data 
# Did one treatment cause an increases or decrease in the number of cells formed taking 
# into account pre-existing differences in cell numbers from 2017?
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR %in% 2017:2018, PLOT %in% c (1, 5)) %>%
  group_by (YEAR, PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (maxNCells = max (cumNCells), .groups = 'drop') %>%
  mutate (year = factor (YEAR, levels = c (2018:2017)),
          treatment = factor (PLOT, levels = c (1, 5)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight, levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)),
          sampleDate = factor (sampleDate))
mod <- lmer (maxNCells ~ (1 | treeId) + (1 | sampleDate) + year + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod); rm (tempData)

# Did one treatment cause an increases or reduction in mean radial cell size?
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR == 2018, RRADDISTR <= 10, PLOT %in% c (1, 5)) %>%
  group_by (PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (meanCellSize = mean (cellRadWidth)) %>%
  mutate (treatment = factor (PLOT, levels = c (5, 1)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight),
          sampleDate = factor (sampleDate))
mod <- lmer (meanCellSize ~ (1|treeId) + (1|sampleDate) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod)

# Did one treatment cause an increases or reduction in mean tangential cell size?
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR == 2018, PLOT %in% c (1, 5)) %>%
  group_by (PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (meanCellSize = mean (cellTanWidth)) %>%
  mutate (treatment = factor (PLOT, levels = c (5, 1)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight),
          sampleDate = factor (sampleDate))
mod <- lmer (meanCellSize ~ (1|treeId) + (1|sampleDate) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod)

# Did one treatment cause an increases or reduction in cumulative cell-wall area?
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR == 2018, PLOT %in% c (1, 5)) %>%
  group_by (PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (maxCumCWA = max (cumCWA)) %>%
  mutate (treatment = factor (PLOT, levels = c (5, 1)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight),
          sampleDate = factor (sampleDate))
mod <- lmer (maxCumCWA ~ (1|treeId) + (1|sampleDate) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod)


# Did one treatment cause an increases or reduction in mean cell-wall area?
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR == 2018, PLOT %in% c (1, 5)) %>%
  group_by (PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (meanCWA = mean (CWA)) %>%
  mutate (treatment = factor (PLOT, levels = c (5, 1)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight),
          sampleDate = factor (sampleDate))
mod <- lmer (meanCWA ~ (1|treeId) + (1|sampleDate) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod)


# was this reduction in cell-wall area pronounced in cells that formed after treatment onset?
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR == 2018, PLOT %in% c (1, 5), period > as_date ('2018-06-25')) %>%
  group_by (PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (meanCWA = mean (CWA, na.rm = TRUE)) %>%
  mutate (treatment = factor (PLOT, levels = c (5, 1)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight),
          sampleDate = factor (sampleDate))
mod <- lmer (meanCWA ~ (1|treeId) + (1|sampleDate) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod)
#========================================================================================