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
source ('processAnatomicalData.R')

# Did one treatment cause an increases or decrease in ring width?
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR == 2018, PLOT %in% c (1, 5), sampleDate == as_date ('2018-11-15')) %>%
  group_by (PLOT, TREE, sampleHeight) %>% 
  summarise (maxRW = max (MRW)) %>%
  mutate (treatment = factor (PLOT, levels = c (5, 1)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight))
mod <- lmer (maxRW ~ (1|treeId) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod); rm (tempData)

# Did one treatment cause an increases or decrease in the number of cells formed?
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR == 2018, PLOT %in% c (1, 5)) %>%
  group_by (PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (maxNCells = max (cumNCells)) %>%
  mutate (treatment = factor (PLOT, levels = c (5, 1)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight),
          sampleDate = factor (sampleDate))
mod <- lmer (maxNCells ~ (1|treeId) + (1|sampleDate) + treatment:sampleHeight, 
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