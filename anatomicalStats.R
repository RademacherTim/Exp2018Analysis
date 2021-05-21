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
if (!existsFunction ('as_date')) library ('lubridate')

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
summary (mod)

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
mod <- lmer (maxRW ~ (1 | treeId) + (1 | sampleDate) + year:treatment:sampleHeight, 
             data = tmpData, 
             REML = TRUE)
summary (mod); rm (tmpData)

# Did one treatment cause an increases or decrease in the number of cells formed taking 
# into account pre-existing differences in cell numbers from 2017?
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR %in% 2017:2018, PLOT %in% c (1, 5)) %>%
  group_by (YEAR, PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (maxNCells = max (cumNCells), .groups = 'drop') %>%
  mutate (year = factor (YEAR, levels = c (2018:2017)),
          treatment = factor (PLOT, levels = c (5, 1)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight, levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)),
          sampleDate = factor (sampleDate))
mod <- lmer (maxNCells ~ (1 | treeId) + (1 | sampleDate) + year:treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod); rm (tempData)
# There were clear differences in the number of cells formed that correspond to 
# temperature gradients with largest differences (roughly a 68% in cell number) at 1.0m 
# between chilled and control trees, 2.0m saw a similar reduction
# The effects were smaller but still substantial at 1.5, 2.5 and even 4.0 m in chilled trees.
# Control trees saw more cells in 2018 (between 2-7 cell more depending on sampling height)
# Chilled trees only had more cells at 0.5m, but about 7 cells more


# Was there already a reduction in the number of cell formed before the chilling?
# N.B.: We cannot take into account pre-existing differences from 2017, because we don't 
# have wood growth dynamics for 2017.
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR == 2018, PLOT %in% c (1, 5), period <= as_date ('2018-06-25')) %>%
  group_by (PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (maxNCells = max (cumNCells), .groups = 'drop') %>%
  mutate (treatment = factor (PLOT, levels = c (1, 5)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight, levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)),
          sampleDate = factor (sampleDate))
mod <- lmer (maxNCells ~ (1 | treeId) + (1 | sampleDate) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod)
# - Yes, there was a small difference of roughly 1 cells less before the chilling in 
#   chilled compared to controlt trees.

# Was the reduction in the number of cell formed between treatment pronounced during the 
# chilling?
# N.B.: We cannot take into account pre-existing differences from 2017, because we don't 
# have wood growth dynamics for 2017.
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR == 2018, PLOT %in% c (1, 5), 
          period > as_date ('2018-06-25') & period <= as_date ('2018-09-03')) %>%
  group_by (PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (maxNCells = max (cumNCells), .groups = 'drop') %>%
  mutate (treatment = factor (PLOT, levels = c (1, 5)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight, levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)),
          sampleDate = factor (sampleDate))
mod <- lmer (maxNCells ~ (1 | treeId) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod)
# - Yes, the difference was pronounced but with differences at specific treatment heights of 
#   3.6 cells more at 0.5m, 8 cells less at 1.5m, 7 cells less at 2.5m and 4 cells less at 4.0m 
#   in chilled versus control trees.
# - This period was also the period with cumulative cell numbers reaching 23 cells 
#   formed on average.


# Did the reduction in the number of cell formed between treatment persist after the 
# chilling?
# N.B.: We cannot take into account pre-existing differences from 2017, because we don't 
# have wood growth dynamics for 2017.
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR == 2018, PLOT %in% c (1, 5), period > as_date ('2018-09-03')) %>%
  group_by (PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (maxNCells = max (cumNCells), .groups = 'drop') %>%
  mutate (treatment = factor (PLOT, levels = c (1, 5)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight, levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)),
          sampleDate = factor (sampleDate))
mod <- lmer (maxNCells ~ (1 | treeId) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod)
# - Differences persisted with 11 cells at 1.5m, 4 cells at 2.5m and 3 cells at 4.0m.
# - There was no difference at 0.5m.
# - On average 14 cells formed during this period.

# Did one treatment cause a change in mean radial cell size of the first twenty percent 
# of the formed ring (i.e., growth occuring before chilling) taking into account 
# pre-existing differences from 2017?
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR %in% 2017:2018, RRADDISTR <= 20, PLOT %in% c (1, 5)) %>%
  group_by (YEAR, PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (meanCellSize = mean (cellRadWidth), .groups = 'drop') %>%
  mutate (year = factor (YEAR, levels = c (2018:2017)),
          treatment = factor (PLOT, levels = c (1, 5)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight, levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)),
          sampleDate = factor (sampleDate))
mod <- lmer (meanCellSize ~ (1 | treeId) + (1 | sampleDate) + year:treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod)
# The largest difference of about 8 micrometer or 18% occured in control trees at 2.0m 
# between 2017 and 2018, suggesting that natural variability was larger than any 
# treatment effect.

# Did one treatment cause a change in mean radial cell size taking into account 
# pre-existing differences from 2017?
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR %in% 2017:2018, PLOT %in% c (1, 5)) %>%
  group_by (YEAR, PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (meanCellSize = mean (cellRadWidth), .groups = 'drop') %>%
  mutate (year = factor (YEAR, levels = c (2018:2017)),
          treatment = factor (PLOT, levels = c (1, 5)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight, levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)),
          sampleDate = factor (sampleDate))
mod <- lmer (meanCellSize ~ (1 | treeId) + (1 | sampleDate) + year:treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod)
# Once more one of the largest difference of less than 4 micrometer or ~10% occured in 
# control trees at 2.0m between 2017 and 2018, suggesting that non-treatment variability 
# was larger than any treatment effect.
# The largest effect actually occured in chilled trees at 1.0m between the 2017 and 2018 
# estimates with an estimated inter-annual effect of 4.7 micrometer larger cells in 2018.
# This appears to be an outlier given remarkably small differences at other sampling heights.


# Did one treatment cause a change in mean tangential cell size taking into account 
# pre-existing differences from 2017?
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR %in% 2017:2018, PLOT %in% c (1, 5)) %>%
  group_by (YEAR, PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (meanCellSize = mean (cellTanWidth), .groups = 'drop') %>%
  mutate (year = factor (YEAR, levels = c (2018:2017)),
          treatment = factor (PLOT, levels = c (1, 5)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight, levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)),
          sampleDate = factor (sampleDate))
mod <- lmer (meanCellSize ~ (1 | treeId) + year:treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod)
# Model fails to converge when sampleDate is included as random effect, but that effect is
# not significant anyway. The largest estimated difference is about 1.4 micrometers or 4% 
# for chilled trees at 1.0m when comparing 2017 and 2018. Same outlier as for radial cell 
# width.

# Did one treatment cause a change in cumulative cell-wall area taking into account 
# pre-existing differences from 2017?
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR %in% 2017:2018, PLOT %in% c (1, 5)) %>%
  group_by (YEAR, PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (maxCumCWA = max (cumCWA), .groups = 'drop') %>%
  mutate (year = factor (YEAR, levels = c (2018:2017)),
          treatment = factor (PLOT, levels = c (1, 5)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight, levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)),
          sampleDate = factor (sampleDate))
mod <- lmer (maxCumCWA ~ (1 | treeId) + (1 | sampleDate) + year:treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod)
# - The control group had significantly more cumulative cell-wall area in 2018 compared 
#   to 2017 across all heights.
# - Chilled trees had already slightly lower estimated cumulative cell-wall area in 
#   2017, but these differences on the order of 600-800 micrometer2 or 5-6% are dwarfed 
#   by the differences between treatment groups in 2018 of 11 000 micrometers2 or 83%
# - The effect was largest at 2.0 > 1.0 > 1.5 > 0.5 > 2.5 > 4.0m 


# Did one treatment cause a change in mean cell-wall area per cell taking into account 
# pre-existing differences from 2017?
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR %in% 2017:2018, PLOT %in% c (1, 5)) %>%
  group_by (YEAR, PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (meanCWA = mean (CWA), .groups= 'drop') %>%
  mutate (year = factor (YEAR, levels = c (2018:2017)),
          treatment = factor (PLOT, levels = c (1, 5)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight, levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)),
          sampleDate = factor (sampleDate))
mod <- lmer (meanCWA ~ (1 | treeId) + year:treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod)
# - Control had higher cell-wall area per cell in 2018 relative to 2017 (8-75 
#   micrometer2 or 2-16%).
# - On the contrary the chilled trees had less cell-wall area per cell with 16-55 
#   micrometers2 or 3-11% in 2018 relative to 2017, except for at 2.0, where there was 
#   virtually no difference.
# - Between treatment differences in 2018 were largest at 2.0 with 127 micrometers2 or 
#   26% less cell-wall area per cell in chilled trees, but were fairly large across all 
#   height.


# Was this reduction in cell-wall area per cell in chilled trees exitant in cells that 
# formed before chilling? 
# N.B.: We cannot take into account pre-existing differences from 2017, because we don't 
# have wood growth dynamics for 2017.
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR == 2018, PLOT %in% c (1, 5), period <= as_date ('2018-06-25')) %>%
  group_by (PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (meanCWA = mean (CWA, na.rm = TRUE), .groups = 'drop') %>%
  mutate (treatment = factor (PLOT, levels = c (1, 5)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight, levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)),
          sampleDate = factor (sampleDate))
mod <- lmer (meanCWA ~ (1 | treeId) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod)
# - There were already differences before the chilling of 21, 53, 14, 36 micrometer2 at 
#   0.5, 1.5, 2.5, 4.0m, respectively.

# Was this reduction in cell-wall area per cell in chilled trees pronounced in cells that 
# formed during the chilling? 
# N.B.: We cannot take into account pre-existing differences from 2017, because we don't 
# have wood growth dynamics for 2017.
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR == 2018, PLOT %in% c (1, 5), 
          period > as_date ('2018-06-25') & period <= as_date ('2018-09-03')) %>%
  group_by (PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (meanCWA = mean (CWA, na.rm = TRUE), .groups = 'drop') %>%
  mutate (treatment = factor (PLOT, levels = c (1, 5)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight, levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)),
          sampleDate = factor (sampleDate))
mod <- lmer (meanCWA ~ (1 | treeId) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod)
# - Yes, the differences were pronounced in cells formed during the chilling with 87, 
#   138, 75, and 71 micrometer2 at 0.5, 1.5, 2.5, 4.0m.

# Did this reduction in cell-wall area per cell in chilled trees persist in cells that 
# formed after the chilling? 
# N.B.: We cannot take into account pre-existing differences from 2017, because we don't 
# have wood growth dynamics for 2017.
#----------------------------------------------------------------------------------------
tempData <- anatomicalData %>% 
  filter (YEAR == 2018, PLOT %in% c (1, 5), period > as_date ('2018-09-03')) %>%
  group_by (PLOT, TREE, sampleHeight, sampleDate) %>% 
  summarise (meanCWA = mean (CWA, na.rm = TRUE), .groups = 'drop') %>%
  mutate (treatment = factor (PLOT, levels = c (1, 5)),
          treeId = factor (TREE),
          sampleHeight = factor (sampleHeight, levels = c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)),
          sampleDate = factor (sampleDate))
mod <- lmer (meanCWA ~ (1 | treeId) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod)
# - Yes, if anything it increased even more to 70, 200, 101, 144 micrometer2 at 0.5, 1.5, 
#   2.5, 4.0m


#========================================================================================