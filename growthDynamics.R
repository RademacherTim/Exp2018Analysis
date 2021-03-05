#========================================================================================
# Script to plot differences in the growing season length for the 2018 experiment at 
# Harvard Forest, which was generated from thin-sections of samples collected during the 
# starting on 2018-05-01 and finishing 2018-11-15 with a follow-up sample from 2019-10-24.
# Samples were analysed using WIAD to determine ring widths. monotonic general additive 
# models were fit to the ring width and a threshold of 5% of full ring width was used to
# determine the onset and end of the growing season. All data and core are publicly 
# available on the Harvard Forest.
#
# Data repository url:
# Code repository url:
#
# Author: Tim Rademacher (trademacher@fas.harvard.edu)
#
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('%>%'))     library ('tidyverse')
if (!existsFunction ('as_date')) library ('lubridate')
if (!existsFunction ('vioplot')) library ('ggplot2')
if (!exists ('tColours')) source ('plotingFunctions.R')
if (!existsFunction ('lmer')) library ('lme4')

# load growing season data derived from general additive models fitted to ring widths 
# from thin-sections measured with WIAD
#----------------------------------------------------------------------------------------
if (!exists ('growingSeasonDates')) source ('extractGrowingSeasonDates.R')

# make box plot of onset and cessation of growth for each treatment
#----------------------------------------------------------------------------------------
ggplot (growingSeasonDates, 
        aes (startOfSeason)) + 
  geom_violin (colour = "black", fill = "red") + 
  scale_y_continuous (breaks = seq (0, 360, by = 60))

vioplot (startOfGrowth ~ treatment, data = growingSeasonDates,
         horizontal = TRUE, ylim = c (0, 365), xlab = 'day of year',
         col = addOpacity (tColours [['colour']] [c (1, 4, 5)], 0.6),
         axes = FALSE)
#axis (side = 1, at = seq (0, 360, by = 60))
vioplot (endOfGrowth ~ treatment, data = growingSeasonDates,
         horizontal = TRUE, add = TRUE, axes = FALSE,
         col = addOpacity (tColours [['colour']] [c (1, 4, 5)], 0.6))
#axis (side = 2, labels = c ('control','compressed','chilled'), at = 1:3)

# make box plot of onset and cessation of growth for each treatment
#----------------------------------------------------------------------------------------
boxplot (startOfGrowth ~ treatment, data = growingSeasonDates,
         horizontal = TRUE, ylim = c (0, 365), xlab = 'day of year', axes = FALSE,
         col = addOpacity (tColours [['colour']] [c (1, 4, 5)], 0.6))
axis (side = 1, at = seq (0, 360, by = 60))
boxplot (endOfGrowth ~ treatment, data = growingSeasonDates,
         horizontal = TRUE, add = TRUE, axes = FALSE,
         col = addOpacity (tColours [['colour']] [c (1, 4, 5)], 0.6))
axis (side = 2, labels = c ('control','compressed','chilled'), at = 1:3)

# make box plot of onset and cessation of growth for each sample height and treatment
#----------------------------------------------------------------------------------------
par (mar = c (5, 12, 1, 1))
boxplot (startOfGrowth ~ treatment + sampleHeight, data = growingSeasonDates,
         horizontal = TRUE, ylim = c (0, 365), xlab = 'day of year', 
         ylab = '', axes = FALSE,
         col = rep (addOpacity (tColours [['colour']] [c (1, 4, 5)], 0.6), 4 ))
axis (side = 1, at = seq (0, 360, by = 60))
boxplot (endOfGrowth ~ treatment + sampleHeight, data = growingSeasonDates,
         horizontal = TRUE, add = TRUE, axes = FALSE,
         col = addOpacity (tColours [['colour']] [c (1, 4, 5)], 0.6))
axis (side = 2, labels = c ('control','compressed','chilled'), at = 1:3, las = 1)
axis (side = 2, labels = c ('control','compressed','chilled'), at = 4:6, las = 1)
axis (side = 2, labels = c ('control','compressed','chilled'), at = 7:9, las = 1)
axis (side = 2, labels = c ('control','compressed','chilled'), at = 10:12, las = 1)
mtext (side = 2, line = 8, text = c ('0.5m','1.5m','2.5m','4.0m'), at = c (2,5,8,11))

# estimate treatment effect on start and end of growing season
tempData <- growingSeasonDates %>% mutate (treeId       = factor (treeId),
                                           treatment    = factor (treatment, levels = c (5,4,1)),
                                           sampleHeight = factor (sampleHeight))
mod1 <-lmer (formula = startOfGrowth ~ (1 | treeId) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod1)
mod2 <-lmer (formula = endOfGrowth ~ (1 | treeId) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod2)
#========================================================================================
