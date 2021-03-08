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
if (!existsFunction ('vioplot')) library ('vioplot')

# load ring widths (ringWidths) and the fitted growing season data based on general 
# additive models from thin-sections measured with WIAD
#----------------------------------------------------------------------------------------
if (!exists ('growingSeasonDates')) source ('extractGrowingSeasonDates.R')
if (!exists ('ringWidths')) source ('readRingWidths.R')
if (!exists ('incrementRingWidths')) source ('readIncrementRingWidths.R')

# make box plot of onset and cessation of growth for each treatment
#----------------------------------------------------------------------------------------
g <- ggplot (growingSeasonDates) +
  geom_boxplot (aes (x = factor (sampleHeight), y = startOfGrowth, fill =factor (treatment)),
               alpha = 0.1, 
               colour = rep (tColours [['colour']] [c (1,4,5)], 4),
               position = c ()) +
  geom_boxplot (aes (x = factor (sampleHeight), y = endOfGrowth, fill = factor (treatment)),
               alpha = 0.7, colour = rep (tColours [['colour']] [c (1,4,5)], 4)) +
  scale_fill_manual (values = tColours [['colour']] [c (1,4,5)], labels = c ('Control','Compressed','Chilled')) +
  labs (x = 'Sample height (m)', y = 'Day of year', fill = 'Treatment') +
  scale_x_discrete (limits = c (0.5, 1.5, 2.5, 4.0)) + 
  scale_y_continuous (breaks = seq (0, 360 , by = 60), limits = c (0, 365)) +
  coord_flip () + theme_classic ()
g

# estimate treatment effect on start and end of growing season
#----------------------------------------------------------------------------------------
tempData <- growingSeasonDates %>%
  mutate (treeId       = factor (treeId),
          treatment    = factor (treatment, levels = c (5,4,1)),
          sampleHeight = factor (sampleHeight))
mod1 <-lmer (formula = startOfGrowth ~ (1 | treeId) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod1)
mod2 <-lmer (formula = endOfGrowth ~ (1 | treeId) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod2); rm (tempData)

# plot final radial growth versus by treatment * sample height
#----------------------------------------------------------------------------------------
tempData <- filter (ringWidths, sampleDate == as_date ('2018-11-15')) %>% 
  mutate (treeId       = factor (treeId),
          treatment    = factor (treatment, levels = c (5,4,1)),
          sampleHeight = factor (sampleHeight))
g <- ggplot (tempData) +
  geom_boxplot (aes (x = sampleHeight, y = Y2018, fill = treatment),
                alpha = 0.7, colour = rep (tColours [['colour']] [c (1,4,5)], 4)) + 
  coord_flip () +
  labs (x = 'Sample Height (m)',
        y = expression (paste ('Ring width (',mu,'m)', sep = ''))) +
  scale_fill_manual (values = tColours [['colour']] [c (1,4,5)], labels = c ('Control','Compressed','Chilled'))
g
# Were there differences in final ring with among treatments?
mod3 <- lmer (formula = Y2018 ~ (1 | treeId) + treatment:sampleHeight,
              data = tempData, REML = TRUE)
summary (mod3); rm (tempData)

# plot final radial growth versus by treatment * sample height
#----------------------------------------------------------------------------------------
tempData <- filter (ringWidths, sampleDate == as_date ('2019-10-24')) %>% 
  mutate (treeId       = factor (treeId),
          treatment    = factor (treatment, levels = c (5,4,1)),
          sampleHeight = factor (sampleHeight))
g <- ggplot (tempData) +
  geom_boxplot (aes (x = sampleHeight, y = Y2018, fill = treatment),
                alpha = 0.7, colour = rep (tColours [['colour']] [c (1,4,5)], 4)) + 
  coord_flip () +
  labs (x = 'Sample Height (m)',
        y = expression (paste ('Ring width (',mu,'m)', sep = ''))) +
  scale_fill_manual (values = tColours [['colour']] [c (1,4,5)], labels = c ('Control','Compressed','Chilled'))
g
# Were there differences in final ring with among treatments?
mod4 <- lmer (formula = Y2018 ~ (1 | treeId) + treatment:sampleHeight,
              data = tempData, REML = TRUE)
summary (mod4); rm (tempData)

# plot final radial growth versus by treatment * sample height
#----------------------------------------------------------------------------------------
tempData <- filter (ringWidths, sampleDate == as_date ('2018-11-15')) %>% 
  mutate (treeId       = factor (treeId),
          treatment    = factor (treatment, levels = c (5,4,1)),
          sampleHeight = factor (sampleHeight))
g <- ggplot (tempData) +
  geom_boxplot (aes (x = sampleHeight, y = RWI2018, fill = treatment),
                alpha = 0.7, colour = rep (tColours [['colour']] [c (1,4,5)], 4)) + 
  coord_flip () +
  labs (x = 'Sample Height (m)', y = 'Radial growth index') +
  scale_fill_manual (values = tColours [['colour']] [c (1,4,5)], labels = c ('Control','Compressed','Chilled'))
g
# Were there differences in final ring with among treatments?
mod5 <- lmer (formula = RWI2018 ~ (1 | treeId) + treatment:sampleHeight,
              data = tempData, REML = TRUE)
summary (mod5); rm (tempData)

# plot final radial growth versus by treatment * sample height
#----------------------------------------------------------------------------------------
tempData <- filter (ringWidths, sampleDate == as_date ('2019-10-24')) %>% 
  mutate (treeId       = factor (treeId),
          treatment    = factor (treatment, levels = c (5,4,1)),
          sampleHeight = factor (sampleHeight))
g <- ggplot (tempData) +
  geom_boxplot (aes (x = sampleHeight, y = RWI2018, fill = treatment),
                alpha = 0.7, colour = rep (tColours [['colour']] [c (1,4,5)], 4)) + 
  coord_flip () +
  labs (x = 'Sample Height (m)', y = 'Radial growth index') +
  scale_fill_manual (values = tColours [['colour']] [c (1,4,5)], labels = c ('Control','Compressed','Chilled'))
g
# Were there differences in final ring with among treatments?
mod6 <- lmer (formula = RWI2018 ~ (1 | treeId) + treatment:sampleHeight,
              data = tempData, REML = TRUE)
summary (mod6); rm (tempData)

# How much growth had occured at the start of the experiment?
#----------------------------------------------------------------------------------------
tempData <- ringWidths %>% group_by (sampleHeight, treeId) %>%
  mutate (maxRGI = max (Y2018, na.rm = TRUE)) %>%
  filter (sampleDate %in% c (as_date ('2018-06-19'), as_date ('2019-10-24'))) %>%
  mutate (f2018 = Y2018 / maxRGI) %>% ungroup
# Across all height and treatments
filter (tempData, sampleDate == as_date ('2018-06-19')) %>% 
  select (f2018) %>% 
  summarise (mean = mean (f2018, na.rm = TRUE),
             se = se (f2018))
# By height and treatment
filter (tempData, sampleDate == as_date ('2018-06-19')) %>%
  group_by (treatment, sampleHeight) %>%
  summarise (mean = mean (f2018, na.rm = TRUE),
             se = se (f2018))

# How much growth had occurred after the experimental onset
#----------------------------------------------------------------------------------------
filter (tempData, sampleDate == as_date ('2018-06-19')) %>% 
  mutate (remainingGrowthFraction = 1 - f2018) %>%
  group_by (treatment, sampleHeight) %>%
  summarise (mean = mean (remainingGrowthFraction, na.rm = TRUE),
             se = se (remainingGrowthFraction))
rm (tempData)

# Were there differences in radial growth after the experimental onset
#----------------------------------------------------------------------------------------
tempData <- ringWidths %>% group_by (sampleHeight, treeId) %>%
  mutate (maxRGI = max (Y2018, na.rm = TRUE)) %>%
  filter (sampleDate == as_date ('2018-06-19')) %>%
  mutate (remainingGrowthFraction = 1 - (Y2018 / maxRGI)) %>% 
  ungroup () %>% 
  select (treeId, treatment, sampleHeight, Y2018, maxRGI, remainingGrowthFraction) %>%
  mutate (treeId = factor (treeId),
          treatment    = factor (treatment, levels = c (5,4,1)),
          sampleHeight = factor (sampleHeight))

mod7 <- lmer (formula = remainingGrowthFraction ~ (1 | treeId) + treatment:sampleHeight, 
              data = tempData, REML = TRUE)
summary (mod7); rm (tempData)

# Was radial growth comparable in the preceeding seven years?
#----------------------------------------------------------------------------------------
tempData <- ringWidths %>% 
  filter (sampleDate %notin% c (as_date ('2018-01-01'), as_date ('2018-05-01'))) %>%
  select (1:14) %>%
  pivot_longer (cols = 7:14, names_prefix = 'Y', names_to = 'year', values_to = 'RW')
mod8 <- lmer (formula = RW ~ (1 | treeId) + factor (year) + factor (sampleHeight) + 
                             factor (treatment),
              data = tempData)
summary (mod8); rm (tempData)

# How do the increment ring widths compare to microcore ring widths?
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (x = 500,
      y = 500,
      col = 'white',
      axes = FALSE,
      xlim = c (0, 3000),
      ylim = c (0, 3000),
      xlab = expression (paste ('Thin-section ring width (',mu,'m)', sep = '')),
      ylab =expression (paste ('Increment core ring width (',mu,'m)', sep = '')))
axis (side = 1)
axis (side = 2, las = 1)
# Draw a 1:1 line
#----------------------------------------------------------------------------------------
abline (a = 0, b = 1, col = '#66666699') 
# Draw mean measurements and their standard error 
#----------------------------------------------------------------------------------------
for (t in 1:15) {
  for (y in 2010: 2017) {
    Year <- paste ('Y', y, sep = '')
    tmpX <- ringWidths %>% 
      filter (treeId == t, 
              sampleHeight == 1.5,#%in% c (0.5, 1.5, 2.5, 4.0), 
              sampleDate != as_date ('2018-01-01')) %>% 
      select (Year) %>% unlist () 
    tmpX <- tibble (mean = mean (tmpX [which (tmpX != 1)], na.rm = TRUE),# * 1.5413335,
                    sd = sd (tmpX [which (tmpX != 1)], na.rm = TRUE),
                    se = se (tmpX [which (tmpX != 1)])) 
    tmpY <- incrementRingWidths %>%  
      filter (treeId == t) %>% select (Year) %>% unlist ()
    tmpY <- tibble (mean = mean (tmpY [which (tmpY != 1)], na.rm = TRUE),
                    sd = sd (tmpY [which (tmpY != 1)], na.rm = TRUE),
                    se = se (tmpY [which (tmpY != 1)]))
    arrows (x0 = tmpX [['mean']] - tmpX [['se']], x1 = tmpX [['mean']] + tmpX [['se']], 
            y0 = tmpY [['mean']], length = 0.1, angle = 90, code = 3, col = '#66666666')
    arrows (y0 = tmpY [['mean']] - tmpY [['se']], y1 = tmpY [['mean']] + tmpY [['se']], 
            x0 = tmpX [['mean']], length = 0.1, angle = 90, code = 3, col = '#66666666')
    points (x = tmpX [['mean']], y = tmpY [['mean']], pch = 21, bg = 'white', lwd = 2,
            col = '#66666666')  
  }
} 

# Did groups grow differently in previous years?
#----------------------------------------------------------------------------------------
tempData <- ringWidths %>%
  filter (sampleDate %notin% c (as_date ('2018-01-01'), as_date ('2018-05-01'))) %>%
  mutate (treeId       = factor (treeId),
          treatment    = factor (treatment, levels = c (1,4,5)),
          sampleHeight = factor (sampleHeight, levels = c (4.0,2.5,1.5,0.5))) %>% 
  select (1:4,7:14) %>% 
  pivot_longer (cols = 5:12, names_to = 'year', names_prefix = 'Y', values_to = 'RW') %>%
  mutate (year = factor (year))
g <- ggplot (tempData) +
  geom_boxplot (aes (x = sampleHeight, y = RW, fill = treatment),
                alpha = 0.7, colour = rep (tColours [['colour']] [c (1,4,5)], 4)) + 
  coord_flip () +
  labs (x = 'Sample Height (m)', y = expression (paste ('Ring widths (',mu,'m)', sep = ''))) +
  scale_fill_manual (values = tColours [['colour']] [c (1,4,5)], labels = c ('Control','Compressed','Chilled'))
g
# Were there differences in the 2010 to 2017 ring width (previous years) among treatments?
mod9 <- lmer (formula = RW ~ (1 | treeId) + year + treatment + sampleHeight,
              data = tempData, REML = TRUE)
summary (mod9); rm (tempData)

# Did groups grow differently in 2019?
#----------------------------------------------------------------------------------------
tempData <- ringWidths %>%
  filter (sampleDate == as_date ('2019-10-24')) %>%
  mutate (treeId       = factor (treeId),
        treatment    = factor (treatment, levels = c (5,4,1)),
        sampleHeight = factor (sampleHeight))
g <- ggplot (tempData) +
  geom_boxplot (aes (x = sampleHeight, y = Y2019, fill = treatment),
                alpha = 0.7, colour = rep (tColours [['colour']] [c (1,4,5)], 4)) + 
  coord_flip () +
  labs (x = 'Sample Height (m)', y = 'Radial growth index') +
  scale_fill_manual (values = tColours [['colour']] [c (1,4,5)], labels = c ('Control','Compressed','Chilled'))
g
# Were there differences in the 2019 ring width (following year) among treatments?
mod10 <- lmer (formula = Y2019 ~ (1 | treeId) + treatment:sampleHeight,
              data = tempData, REML = TRUE)
summary (mod10); rm (tempData)

# clean up 
#----------------------------------------------------------------------------------------
rm (mo1, mod2, tempData)
#========================================================================================
