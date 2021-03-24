#========================================================================================
# Script to plot ring widths for the 2018 experiment at Harvard Forest, which was 
# generated from thin-sections of samples collected on the 2018-11-15 and 2019-10-24 
# using ROXAS. Additionally, we used the Wood Image Analysis and Database (WIAD) to 
# measure ring-width in thin-sections of samples collected on ??? and increment cores collected on the ??? and ???. 
# All data are publicly available on the Harvard Forest data archive.
#
# Data repository url:
# Code repository url:
#
# Author: Tim Rademacher (trademacher@fas.harvard.edu)
#
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('tibble'))  library ('tidyverse')
if (!existsFunction ('as_date')) library ('lubridate')
if (!exists ('tColours')) source ('plotingFunctions.R')

# load ring width data from thin-sections measured with WIAD
#----------------------------------------------------------------------------------------
if (!exists ('ringWidths')) source ('readRingWidths.R')

# load ring width from increment cores
#----------------------------------------------------------------------------------------
if (!exists ('increm')) source ('readIncrementRingWidths.R')

# load anatomical data from thin-sections
#----------------------------------------------------------------------------------------
source ('readAnatomicalData.R')

# Plot 2017, 2018, and 2019 ring from thin-sections as measured by WIAD versus ROXAS
#----------------------------------------------------------------------------------------
# We have ring widths measured using ROXAS and WIAD for samples from the thin-section of 
# samples collected on the 2019-10-24 and 2018-11-15.
# N.B. This also includes compressed trees for now.
#----------------------------------------------------------------------------------------
png (filename = './fig/ROXAS_vs_WIAD_ring_width_measurements_comparison.png', width = 700,
     height = 600)
par (mfrow = c (1, 1), mar = c (5, 5, 1, 1)) 

# plot 2018 ring widths from the samples collected in 2019-10-24
plot (x = ringWidths %>% filter (sampleDate == as_date ('2019-10-24')) %>% 
        select (Y2017) %>% filter (!is.na (Y2017)) %>% unlist (),
      y = anatomicalData %>%  filter (YEAR == 2017 & sampleDate == as_date ('2019-10-24')) %>%
        group_by (TREE, sampleHeight, YEAR, sampleDate) %>%
        dplyr::summarise (maxRW = max (MRW)) %>% ungroup %>% select (maxRW) %>% unlist (),
      xlab = expression (paste ('WIAD ring width (',mu,m,')',sep = '')), 
      ylab = expression (paste ('ROXAS ring width (',mu,m,')',sep = '')), 
      xlim = c (0, 4500), ylim = c (0, 4500),
      col = '#ffffb399', pch = 19, las = 1)

# add 2019 ring widths from the samples collected in 2019-10-24 
points (x = ringWidths %>% filter (sampleDate == as_date ('2019-10-24')) %>% 
          select (Y2018) %>% filter (!is.na (Y2018)) %>% unlist (),
        y = anatomicalData %>% group_by (TREE, sampleHeight, YEAR, sampleDate) %>% 
          filter (YEAR == 2018 & sampleDate == as_date ('2019-10-24')) %>% 
          dplyr::summarise (maxRW = max (MRW)) %>% ungroup %>% select (maxRW) %>% unlist (), 
        col = '#8dd3c799', pch = 19)

# add 2017 ring widths from the samples collected in 2019-10-24 
points (x = ringWidths %>% filter (sampleDate == as_date ('2019-10-24')) %>% 
          select (Y2019) %>% filter (!is.na (Y2019)) %>% unlist (),
        y = anatomicalData %>% group_by (TREE, sampleHeight, YEAR, sampleDate) %>% 
          filter (YEAR == 2019 & sampleDate == as_date ('2019-10-24')) %>% 
          dplyr::summarise (maxRW = max (MRW)) %>% ungroup %>% select (maxRW) %>% unlist (), 
        col = '#bebada99', pch = 19)

# add the 2017 ring widths from the samples collected in 2018-11-15 
points (x = ringWidths %>% filter (sampleDate == as_date ('2018-11-15') & 
                                     treeId %in% c (1:5, 11:15)) %>% 
          select (Y2017) %>% filter (!is.na (Y2017)) %>% unlist (),
        y = anatomicalData %>% group_by (TREE, sampleHeight, YEAR, sampleDate) %>% 
          filter (YEAR == 2017 & sampleDate == as_date ('2018-11-15') & 
                    sampleHeight %in% c (0.5, 1.5, 2.5, 4) & TREE != 6) %>% 
          dplyr::summarise (maxRW = max (MRW)) %>% ungroup %>% select (maxRW) %>% unlist (),  
          col = '#fb807299', pch = 19)

# add 2018 ring widths from the samples collected in 2018-11-15
points (x = ringWidths %>% filter (sampleDate == as_date ('2018-11-15') & 
                                     treeId %in% c (1:5, 11:15)) %>% 
          select (Y2018) %>% filter (!is.na (Y2018)) %>% unlist (),
        y = anatomicalData %>% group_by (TREE, sampleHeight, YEAR, sampleDate) %>% 
          filter (YEAR == 2018 & sampleDate == as_date ('2018-11-15') & 
                    sampleHeight %in% c (0.5, 1.5, 2.5, 4) & TREE != 6) %>% 
          dplyr::summarise (maxRW = max (MRW)) %>% ungroup %>% select (maxRW) %>% unlist (), 
        col = '#80b1d399', pch = 19)

# add 1:1 line on which they should fall
abline (a = 0, b = 1, col = '#99999999')

# add legend of the colours used
legend (x = 100, y = 4500, box.lty = 0, pch = 19, title = '2018-11-15',
        col = c ('#fb807299','#80b1d399'),
        legend = c ('2017','2018'))
legend (x = 100, y = 4000, box.lty = 0, pch = 19, title = '2019-10-24',
        col = c ('#ffffb399','#8dd3c799','#bebada99'),
        legend = c ('2017','2018', '2019'))
dev.off ()

# Plot ring width from 2019-10-24 samples versus from 2018-11-15 samples for WIAD
#----------------------------------------------------------------------------------------
# TR - NB: I could add addtional sample dates in various panels (9 sampling dates are measured) 
png (filename = './fig/ring_width_between-sample_variability.png', width = 700, height = 600)
par (mfrow = c (1, 1), mar = c (5, 5, 1, 1)) 

# plot 2018 rings
plot (x = ringWidths %>% filter (sampleDate == as_date ('2018-11-15')) %>% 
        select (Y2018) %>% unlist (),
      y = ringWidths %>% filter (sampleDate == as_date ('2019-10-24')) %>% 
        select (Y2018) %>% unlist (), pch = 19, col = '#8dd3c799',
      xlab = expression (paste ('2018-11-15 sample ring widths (',mu,m,')',sep = '')), 
      ylab = expression (paste ('2019-10-24 sample ring widths (',mu,m,')',sep = '')),
      las = 1, xlim = c (0, 4500), ylim = c (0, 4500))

# add 2017 rings
points (x = ringWidths %>% filter (sampleDate == as_date ('2018-11-15')) %>% 
          select (Y2017) %>% unlist (),
        y = ringWidths %>% filter (sampleDate == as_date ('2019-10-24')) %>% 
          select (Y2017) %>% unlist (), pch = 19, col = '#bebada99')

# add 2016 rings
points (x = ringWidths %>% filter (sampleDate == as_date ('2018-11-15')) %>% 
          select (Y2016) %>% unlist (),
        y = ringWidths %>% filter (sampleDate == as_date ('2019-10-24')) %>% 
          select (Y2016) %>% unlist (), pch = 19, col = '#fb807299')

# add 2015 rings
points (x = ringWidths %>% filter (sampleDate == as_date ('2018-11-15')) %>% 
          select (Y2015) %>% unlist (),
        y = ringWidths %>% filter (sampleDate == as_date ('2019-10-24')) %>% 
          select (Y2015) %>% unlist (), pch = 19, col = '#80b1d399')

# add 2014 rings
points (x = ringWidths %>% filter (sampleDate == as_date ('2018-11-15')) %>% 
          select (Y2014) %>% unlist (),
        y = ringWidths %>% filter (sampleDate == as_date ('2019-10-24')) %>% 
          select (Y2014) %>% unlist (), pch = 19, col = '#ffffb399')

# add 1:1 line on which they should fall
abline (a = 0, b = 1, col = '#99999999')

# add legend of the colours used
legend (x = 100, y = 4500, box.lty = 0, pch = 19, title = 'Year',
        col = c ('#8dd3c799','#bebada99','#fb807299','#80b1d399','#ffffb399'),
        legend = 2018:2014)

# Get correlation of ring widths from one sample as a function from another
#----------------------------------------------------------------------------------------
mod0 <- lm (ringWidths %>% filter (sampleDate == as_date ('2019-10-24')) %>% 
      select (Y2014, Y2015, Y2016, Y2017, Y2018) %>% unlist () ~ 
      0 + ringWidths %>% filter (sampleDate == as_date ('2018-11-15')) %>% 
      select (Y2014, Y2015, Y2016, Y2017, Y2018) %>% unlist ())
mod1 <- lm (ringWidths %>% filter (sampleDate == as_date ('2019-10-24')) %>% 
              select (Y2018) %>% unlist () ~ 
              0 + ringWidths %>% filter (sampleDate == as_date ('2018-11-15')) %>% 
              select (Y2018) %>% unlist ())
#summary (mod0)
abline (mod0, col = '#666666', lwd = 2)
abline (mod1, col = '#8dd3c7', lwd = 2)
r2 <- summary (mod0)$adj.r.squared
mylabel = bquote (italic (R)^2 == .(format (r2, digits = 3)))
text (x = 4100, y = 500, labels = mylabel, col = '#666666')
r2 <- summary (mod1)$adj.r.squared
mylabel = bquote (italic (R)^2 == .(format (r2, digits = 3)))
text (x = 4100, y = 800, labels = mylabel, col = '#8dd3c7')
dev.off ()
rm (mod0, mod1, r2, mylabel)

# Plot ring width from 2019-10-24 versus 2018-11-15 samples (WIAD measurements)
#----------------------------------------------------------------------------------------
png (filename = './fig/ring_growth_index_between-sample_variability.png')
par (mfrow = c (1, 1), mar = c (5, 5, 1, 1)) 

# plot line for each indiviual tree and sampling height
plot (x = ringWidths %>% filter (sampleDate == as_date ('2018-11-15')) %>% 
        select (RWI2018) %>% unlist (),
      y = ringWidths %>% filter (sampleDate == as_date ('2019-10-24')) %>% 
        select (RWI2018) %>% unlist (), pch = 19, col = '#8dd3c799',
      xlab = expression (paste ('2019-10-24 sample ring width index (',mu,m,')',sep = '')), 
      ylab = expression (paste ('2018-11-15 sample ring width index (',mu,m,')',sep = '')),
      las = 1, xlim = c (0, 2.5), ylim = c (0, 2.5))

# add 1:1 line on which they should fall
abline (a = 0, b = 1, col = '#99999999')

# add legend of the colours used
legend (x = 0, y = 2.5, box.lty = 0, pch = 19, col = '#8dd3c799',
        legend = 'Radial growth index 2018')

mod1 <- lm (ringWidths %>% filter (sampleDate == as_date ('2019-10-24')) %>% select (RWI2018) %>% unlist () ~
            0 + ringWidths %>% filter (sampleDate == as_date ('2018-11-15')) %>% select (RWI2018) %>% unlist ())
#summary (mod1)
abline (mod1, col = '#8dd3c7', lwd = 2)
r2 <- summary (mod1)$adj.r.squared
mylabel = bquote (italic (R)^2 == .(format (r2, digits = 3)))
text (x = 0.2, y = 2.2, labels = mylabel, col = '#8dd3c7')
dev.off ()
rm (mod1, r2, mylabel)

#========================================================================================