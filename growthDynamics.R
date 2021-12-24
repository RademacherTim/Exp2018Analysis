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
if (!existsFunction ('ggplot'))  library ('ggplot2')
if (!existsFunction ('lmer'))    library ('lme4')
if (!existsFunction ('vioplot')) library ('vioplot')
if (!exists ('tColours')) source ('plotingFunctions.R')

# load ring widths (ringWidths) and the fitted growing season data based on general 
# additive models from thin-sections measured with WIAD
#----------------------------------------------------------------------------------------
if (!exists ('growingSeasonDates')) source ('extractGrowingSeasonDates.R')
if (!exists ('ringWidths')) source ('readRingWidths.R')
if (!exists ('incrementRingWidths')) source ('readIncrementRingWidths.R')

# extract ring width data
#----------------------------------------------------------------------------------------
tempData <- ringWidths %>% filter (treatment %in% c (1, 5)) %>%
  select (Y2018, RWI2018, treeId, treatment, sampleHeight, sampleDate) %>%
  group_by (sampleHeight, treeId) 

# add a row with the maxRWIfit from the growingSeasonDates tibble
#----------------------------------------------------------------------------------------
tempData <- merge (tempData, growingSeasonDates, by = c ('treeId','treatment','sampleHeight')) %>%
  mutate (RGI = RWI2018 / maxRWIfit)

# make four panel figure of growth over time for control and chilled trees by sample height
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2018ChillingRelativeVolumeGrowthDynamics.png', width = 800 , height = 700)
layout (matrix (1:4, nrow = 4), heights = c (1,1,1,1.4))
for (h in c (4.0, 2.5, 1.5, 0.5)) {
  
  # determine panel margins
  if (h != 0.5) {
    par (mar = c (1, 5, 1, 1))
  } else {
    par (mar = c (5, 5, 1, 1))
  }
  
  # plot 
  plot (x = tempData %>% filter (treatment == 1, sampleHeight == h) %>% 
          select (sampleDate) %>% unlist () - 17532,
        y = tempData %>% filter (treatment == 1, sampleHeight == h) %>% 
          select (RGI) %>% unlist (),
        xlim = c (110, 365), ylim = c (0, 2.05), axes = FALSE, pch = 19, 
        las = 1, xlab = ifelse (h != 0.5, '', 'Day of the year'), 
        ylab = '', 
        col = addOpacity (tColours [['colour']] [1], 0.5)) 

  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- abline (v = lubridate::yday (c ('2018-06-25','2018-09-03')), 
                 col = '#999999', lty = 2, lwd = 1)
  
  points (x = tempData %>% filter (treatment == 5, sampleHeight == h) %>% 
            select (sampleDate) %>% unlist () - 17532,
          y = tempData %>% filter (treatment == 5, sampleHeight == h) %>% 
            select (RGI) %>% unlist (), pch = 23, 
          col = addOpacity (tColours [['colour']] [5], 0.5)) 

  # add axis
  if (h != 0.5) {
    axis (side = 1, at = seq (0, 360, 60), labels = rep ('', 7))
  } else {
    axis (side = 1, at = seq (0, 360, 60), cex.axis = 1.4)
  }  
  axis (side = 2, at = seq (0, 1.2, 0.3), las = 1, cex.axis = 1.4)
  mtext (side = 2, line = 3, text = 'Relative growth (fration)', at = 0.7, cex = 0.7)
  
  # add monotonic GAMs for each tree
  for (t in c (1:5, 11:15)) {
  
    # get tree-specific volume growth
    treeData <- tempData %>% mutate (doy = lubridate::yday (sampleDate)) %>% 
      filter (treeId == t & sampleHeight == h) %>% arrange (by = sampleDate)
      
    # Assume that the ring measured in 2019 is the end of the year growth
    if (sum (treeData [['sampleDate']] == as_date ('2019-10-24'), na.rm = TRUE) > 0) {
      treeData [['sampleDate']] [treeData [['sampleDate']] == as_date ('2019-10-24')] <- as_date ('2018-12-31')
      treeData [['doy']] [treeData [['sampleDate']] == as_date ('2018-12-31')] <- yday ('2018-12-31')
    }
    
    # Fit general additive model to growth data
    fit.gam <- scam::scam (RGI ~ s (doy, k = 8, bs = 'mpi'), 
                           data   = treeData, 
                           family = quasipoisson)
    
    lines (x = 1:365, y = exp (predict (fit.gam, newdata = data.frame (doy = 1:365))),
           col = tColours [['colour']] [unique (treeData [['treatment']])], 
           lty = ifelse (unique (treeData [['treatment']]) == 1, 1, 2), lwd = 0.3)
  }
  
  # add monotonic GAM for treatment 
  for (t in c (1, 5)) {
    
    # get treatment specific data
    treatmentData <- tempData %>% mutate (doy = lubridate::yday (sampleDate)) %>% 
      filter (treatment == t & sampleHeight == h) %>% arrange (by = sampleDate)
  
    # assume that the ring measured in 2019 is the end of the year growth
    if (sum (treatmentData [['sampleDate']] == as_date ('2019-10-24'), na.rm = TRUE) > 0) {
      treatmentData [['sampleDate']] [treatmentData [['sampleDate']] == as_date ('2019-10-24')] <- as_date ('2018-12-31')
      treatmentData [['doy']] [treatmentData [['sampleDate']] == as_date ('2018-12-31')] <- yday ('2018-12-31')
    }
    
    # Fit general additive model to growth data
    fit.gam <- scam::scam (RGI ~ s (doy, k = 8, bs = 'mpi'), 
                           data   = treatmentData, 
                           family = quasipoisson)
    
    # add confidence interval for the model
    m <- predict (fit.gam, newdata = data.frame (doy = 1:365), type = 'link', se.fit = TRUE) 
    polygon (x = c (160:365, 365:160), 
             y = exp (c (m$fit [160:365] + 2 * m$se.fit [160:365], 
                         rev (m$fit [160:365] - 2 * m$se.fit [160:365]))), 
             lty = 0,
             col = addOpacity (tColours [['colour']] [t], 0.5))
    
    # add treatment mean behaviour
    lines (x = 1:365, y = exp (m$fit), col = tColours [['colour']] [t], lwd = 2,
           lty = ifelse (t == 1, 1, 2))
    
    # add mean and standard error for start of the growing season
    arrows (x0 = mean (treatmentData [['startOfGrowth']]) - se (treatmentData [['startOfGrowth']]),
            x1 = mean (treatmentData [['startOfGrowth']]) + se (treatmentData [['startOfGrowth']]), 
            y0 = 1.9 + ifelse (t == 1, -0.1, 0.1), col = tColours [['colour']] [t], 
            length = 0, angle = 90, code = 3, lwd = 2)
    points (x = mean (treatmentData [['startOfGrowth']]), 
            y = 1.9 + ifelse (t == 1, -0.1, 0.1), pch = ifelse (t == 1, 19, 23), 
            col = tColours [['colour']] [t], cex = 1.5, bg = 'white', lwd = 2)
    
    # add mean and standard error for end of the growing season
    arrows (x0 = mean (treatmentData [['endOfGrowth']]) - se (treatmentData [['endOfGrowth']]),
            x1 = mean (treatmentData [['endOfGrowth']]) + se (treatmentData [['endOfGrowth']]), 
            y0 = 1.9 + ifelse (t == 1, -0.1, 0.1), col = tColours [['colour']] [t], 
            length = 0, angle = 90, code = 3, lwd = 2)
    points (x = mean (treatmentData [['endOfGrowth']]), 
            y = 1.9 + ifelse (t == 1, -0.1, 0.1), pch = ifelse (t == 1, 19, 23), 
            col = tColours [['colour']] [t], cex = 1.5, bg = 'white', lwd = 2)
  } # end treatment loop
} # end sample height loop

# add legend 
legend (x = 110, y = 1.4, legend = c ('',''), bg = 'transparent', box.lty = 0,
        lty = 1:2, col = tColours [['colour']] [c (1, 5)], lwd = 2, cex = 1.4)
legend (x = 125, y = 1.4, legend = c ('control','chilled'), bg = 'transparent', box.lty = 0,
        pch = c (19, 23), col = tColours [['colour']] [c (1, 5)], cex = 1.4)
dev.off ()

# make four panel figure of absolute growth over time for control and chilled trees by sample height
#----------------------------------------------------------------------------------------
PLOTTREE <- FALSE
png (filename = './fig/Exp2018ChillingAbsoluteVolumeGrowthDynamics.png', width = 800 , height = 700)
layout (matrix (1:4, nrow = 4), heights = c (1, 1, 1, 1.2))
for (h in c (4.0, 2.5, 1.5, 0.5)) {
  
  # determine panel margins
  if (h != 0.5) {
    par (mar = c (1, 6, 1, 1))
  } else {
    par (mar = c (4, 6, 1, 1))
  }
  
  # plot 
  plot (x = tempData %>% filter (treatment == 1, sampleHeight == h) %>% 
          select (sampleDate) %>% unlist () - 17532,
        y = tempData %>% filter (treatment == 1, sampleHeight == h) %>% 
          select (Y2018) %>% unlist (),
        xlim = c (122, 324), ylim = c (0, ifelse (PLOTTREE, 4600, 2050)), 
        axes = FALSE, pch = 19, las = 1, 
        xlab = '', ylab = '', col = 'white') 
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- abline (v = lubridate::yday (c ('2018-06-25','2018-09-03')), 
                 col = '#999999', lty = 2, lwd = 1)
  
  # Add points for chilled trees
  #--------------------------------------------------------------------------------------
  if (PLOTTREE) {
    points (x = tempData %>% filter (treatment == 5, sampleHeight == h) %>% 
              select (sampleDate) %>% unlist () - 17532,
            y = tempData %>% filter (treatment == 5, sampleHeight == h) %>% 
              select (Y2018) %>% unlist (), pch = 23, 
            col = addOpacity (tColours [['colour']] [5], 0.5)) 
    points (x = tempData %>% filter (treatment == 1, sampleHeight == h) %>% 
              select (sampleDate) %>% unlist () - 17532,
            y = tempData %>% filter (treatment == 1, sampleHeight == h) %>% 
              select (Y2018) %>% unlist (), pch = 19, 
            col = addOpacity (tColours [['colour']] [1], 0.5)) 
  }
  # add x-axis
  if (h != 0.5) {
    axis (side = 1, at = c (91, 121, 152, 182, 213, 244, 274, 305, 335), 
          labels = rep ('', 9))
  } else {
    axis (side = 1, at = c (91, 121, 152, 182, 213, 244, 274, 305, 335), 
          labels = c ('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'), 
          cex.axis = 1.4)
    
    # add tick marks indicating sampling dates
    points (x = yday (unique (tempData [['sampleDate']])),
            y =  rep (-100, length (unique (tempData [['sampleDate']]))), 
            pch = 3, cex = 1.5, col = '#e37222', lwd = 3)
  }  
  
  # add y-axis
  if (PLOTTREE) {
    axis (side = 2, at = seq (0, 4000, 1000), labels = seq (0, 4000, 1000), las = 1, 
          cex.axis = 1.4)
  } else {
    axis (side = 2, at = seq (0, 2000, 1000), labels = seq (0, 2000, 1000), las = 1, 
          cex.axis = 1.4)
  }
  mtext (side = 2, line = 4, 
         text = expression (paste ('Volume growth (',mu,m,')',sep = '')), 
         at = ifelse (PLOTTREE, 2000, 900), cex = 0.7)
  
  # add monotonic GAMs for each tree
  if (PLOTTREE) {
    for (t in c (1:5, 11:15)) {
    
      # get tree-specific volume growth
      treeData <- tempData %>% mutate (doy = lubridate::yday (sampleDate)) %>% 
        filter (treeId == t & sampleHeight == h) %>% arrange (by = sampleDate)
      
      # Assume that the ring measured in 2019 is the end of the year growth
      if (sum (treeData [['sampleDate']] == as_date ('2019-10-24'), na.rm = TRUE) > 0) {
        treeData [['sampleDate']] [treeData [['sampleDate']] == as_date ('2019-10-24')] <- as_date ('2018-12-31')
        treeData [['doy']] [treeData [['sampleDate']] == as_date ('2018-12-31')] <- yday ('2018-12-31')
      }
      
      # Fit general additive model to growth data
      fit.gam <- scam::scam (Y2018 ~ s (doy, k = 8, bs = 'mpi'), 
                             data   = treeData, 
                             family = quasipoisson)
      
      lines (x = 1:365, y = exp (predict (fit.gam, newdata = data.frame (doy = 1:365))),
             col = tColours [['colour']] [unique (treeData [['treatment']])], 
             lty = ifelse (unique (treeData [['treatment']]) == 1, 1, 2), lwd = 0.3)
    }
  }
  
  # add monotonic GAM for treatment 
  for (t in c (1, 5)) {
    
    # get treatment specific data
    treatmentData <- tempData %>% mutate (doy = lubridate::yday (sampleDate)) %>% 
      filter (treatment == t & sampleHeight == h) %>% arrange (by = sampleDate)
    
    # assume that the ring measured in 2019 is the end of the year growth
    if (sum (treatmentData [['sampleDate']] == as_date ('2019-10-24'), na.rm = TRUE) > 0) {
      treatmentData [['sampleDate']] [treatmentData [['sampleDate']] == as_date ('2019-10-24')] <- as_date ('2018-12-31')
      treatmentData [['doy']] [treatmentData [['sampleDate']] == as_date ('2018-12-31')] <- yday ('2018-12-31')
    }
    
    # Fit general additive model to growth data
    fit.gam <- scam::scam (Y2018 ~ s (doy, k = 8, bs = 'mpi'), 
                           data   = treatmentData, 
                           family = quasipoisson)
    
    # add confidence interval for the model
    m <- predict (fit.gam, newdata = data.frame (doy = 1:365), type = 'link', se.fit = TRUE) 
    polygon (x = c (160:365, 365:160), 
             y = exp (c (m$fit [160:365] + m$se.fit [160:365], 
                         rev (m$fit [160:365] - m$se.fit [160:365]))), 
             lty = 0,
             col = addOpacity (tColours [['colour']] [t], 0.3))
    
    # add treatment mean behaviour
    lines (x = 1:365, y = exp (m$fit), col = tColours [['colour']] [t], lwd = 2,
           lty = ifelse (t == 1, 1, 2))
    
    # add mean and standard error for start of the growing season
    arrows (x0 = mean (treatmentData [['startOfGrowth']]) - 
              se (treatmentData [['startOfGrowth']]),
            x1 = mean (treatmentData [['startOfGrowth']]) + 
              se (treatmentData [['startOfGrowth']]), 
            y0 = ifelse (PLOTTREE, 4400, 1800) + ifelse (t == 1, -100, 100), 
            col = tColours [['colour']] [t], 
            length = 0, angle = 90, code = 3, lwd = 2)
    points (x = mean (treatmentData [['startOfGrowth']]), 
            y = ifelse (PLOTTREE, 4400, 1800) + ifelse (t == 1, -100, 100), 
            pch = ifelse (t == 1, 19, 23), 
            col = tColours [['colour']] [t], cex = 2.5, bg = 'white', lwd = 2)
    
    # add mean and standard error for end of the growing season
    arrows (x0 = mean (treatmentData [['endOfSeasonDOY']], na.rm = TRUE) - 
              se (treatmentData [['endOfSeasonDOY']]),
            x1 = mean (treatmentData [['endOfSeasonDOY']], na.rm = TRUE) + 
              se (treatmentData [['endOfSeasonDOY']]), 
            y0 = ifelse (PLOTTREE, 4400, 1800) + ifelse (t == 1, -100, 100), 
            col = tColours [['colour']] [t], 
            length = 0, angle = 90, code = 3, lwd = 2)
    points (x = mean (treatmentData [['endOfSeasonDOY']], na.rm = TRUE), 
            y = ifelse (PLOTTREE, 4400, 1800) + ifelse (t == 1, -100, 100), 
            pch = ifelse (t == 1, 19, 23), 
            col = tColours [['colour']] [t], cex = 2.5, bg = 'white', lwd = 2)
    
    
    # add mean and standard error of the end of growing season for trees with 
    # intra-annual density fluctuation 
    #------------------------------------------------------------------------------------
    if (t == 5 & sum (treatmentData [['densityFluctuation']], na.rm = TRUE) > 0) {
      arrows (x0 = mean (treatmentData [['endOfSeasonDOY']] [treatmentData [['densityFluctuation']]] , na.rm = TRUE) - 
                se (treatmentData [['endOfSeasonDOY']] [treatmentData [['densityFluctuation']]]),
              x1 = mean (treatmentData [['endOfSeasonDOY']] [treatmentData [['densityFluctuation']]], na.rm = TRUE) + 
                se (treatmentData [['endOfSeasonDOY']] [treatmentData [['densityFluctuation']]]), 
              y0 = ifelse (PLOTTREE, 4400, 1800) + ifelse (t == 1, -100, 100), 
              col = '#666666', 
              length = 0, angle = 90, code = 3, lwd = 2)
      points (x = mean (treatmentData [['endOfSeasonDOY']] [treatmentData [['densityFluctuation']]], na.rm = TRUE), 
              y = ifelse (PLOTTREE, 4400, 1800) + ifelse (t == 1, -100, 100), 
              pch = ifelse (t == 1, 19, 24), 
              col = '#666666', cex = 2, bg = 'white', lwd = 2)
      
      arrows (x0 = mean (treatmentData [['endOfSeasonDOY']] [!treatmentData [['densityFluctuation']]] , na.rm = TRUE) - 
                se (treatmentData [['endOfSeasonDOY']] [!treatmentData [['densityFluctuation']]]),
              x1 = mean (treatmentData [['endOfSeasonDOY']] [!treatmentData [['densityFluctuation']]], na.rm = TRUE) + 
                se (treatmentData [['endOfSeasonDOY']] [!treatmentData [['densityFluctuation']]]), 
              y0 = ifelse (PLOTTREE, 4400, 1800) + ifelse (t == 1, -100, 100), 
              col = '#666666', 
              length = 0, angle = 90, code = 3, lwd = 2)
      points (x = mean (treatmentData [['endOfSeasonDOY']] [!treatmentData [['densityFluctuation']]], na.rm = TRUE), 
              y = ifelse (PLOTTREE, 4400, 1800) + ifelse (t == 1, -100, 100), 
              pch = ifelse (t == 1, 19, 25), 
              col = '#666666', cex = 2, bg = 'white', lwd = 2)
      #if (h == 1.5) {
      #  legend (x = 125, y = ifelse (PLOTTREE, 4000, 1900), legend = c ('with','without'), 
      #          bg = 'transparent', box.lty = 0, cex = 1.8, lwd = 2, lty = 0,
      #          pch = c (24, 25), col = tColours [['colour']] [5])
      #}
    }
  } # end treatment loop
} # end sample height loop

# add legend 
#legend (x = 110, y = ifelse (PLOTTREE, 4000, 1900), legend = c ('',''), 
#        bg = 'transparent', box.lty = 0, cex = 1.8,
#        lty = 1:2, col = tColours [['colour']] [c (1, 5)], lwd = 2)
#legend (x = 125, y = ifelse (PLOTTREE, 4000, 1900), legend = c ('control','chilled'), 
#        bg = 'transparent', box.lty = 0, cex = 1.8, lwd = 2, lty = 0,
#        pch = c (19, 23), col = tColours [['colour']] [c (1, 5)])
dev.off ()

# estimate treatment effect on start and end of growing season
#----------------------------------------------------------------------------------------
tempData <- growingSeasonDates %>% filter (treatment %in% c (1, 5)) %>%
  mutate (treeId       = factor (treeId),
          treatment    = factor (treatment, levels = c (5, 1)),
          sampleHeight = factor (sampleHeight))
mod1 <-lmer (formula = startOfGrowth ~ (1 | treeId) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod1)
mod2 <-lmer (formula = endOfGrowth ~ (1 | treeId) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod2)
mod3 <-lmer (formula = endOfSeasonDOY ~ (1 | treeId) + treatment:sampleHeight, 
             data = tempData, REML = TRUE)
summary (mod3); rm (tempData)

# How much growth (fraction) had occurred at the start of the experiment?
#----------------------------------------------------------------------------------------
tempData <- ringWidths %>% filter (treatment %in% c (1, 5)) %>% group_by (sampleHeight, treeId) %>%
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

# How much growth (fraction) had occurred after the experimental onset
#----------------------------------------------------------------------------------------
filter (tempData, sampleDate == as_date ('2018-06-19')) %>% 
  mutate (remainingGrowthFraction = 1 - f2018) %>%
  group_by (treatment, sampleHeight) %>%
  summarise (mean = mean (remainingGrowthFraction, na.rm = TRUE),
             se = se (remainingGrowthFraction))

# How much growth (absolute; in micrometer) had occured at the start of the experiment?
#----------------------------------------------------------------------------------------
# Across all height and treatments
filter (tempData, sampleDate == as_date ('2018-06-19')) %>% 
  select (Y2018) %>% 
  summarise (mean = mean (Y2018, na.rm = TRUE),
             se = se (Y2018))
# By height and treatment
filter (tempData, sampleDate == as_date ('2018-06-19')) %>%
  group_by (treatment, sampleHeight) %>%
  summarise (mean = mean (Y2018, na.rm = TRUE),
             se = se (Y2018))

# How much growth had occurred after the experimental onset
#----------------------------------------------------------------------------------------
tempData <- ringWidths %>% filter (treatment %in% c (1, 5)) %>% 
  group_by (sampleHeight, treeId) %>%
  mutate (endG = max (Y2018, na.rm = TRUE)) %>%
  filter (sampleDate == as_date ('2018-09-06')) %>%
  mutate (G2018 = endG - Y2018) %>% ungroup ()
tempData <- tempData %>% mutate (treatment = factor (treatment, levels = c (5, 1)),
                                 treeId = factor (treeId), 
                                 sampleHeight = factor (sampleHeight, levels = c (0.5, 1.5, 2.5, 4.0)))
mod7 <- lmer (formula = G2018 ~ (1 | treeId) + treatment:sampleHeight, 
              data = tempData, REML = TRUE)
summary (mod7)#; rm (tempData)

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

mod8 <- lmer (formula = remainingGrowthFraction ~ (1 | treeId) + treatment:sampleHeight, 
              data = tempData, REML = TRUE)
summary (mod8); rm (tempData)

# Was radial growth comparable in the preceeding seven years?
#----------------------------------------------------------------------------------------
tempData <- ringWidths %>% 
  filter (sampleDate %notin% c (as_date ('2018-01-01'), as_date ('2018-05-01')),
          treatment == 5) %>%
  select (1:14) %>% 
  pivot_longer (cols = 6:14, names_prefix = 'Y', names_to = 'year', values_to = 'RW') %>%
  mutate (treeId = factor (treeId),
          treatment = factor (treatment, levels = c (1,5)),
          sampleHeight= factor (sampleHeight, levels = c (0.5, 1.5, 2.5, 4.0)),
          year = factor (year))
mod9 <- lmer (formula = RW ~ (1 | treeId) + year + sampleHeight,# + treatment,
              data = tempData)
summary (mod9); rm (tempData)

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
  filter (treatment %in% c (1, 5), sampleDate %notin% c (as_date ('2018-01-01'), as_date ('2018-05-01'))) %>%
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
mod10 <- lmer (formula = RW ~ (1 | treeId) + year + treatment + sampleHeight,
              data = tempData, REML = TRUE)
summary (mod10); rm (tempData)

# Did groups grow differently in 2019?
#----------------------------------------------------------------------------------------
tempData <- ringWidths %>%
  filter (sampleDate == as_date ('2019-10-24'), treatment %in% c (1, 5)) %>%
  mutate (treeId       = factor (treeId),
          treatment    = factor (treatment, levels = c (5,4,1)),
          sampleHeight = factor (sampleHeight))
g <- ggplot (tempData) +
  geom_boxplot (aes (x = sampleHeight, y = RWI2019, fill = treatment),
                alpha = 0.7, colour = rep (tColours [['colour']] [c (1,4,5)], 4)) + 
  coord_flip () +
  labs (x = 'Sample Height (m)', y = 'Radial growth index') +
  scale_fill_manual (values = tColours [['colour']] [c (1,4,5)], labels = c ('Control','Compressed','Chilled'))
g
# Were there differences in the 2019 ring width (following year) among treatments?
mod11 <- lmer (formula = Y2019 ~ (1 | treeId) + treatment + treatment:sampleHeight,
              data = tempData, REML = TRUE)
summary (mod11); rm (tempData)

# clean up 
#----------------------------------------------------------------------------------------
rm (mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10)
#========================================================================================
