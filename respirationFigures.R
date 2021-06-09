#========================================================================================
# Script to plot respiration data for the 2018 plhoem chilling and compression experiment
# at Harvard Forest.
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('as_date')) library ('lubridate')

# source ploting functions for treatment colours 
#----------------------------------------------------------------------------------------
if (!exists ('tColours')) source ('./plotingFunctions.R')

# read processed respiration data
#----------------------------------------------------------------------------------------
if (!exists ('respData2018')) source ('./readProcessedRespData.R')

# source function to convert units from RespChamberFlux package
#----------------------------------------------------------------------------------------
source ('../stemCO2Efflux/RespChamberProc/R/convertUnits.R')

# convert flux from micro mol per square meter per second to g per square meter per day 
#----------------------------------------------------------------------------------------
respDataExp2018 [['flux.raw.g']] <- convert_mumolPers_to_gPerday (respDataExp2018 [['flux.raw']])

# plot respiration in control, compressed and chilled trees over time
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
layout (matrix (1:15, nrow = 3, byrow = TRUE))
for (t in 1:15) {
  con <- respDataExp2018 [['tree']] == t & respDataExp2018 [['chamber']] == 1
  plot (x = as_datetime (respDataExp2018 [['datetime']] [con]),
        y = respDataExp2018 [['flux.raw']] [con], typ = 'l', las = 1,
        xlim = c (as_datetime ('2018-05-01'), as_datetime ('2019-08-01')),
        ylim = c (0, 5),
        xlab = 'date', col = tColours [['colour']] [respDataExp2018 [['treatment']] [con]], 
        ylab = expression (paste ('respiration rate (',mu, mol,' ', m^-2,' ', s^-1,')', sep = ' ')))
  for (c in 2:3) {
    con <- respDataExp2018 [['tree']] == t & respDataExp2018 [['chamber']] == c
    lines (x = as_datetime (respDataExp2018 [['datetime']] [con]),
           y = respDataExp2018 [['flux.raw']] [con], 
           col = tColours [['colour']] [respDataExp2018 [['treatment']] [con]],
           lty = c)
  }
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  res <- abline (v = as_date ('2018-06-25'), col = '#99999999', lty = 2) # start date
  
  # Add tree panel descriptor
  #--------------------------------------------------------------------------------------
  text (x = as_datetime ('2018-05-04'), y = 3.8, labels = t, cex = 2)
}

# plot respiration in control, compressed and chilled trees for 2018 only
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
layout (matrix (1:15, nrow = 3, byrow = TRUE))
for (t in 1:15) {
  con <- respDataExp2018 [['tree']] == t & 
    respDataExp2018 [['chamber']]   == 1 & 
    respDataExp2018 [['datetime']]  < as_datetime ("2019-01-01")
  plot (x = as_datetime (respDataExp2018 [['datetime']] [con]),
        y = respDataExp2018 [['flux.raw']] [con], typ = 'l', las = 1,
        xlab = 'date',
        ylab = expression (paste ('respiration rate (',mu, mol,' ', m^-2,' ', s^-1,')', sep = ' ')),
        col = tColours [['colour']] [respDataExp2018 [['treatment']] [con]], 
        xlim = c (as_datetime ('2018-05-01'), as_datetime ('2018-11-10')),
        ylim = c (0, 5))
  for (c in 2:3) {
    con <- respDataExp2018 [['tree']] == t & 
      respDataExp2018 [['chamber']]   == c & 
      respDataExp2018 [['datetime']]  < as_datetime ("2019-01-01")
    lines (x = as_datetime (respDataExp2018 [['datetime']] [con]),
           y = respDataExp2018 [['flux.raw']] [con], 
           col = tColours [['colour']] [respDataExp2018 [['treatment']] [con]],
           lty = c)
  }
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates (group = unique (respDataExp2018 [['treatment']] [con]), asDate = FALSE)
  
  # Add tree panel descriptor
  #--------------------------------------------------------------------------------------
  text (x = as_datetime ('2018-05-10'), y = 4.8, labels = t, cex = 2)
}
legend (x = as_datetime ('2018-07-01'), y = 5, box.lty = 0, lty = 1:3, col = '#91b9a4', 
        legend = c ('0.5 m','1.5 m','2.5 m'))

# plot respiration versus air temperature
#----------------------------------------------------------------------------------------
par (mfrow = c (1, 1))
plot (x = respDataExp2018 [['airt.c']],
      y = respDataExp2018 [['flux.raw']],
      xlab = expression (paste ('air temperature (', degree,'C)', sep = '')),
      ylab = expression (paste ('stem ',CO[2],' efflux (',mu, mol,' ', m^-2,' ', s^-1,')', sep = ' ')),
      pch = 19, 
      col = addOpacity (tColours [['colour']] [respDataExp2018 [['treatment']]], 0.6),
      las = 1)

# plot respiration versus air temperature for control only
#----------------------------------------------------------------------------------------
par (mfrow = c (1, 1))
plot (x = respDataExp2018 [['airt.c']] [respDataExp2018 [['treatment']] == 1],
      y = respDataExp2018 [['flux.raw']] [respDataExp2018 [['treatment']] == 1],
      xlab = expression (paste ('air temperature (', degree,'C)', sep = '')),
      ylab = expression (paste ('stem ',CO[2],' efflux (',mu, mol,' ', m^-2,' ', s^-1,')', sep = ' ')),
      xlim = c (0, 30), ylim = c (0, 4.5),
      pch = 19, las = 1, 
      col = addOpacity (tColours [['colour']] [2], 0.6))

# add observational trees to the plot to get a larger temperature variation and sample size 
#----------------------------------------------------------------------------------------
points (x = respDataObs [['airt.c']] [respDataObs [['treatment']] == 1],
        y = respDataObs [['flux.raw']] [respDataObs [['treatment']] == 1],
        pch = 19, 
        col = addOpacity (tColours [['colour']] [1], 0.6))

# legend 
#----------------------------------------------------------------------------------------
legend (x = 24, y = 4.5, box.lty = 0, col = addOpacity (tColours [['colour']] [1:2], 0.6),
        pch = 19, legend = c ('observational study','experimental study'))

# plot respiration versus soil moisture for control group only
#----------------------------------------------------------------------------------------
par (mfrow = c (1, 1))
plot (x = respDataExp2018 [['vwc2']] [respDataExp2018 [['treatment']] == 1],
      y = respDataExp2018 [['flux.raw']] [respDataExp2018 [['treatment']] == 1],
      xlab = 'soil moisture (%)',
      ylab = expression (paste ('stem ',CO[2],' efflux (',mu, mol,' ', m^-2,' ', s^-1,')', sep = ' ')),
      xlim = c (0, 0.6), ylim = c (0, 4.5),
      pch = 19, las = 1, 
      col = addOpacity (tColours [['colour']] [2], 0.6))
# add observational trees to the plot to get a larger temperature variation and sample size 
#----------------------------------------------------------------------------------------
points (x = respDataObs [['vwc2']] [respDataObs [['treatment']] == 1],
        y = respDataObs [['flux.raw']] [respDataObs [['treatment']] == 1],
        pch = 19, 
        col = addOpacity (tColours [['colour']] [1], 0.6))

# legend 
#----------------------------------------------------------------------------------------
legend (x = 0, y = 4.5, box.lty = 0, col = addOpacity (tColours [['colour']] [1:2], 0.6),
        pch = 19, legend = c ('observational study','experimental study'))

# wrangle respiration data to get group means and standard errors
#----------------------------------------------------------------------------------------
summaryRespData <- respDataExp2018 %>% 
  mutate (date = as.POSIXct (session, format = '%Y%m%d_%H%M')) %>%
  mutate (date = as_date (date)) %>%
  group_by (date, treatment, chamber) %>%
  summarise (meanRawResp = mean (flux.raw, na.rm = TRUE),
             sdRawResp = sd (flux.raw, na.rm = TRUE),
             seRawResp = se (flux.raw),
             .groups = 'keep') 

# plot respiration in control, compressed and chilled treatments for 2018 only
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
layout (matrix (1:3, nrow = 1, byrow = TRUE))
for (t in c (1, 4, 5)) {
  con <- summaryRespData [['treatment']] == t & 
    summaryRespData [['chamber']] == 1 & 
    summaryRespData [['date']] < as_date ("2019-01-01")
  plot (x = summaryRespData [['date']] [con],
        y = summaryRespData [['meanRawResp']] [con], typ = 'l', las = 1,
        xlab = 'date',
        ylab = expression (paste ('respiration rate (',mu, mol,' ', m^-2,' ', s^-1,')', sep = ' ')),
        col = 'white', 
        xlim = c (as_date ('2018-05-01'), as_date ('2018-11-10')),
        ylim = c (0, 3.5))
  for (c in 1:3) {
    con <- summaryRespData [['treatment']] == t & 
      summaryRespData [['chamber']] == c & 
      summaryRespData [['date']]  < as_date ("2019-01-01")
    lines (x = summaryRespData [['date']] [con],
           y = summaryRespData [['meanRawResp']] [con], 
           col = tColours [['colour']] [t],
           lty = ifelse (c == 1, 3, ifelse (c == 2, 2, 1)))
    # add standard error
    polygon (x = c (summaryRespData [['date']] [con], rev (summaryRespData [['date']] [con])),
             y = c (summaryRespData [['meanRawResp']] [con] + summaryRespData [['seRawResp']] [con],
                    rev (summaryRespData [['meanRawResp']] [con] - summaryRespData [['seRawResp']] [con])),
             col = addOpacity (tColours [['colour']] [t], 0.3), lty = 0)
  }
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates (group = t, asDate = TRUE)
  
  # Add tree panel descriptor
  #--------------------------------------------------------------------------------------
  if (t == 1) {
    text <- 'control'
  } else if (t == 4) {
    text <- 'compressed'
  } else if (t == 5) {
    text <- 'chilled'
  }
  text (x = as_date ('2018-09-15'), y = 3.3, labels = text, cex = 2)
}
legend (x = as_date ('2018-04-25'), y = 3.5, box.lty = 0, lty = 1:3, col = tColours [['colour']] [5], 
        legend = c ('2.5 m','1.5 m','0.5 m'), bg = 'transparent')


# plot respiration in control and chilled trees for the three different sampling heights
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2018ChillingCO2EffluxRates.png', width = 400, height = 600)
layout (matrix (1:3, nrow = 3, byrow = TRUE), heights = c (1, 1, 1.2))
for (h in 3:1) {
  
  # determine margins
  if (h == 1) {
    par (mar = c (5, 5, 1, 1))
  } else {
    par (mar = c (1, 5, 1, 1))
  }
  
  # plot control trees
  con <- summaryRespData [['treatment']] == 1 & 
    summaryRespData [['chamber']] == h & 
    summaryRespData [['date']] < as_date ("2019-01-01")
  plot (x = summaryRespData [['date']] [con],
        y = summaryRespData [['meanRawResp']] [con], typ = 'l', las = 1,
        xlab = ifelse (h == 1, '', 'Date'),
        ylab = expression (paste (CO[2],' efflux rate (',mu, mol,' ', m^-2,' ', s^-1,')', sep = ' ')),
        col = tColours [['colour']] [1], 
        xlim = c (as_date ('2018-05-01'), as_date ('2018-11-10')),
        ylim = c (0, 3.3), axes = FALSE)
  
  # plot control trees mean and standard error
  polygon (x = c (summaryRespData [['date']] [con], rev (summaryRespData [['date']] [con])),
           y = c (summaryRespData [['meanRawResp']] [con] + summaryRespData [['seRawResp']] [con],
                  rev (summaryRespData [['meanRawResp']] [con] - summaryRespData [['seRawResp']] [con])),
           col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
  lines (x = summaryRespData [['date']] [con],
         y = summaryRespData [['meanRawResp']] [con], 
         col = tColours [['colour']] [1], lty = 1, lwd = 2)
  
  # plot chilled trees
  con <- summaryRespData [['treatment']] == 5 & 
    summaryRespData [['chamber']] == h & 
    summaryRespData [['date']] < as_date ("2019-01-01")
  polygon (x = c (summaryRespData [['date']] [con], rev (summaryRespData [['date']] [con])),
           y = c (summaryRespData [['meanRawResp']] [con] + summaryRespData [['seRawResp']] [con],
                  rev (summaryRespData [['meanRawResp']] [con] - summaryRespData [['seRawResp']] [con])),
           col = addOpacity (tColours [['colour']] [5], 0.3), lty = 0)
  lines (x = summaryRespData [['date']] [con],
         y = summaryRespData [['meanRawResp']] [con], 
         col = tColours [['colour']] [5], lty = 2, lwd = 2)
  
  # add axes
  #--------------------------------------------------------------------------------------
  if (h == 1) {
    axis (side = 1, labels = c ('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov'),
          at = c (as_date ('2018-04-01'), as_date ('2018-05-01'), as_date ('2018-06-01'), 
                  as_date ('2018-07-01'), as_date ('2018-08-01'), as_date ('2018-09-01'), 
                  as_date ('2018-10-01'), as_date ('2018-11-01')))
    
    # add tick marks for sampling dates
    points (x = unique (summaryRespData [['date']]),
            y = rep (-0.15, length (unique (summaryRespData [['date']]))),
            pch = 3, lwd = 3, col = '#e37222', cex = 1.5)
  } else {
    axis (side = 1, labels = rep ('', 8),
          at = c (as_date ('2018-04-01'), as_date ('2018-05-01'), as_date ('2018-06-01'), 
                  as_date ('2018-07-01'), as_date ('2018-08-01'), as_date ('2018-09-01'), 
                  as_date ('2018-10-01'), as_date ('2018-11-01')))
  }
  axis (side = 2, las = 1, at = seq (0, 3.2, 0.8))
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates (group = t, asDate = TRUE)
  
  # add legend
  #--------------------------------------------------------------------------------------
  if (h == 1) {
    legend (x = as_date ('2018-05-01'), y = 3.2, box.lty = 0, bg = 'transparent', 
            legend = c ('control', 'chilled'), lty = 1:2, lwd = 2, 
            col = tColours [['colour']] [c (1, 5)])
  }
}
dev.off ()
#========================================================================================

