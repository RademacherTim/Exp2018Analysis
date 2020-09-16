#========================================================================================
# Script to plot respiration data for the 2018 plhoem chilling and compression experiment
# at Harvard Forest.
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lubridate')

# source ploting functions for treatment colours 
#----------------------------------------------------------------------------------------
source ('./plotingFunctions.R')

# read processed respiration data
#----------------------------------------------------------------------------------------
source ('./readProcessedRespData.R')

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
      col = addOpacity (tColours [['colour']] [respDataExp2018 [['treatment']] [respDataExp2018 [['treatment']] == 1]], 0.6))

# add observational trees to the plot to get a larger temperature variation and sample size 
#----------------------------------------------------------------------------------------
points (x = respDataObs [['airt.c']] [respDataObs [['treatment']] == 1],
        y = respDataObs [['flux.raw']] [respDataObs [['treatment']] == 1],
        pch = 19, 
        col = addOpacity (tColours [['colour']] [2], 0.6))

# legend 
#----------------------------------------------------------------------------------------
legend (x = 25, y = 4.5, box.lty = 0, col = addOpacity (tColours [['colour']] [1:2], 0.6),
        pch = 19, legend = c ('observational','experimental'))
#========================================================================================

