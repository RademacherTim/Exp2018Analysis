#========================================================================================
# script to plot the water potential data for the 2018 chilling experiment on white pine 
# at Harvard Forest.
#----------------------------------------------------------------------------------------

# source water potential data
#----------------------------------------------------------------------------------------
source ('./readWaterPotential.R')
source ('./plotingFunctions.R')

# plot needle and branch water potential
#----------------------------------------------------------------------------------------
par (mfrow = c (1, 1))
plot (x = phi [['date']],
      y = phi [['phi.branch']] / 10,
      col = addOpacity (tColours [['colour']] [phi [['treatment']]], 0.7),
      xlab = 'date', ylab = expression (paste (phi[branch],'(MPa)', sep = '')),
      las = 1, pch = 19, 
      ylim = c (0, -1))

# divide data into weeks
#----------------------------------------------------------------------------------------
phi <- phi %>% mutate (week = cut (date, '1 week')) %>% mutate (week = as_date (week))

# make boxplots by date
#----------------------------------------------------------------------------------------
par (mar = c (5, 6, 1, 1))
boxplot (phi.branch / 10.0 ~ treatment, data = phi [phi [['week']] < as_date ('2018-10-01'), ], 
         horizontal = TRUE, 
         col = addOpacity (tColours [['colour']] [c (1, 4, 5)], 0.7),
         ylim = c (0, -1), yaxt = 'n', xlim = c (0, 8), axes = FALSE,
         xlab = expression (paste (phi,'(MPa)', sep = '')), ylab = '')
boxplot (phi.needles / 10.0 ~ treatment, data = phi [phi [['week']] < as_date ('2018-10-01'), ], 
         horizontal = TRUE, add = TRUE, at = 5:7,
         col = addOpacity (tColours [['colour']] [c (1, 4, 5)], 0.7),
         ylim = c (0, -1), yaxt = 'n')
axis (side = 2, at = c (1:3,5:7), labels = rep (c ('control','compressed','chilled'), 2),
      las = 1)
abline (h = 4, col = '#666666')
text (labels = c ('branches', 'needles'), x = -0.9, y = c (0.5, 4.5), col = '#666666')
#========================================================================================