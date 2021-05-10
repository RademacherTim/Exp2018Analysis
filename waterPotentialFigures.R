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
      y = phi [['phi.branch']],
      col = addOpacity (tColours [['colour']] [phi [['treatment']]], 0.7),
      xlab = 'date', ylab = expression (paste (phi[branch],'(MPa)', sep = '')),
      las = 1, pch = 19, 
      ylim = c (0, -1))

# divide data into weeks
#----------------------------------------------------------------------------------------
phi <- phi %>% mutate (week = cut (date, '1 week')) %>% mutate (week = as_date (week))

# make boxplots by date
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2018ChillingWaterPotential.png', width = 900, height = 400)
par (mar = c (5, 6, 1, 1))
boxplot (phi.branch ~ treatment, data = phi [phi [['week']] < as_date ('2018-10-01') &
                                             phi [['treatment']] != 4, ], 
        # horizontal = TRUE, 
         col = addOpacity (tColours [['colour']] [c (1, 5)], 0.5),
         border = tColours [['colour']] [c (1, 5)],
         ylim = c (-1,  0), xaxt = 'n', xlim = c (0, 6), frame = FALSE,
         ylab = expression (paste (phi,'(MPa)', sep = '')), xlab = '', 
         out.pch = 19, out.col = addOpacity (tColours [['colour']] [c (1, 4, 5)], 0.7))
boxplot (phi.needles ~ treatment, data = phi [phi [['week']] < as_date ('2018-10-01') &
                                              phi [['treatment']] != 4, ], 
         #horizontal = TRUE, 
         add = TRUE, at = 4:5,
         col = addOpacity (tColours [['colour']] [c (1, 5)], 0.5),
         border = tColours [['colour']] [c (1, 5)],
         ylim = c (0, -1), axes = FALSE, frame = FALSE)
axis (side = 1, at = 1:2, labels = c ('control','chilled'))
axis (side = 1, at = 4:5, labels = c ('control','chilled'))
abline (v = 3, col = '#666666')
text (labels = c ('branches', 'needles'), y = 0, x = c (0.3, 3.5), col = '#666666', 
      cex = 1.5)
dev.off ()
#========================================================================================