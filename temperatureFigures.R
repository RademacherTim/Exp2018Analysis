#========================================================================================
# Script to plot the temperature data from the project chill shed and the phloem of all 
# trees at 1.0 and 2.0 m of the 2018 chilling experiment on white pine at Harvard Forest.
#----------------------------------------------------------------------------------------

# source temperature data
#----------------------------------------------------------------------------------------
source ('./readTemperatureData.R')
source ('./plotingFunctions.R')

# plot temperature of chilled and control trees at 1 and 2m 
#----------------------------------------------------------------------------------------
layout (matrix (1:3), heights = c (1, 1, 1.2))
par (mar = c (1, 8, 1, 1))
plot (x = tempData [['datetime']], 
      y = tempData [['t.c1.2p0m']], 
      typ = 'l', las = 1,
      xlim = c (as_datetime ('2018-06-07'), as_datetime ('2018-09-01')),
      ylim = c (-5, 38),
      xaxt = 'n', xlab = '', ylab = '2.0 m',
      col = tColours [['colour']] [5])

# add desired chilling zone
rect (xleft = as_datetime ('2018-05-01'), xright = as_datetime ('2018-09-20'),
      ybottom = 0, ytop = 5, col = addOpacity ('#666666', 0.3), lty = 0)

# add critical dates
res <- criticalDates (group = 5, asDate = FALSE)

# add more chilled trees
lines (x = tempData [['datetime']], 
       y = tempData [['t.c2.2p0m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['datetime']], 
       y = tempData [['t.c3.2p0m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['datetime']], 
       y = tempData [['t.c4.2p0m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['datetime']], 
       y = tempData [['t.c5.2p0m']],
       col = tColours [['colour']] [5])

# add air temperature
lines (x = tempData [['datetime']], 
       y = tempData [['t.air.2p0m']],
       col = tColours [['colour']] [2])

# add temperatures at 1.5 m
par (mar = c (1, 8, 1, 1))
plot (x = tempData [['datetime']], 
      y = tempData [['t.c1.1p5m']], 
      typ = 'l', las = 1,
      xlim = c (as_datetime ('2018-06-07'), as_datetime ('2018-09-01')),
      ylim = c (-5, 38),
      xaxt = 'n', xlab = '', ylab = '1.5 m',
      col = tColours [['colour']] [5])
mtext (text = expression (paste ('phloem temperature (',degree,'C)', sep = '')), 
       side = 2, line = 5, cex = 1.8)

# add desired chilling zone
rect (xleft = as_datetime ('2018-05-01'), xright = as_datetime ('2018-09-20'),
      ybottom = 0, ytop = 5, col = addOpacity ('#666666', 0.3), lty = 0)

# add critical dates
res <- criticalDates (group = 5, asDate = FALSE)

# add more chilled trees
lines (x = tempData [['datetime']], 
       y = tempData [['t.c2.1p5m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['datetime']], 
       y = tempData [['t.c3.1p5m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['datetime']], 
       y = tempData [['t.c4.1p5m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['datetime']], 
       y = tempData [['t.c5.1p5m']],
       col = tColours [['colour']] [5])

# add existing measurements from compressed and control trees
lines (x = tempData [['datetime']], 
       y = tempData [['t.c6.1p5m']],
       col = tColours [['colour']] [4])
lines (x = tempData [['datetime']], 
       y = tempData [['t.c7.1p5m']],
       col = tColours [['colour']] [4])
lines (x = tempData [['datetime']], 
       y = tempData [['t.c8.1p5m']],
       col = tColours [['colour']] [4])
lines (x = tempData [['datetime']], 
       y = tempData [['t.c10.1p5m']],
       col = tColours [['colour']] [1])

# add air temperature
lines (x = tempData [['datetime']], 
       y = tempData [['t.air.2p0m']],
       col = tColours [['colour']] [2])


# add temperatures at 1.0 m
par (mar = c (3, 8, 1, 1))
plot (x = tempData [['datetime']], 
      y = tempData [['t.c1.1p0m']], 
      typ = 'l', las = 1,
      xlim = c (as_datetime ('2018-06-07'), as_datetime ('2018-09-01')),
      ylim = c (-5, 38),
      xaxt = 'n', xlab = '', ylab = '1.0 m',
      col = tColours [['colour']] [5])

# add x axis
axis (side = 1, at = seq (as_datetime ('2018-07-01'), as_datetime ('2018-08-01'), 
                          length.out = 2), label = c ('July','August'))

# add desired chilling zone
rect (xleft = as_datetime ('2018-05-01'), xright = as_datetime ('2018-09-20'),
      ybottom = 0, ytop = 5, col = addOpacity ('#666666', 0.3), lty = 0)

# add critical dates
res <- criticalDates (group = 5, asDate = FALSE)

# add more chilled trees
lines (x = tempData [['datetime']], 
       y = tempData [['t.c2.1p0m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['datetime']], 
       y = tempData [['t.c3.1p0m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['datetime']], 
       y = tempData [['t.c4.1p0m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['datetime']], 
       y = tempData [['t.c5.1p0m']],
       col = tColours [['colour']] [5])

# add air temperature
lines (x = tempData [['datetime']], 
       y = tempData [['t.air.2p0m']],
       col = tColours [['colour']] [2])

# add legend 
legend (x = as_datetime ('2018-06-07'), y = 38, box.lty = 0, 
        col = tColours [['colour']] [c (2, 1, 4, 5)], 
        legend = c ('air temperature','control trees','compressed trees','chilled trees'),
        lwd = 1, cex = 0.6, bg = 'transparent')
