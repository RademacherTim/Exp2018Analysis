# plot effect of temperature on cell number
#----------------------------------------------------------------------------------------
par (mfrow = c (1, 1))
plot (x = rep (0, 6),
      y = c (34, 29, 32, 25, 26, 24),
      xlab = expression (paste ('Estimated mean temperature anomaly (',degree,'C)', sep = '')),
      ylab = 'Number of cells (n)', las = 1, xlim = c (-16, 2), ylim = c (10, 35), 
      col = addOpacity (tColours [['colour']] [1], 1), pch = 19)
points (x = c (-1, -14.6, -5, -14.7, -5, -3),
        y = c (32, 15, 20, 15, 19, 19), pch = 23, col = addOpacity (tColours [['colour']] [5], 1))
legend (x = -15, y = 35, box.lty = 0, bg = 'transparent', legend = c ('control','chilled'),
        pch = c (19, 23),
        col = tColours [['colour']] [c (1, 5)])

# plot cell number versus carbon supply
#----------------------------------------------------------------------------------------
par (mfrow = c (1, 1))
plot (x = rep (1, 6),
      y = c (34, 29, 32, 25, 26, 24),
      xlab = expression (paste ('Estimated carbon supply anomaly (fraction)', sep = '')),
      ylab = 'Number of cells (n)', las = 1, xlim = c (0, 2), ylim = c (10, 35), 
      col = addOpacity (tColours [['colour']] [1], 1), pch = 19)
points (x = c (0.2, 0.4, 0.45, 0.9, 1.5, 1),
        y = c (32, 15, 20, 15, 19, 19), pch = 23, col = addOpacity (tColours [['colour']] [5], 1))
legend (x = 1.5, y = 35, box.lty = 0, bg = 'transparent', legend = c ('control','chilled'),
        pch = c (19, 23),
        col = tColours [['colour']] [c (1, 5)])

# plot temperature response of cell-wall thickness
#----------------------------------------------------------------------------------------
