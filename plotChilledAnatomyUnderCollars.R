#========================================================================================
# Script to explore anatomical data for the 2018 chilled and control trees only at 
# Harvard Forest
#----------------------------------------------------------------------------------------

# Get the anatomical and ring width data
source ('processAnatomicalData.R')

# plot lumen area over ring profile in 2017, 2018, and 2019
layout (matrix (1:30, nrow = 6, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1, 1, 1.3))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)) {
  
  # loop over trees
  for (i in 1:5) {
    
    # Determine plot margins
    if (i == 1 & h != 0.5) {
      par (mar = c (2, 5, 1, 1))
    } else if (i == 1 & h == 0.5) {
      par (mar = c (5, 5, 1, 1))
    } else if (i != 1 & h != 0.5) {
      par (mar = c (2, 1, 1, 1))
    } else if (i != 1 & h == 0.5) {
      par (mar = c (5, 1, 1, 1))
    }
    
    # loop over sampling dates
    for (d in as_date (c ('2018.11.15','2019.10.24'))) {
      
      # Condition to extract relevant data
      con <- anatomicalData [['TREE']] == i & anatomicalData [['sampleHeight']] == h & 
        anatomicalData [['YEAR']] %in% 2017:2019 & anatomicalData [['sampleDate']] == d
      
      # Extract treatment
      t <- unique (anatomicalData [['PLOT']] [con]) 
      
      # loop over three years
      for (y in 2017:2019) {
        
        # plot 
        if (d == as_date ('2018.11.15') & y == 2017) {
          plot (x = anatomicalData [['RRADDISTR']] [con & anatomicalData [['YEAR']] == y], 
                y = anatomicalData [['LA']] [con & anatomicalData [['YEAR']] == y],
                xlab = ifelse (h == 0.5, 
                               expression (paste ('percent ring growth (%)', sep = '')),
                               ' '),
                ylab = ifelse (i == 1, 
                               expression (paste ('median lumen area (',mu,m^2,')', sep = '')), 
                               ' '), 
                las = 1, col = tColours [['colour']] [t], pch = 1, xlim = c (0, 300), ylim = c (0, 1000),
                xaxt = 'n', yaxt = ifelse (i == 1, 't', 'n'))
          axis (side = 1, at = seq (0, 300, by = 50), labels = c (0, rep (c (50, 100), 3)))
          abline (v = seq (0, 300, by = 100), col = '#66666666')
        } else {
          if (d == as_date ('2018.11.15') & y == 2019) next
          dpch = ifelse (d == as_date ('2019.10.24'), 19, 1) 
          points (x = anatomicalData [['RRADDISTR']] [con & anatomicalData [['YEAR']] == y] + 
                    ifelse (y == 2017, 0, ifelse (y == 2018, 100, 200)), 
                  y = anatomicalData [['LA']] [con & anatomicalData [['YEAR']] == y],
                  pch = dpch, col = tColours [['colour']] [t])
        }
      }
    }
  }
}
legend (x = 0, y = 1000, box.lty = 0, pch = c (1, 19), col = tColours [['colour']] [t], 
        legend = c ('2018-11-15 sample','2019-10-24 sample'), bg = 'transparent')


# plot lumen area over ring profile in 2017, 2018, and 2019
layout (matrix (1:30, nrow = 6, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1, 1, 1.3))
# loop over sampling heights for control trees
for (h in c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)) {
  
  # loop over trees
  for (i in 11:15) {
    
    # Determine plot margins
    if (i == 11 & h != 0.5) {
      par (mar = c (2, 5, 1, 1))
    } else if (i == 11 & h == 0.5) {
      par (mar = c (5, 5, 1, 1))
    } else if (i != 11 & h != 0.5) {
      par (mar = c (2, 1, 1, 1))
    } else if (i != 11 & h == 0.5) {
      par (mar = c (5, 1, 1, 1))
    }
    
    # loop over sampling dates
    for (d in as_date (c ('2018.11.15','2019.10.24'))) {
      
      # Condition to extract relevant data
      con <- anatomicalData [['TREE']] == i & anatomicalData [['sampleHeight']] == h & 
        anatomicalData [['YEAR']] %in% 2017:2019 & anatomicalData [['sampleDate']] == d
      
      # Extract treatment
      t <- unique (anatomicalData [['PLOT']] [con]) 
      
      # loop over three years
      for (y in 2017:2019) {
        
        # plot 
        if (d == as_date ('2018.11.15') & y == 2017) {
          plot (x = anatomicalData [['RRADDISTR']] [con & anatomicalData [['YEAR']] == y], 
                y = anatomicalData [['LA']] [con & anatomicalData [['YEAR']] == y],
                xlab = ifelse (h == 0.5, 
                               expression (paste ('percent ring growth (%)', sep = '')),
                               ' '),
                ylab = ifelse (i == 11, 
                               expression (paste ('median lumen area (',mu,m^2,')', sep = '')), 
                               ' '), 
                las = 1, col = tColours [['colour']] [t], pch = 1, xlim = c (0, 300), ylim = c (0, 1000),
                xaxt = 'n', yaxt = ifelse (i == 11, 't', 'n'))
          axis (side = 1, at = seq (0, 300, by = 50), labels = c (0, rep (c (50, 100), 3)))
          abline (v = seq (0, 300, by = 100), col = '#66666666')
        } else {
          if (d == as_date ('2018.11.15') & y == 2019) next
          dpch = ifelse (d == as_date ('2019.10.24'), 19, 1) 
          points (x = anatomicalData [['RRADDISTR']] [con & anatomicalData [['YEAR']] == y] + 
                    ifelse (y == 2017, 0, ifelse (y == 2018, 100, 200)), 
                  y = anatomicalData [['LA']] [con & anatomicalData [['YEAR']] == y],
                  pch = dpch, col = tColours [['colour']] [t])
        }
      }
    }
  }
}
legend (x = 0, y = 1000, box.lty = 0, pch = c (1, 19), col = tColours [['colour']] [t], 
        legend = c ('2018-11-15 sample','2019-10-24 sample'), bg = 'transparent')


# plot cell number over ring profile in 2017, 2018, and 2019
layout (matrix (1:30, nrow = 6, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1, 1, 1.3))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)) {
  
  # loop over trees
  for (i in 1:5) {
    
    # Determine plot margins
    if (i == 1 & h != 0.5) {
      par (mar = c (2, 5, 1, 1))
    } else if (i == 1 & h == 0.5) {
      par (mar = c (5, 5, 1, 1))
    } else if (i != 1 & h != 0.5) {
      par (mar = c (2, 1, 1, 1))
    } else if (i != 1 & h == 0.5) {
      par (mar = c (5, 1, 1, 1))
    }
    
    # loop over sampling dates
    for (d in as_date (c ('2018.11.15','2019.10.24'))) {
      
      # Condition to extract relevant data
      con <- anatomicalData [['TREE']] == i & anatomicalData [['sampleHeight']] == h & 
        anatomicalData [['YEAR']] %in% 2017:2019 & anatomicalData [['sampleDate']] == d
      
      # Extract treatment
      t <- unique (anatomicalData [['PLOT']] [con]) 
      
      # loop over three years
      for (y in 2017:2019) {
        
        # plot 
        if (d == as_date ('2018.11.15') & y == 2017) {
          plot (x = anatomicalData [['RRADDISTR']] [con & anatomicalData [['YEAR']] == y], 
                y = anatomicalData [['cumNCells']] [con & anatomicalData [['YEAR']] == y],
                xlab = ifelse (h == 0.5, 
                               expression (paste ('percent ring growth (%)', sep = '')),
                               ' '),
                ylab = ifelse (i == 1, 
                               'number of cells', 
                               ' '), 
                las = 1, col = tColours [['colour']] [t], pch = 1, xlim = c (0, 300), ylim = c (0, 90),
                xaxt = 'n', yaxt = ifelse (i == 1, 't', 'n'))
          axis (side = 1, at = seq (0, 300, by = 50), labels = c (0, rep (c (50, 100), 3)))
          abline (v = seq (0, 300, by = 100), col = '#66666666')
        } else {
          if (d == as_date ('2018.11.15') & y == 2019) next
          dpch = ifelse (d == as_date ('2019.10.24'), 19, 1) 
          points (x = anatomicalData [['RRADDISTR']] [con & anatomicalData [['YEAR']] == y] + 
                    ifelse (y == 2017, 0, ifelse (y == 2018, 100, 200)), 
                  y = anatomicalData [['cumNCells']] [con & anatomicalData [['YEAR']] == y],
                  pch = dpch, col = tColours [['colour']] [t])
        }
      }
    }
  }
}
legend (x = 0, y = 90, box.lty = 0, pch = c (1, 19), col = tColours [['colour']] [t], 
        legend = c ('2018-11-15 sample','2019-10-24 sample'), bg = 'transparent')

# plot cell number over ring profile in 2017, 2018, and 2019
layout (matrix (1:30, nrow = 6, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1, 1, 1.3))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)) {
  
  # loop over trees
  for (i in 11:15) {
    
    # Determine plot margins
    if (i == 11 & h != 0.5) {
      par (mar = c (2, 5, 1, 1))
    } else if (i == 11 & h == 0.5) {
      par (mar = c (5, 5, 1, 1))
    } else if (i != 11 & h != 0.5) {
      par (mar = c (2, 1, 1, 1))
    } else if (i != 11 & h == 0.5) {
      par (mar = c (5, 1, 1, 1))
    }
    
    # loop over sampling dates
    for (d in as_date (c ('2018.11.15','2019.10.24'))) {
      
      # Condition to extract relevant data
      con <- anatomicalData [['TREE']] == i & anatomicalData [['sampleHeight']] == h & 
        anatomicalData [['YEAR']] %in% 2017:2019 & anatomicalData [['sampleDate']] == d
      
      # Extract treatment
      t <- unique (anatomicalData [['PLOT']] [con]) 
      
      # loop over three years
      for (y in 2017:2019) {
        
        # plot 
        if (d == as_date ('2018.11.15') & y == 2017) {
          plot (x = anatomicalData [['RRADDISTR']] [con & anatomicalData [['YEAR']] == y], 
                y = anatomicalData [['cumNCells']] [con & anatomicalData [['YEAR']] == y],
                xlab = ifelse (h == 0.5, 
                               expression (paste ('percent ring growth (%)', sep = '')),
                               ' '),
                ylab = ifelse (i == 11, 
                               'number of cells', 
                               ' '), 
                las = 1, col = tColours [['colour']] [t], pch = 1, xlim = c (0, 300), ylim = c (0, 90),
                xaxt = 'n', yaxt = ifelse (i == 11, 't', 'n'))
          axis (side = 1, at = seq (0, 300, by = 50), labels = c (0, rep (c (50, 100), 3)))
          abline (v = seq (0, 300, by = 100), col = '#66666666')
        } else {
          if (d == as_date ('2018.11.15') & y == 2019) next
          dpch = ifelse (d == as_date ('2019.10.24'), 19, 1) 
          points (x = anatomicalData [['RRADDISTR']] [con & anatomicalData [['YEAR']] == y] + 
                    ifelse (y == 2017, 0, ifelse (y == 2018, 100, 200)), 
                  y = anatomicalData [['cumNCells']] [con & anatomicalData [['YEAR']] == y],
                  pch = dpch, col = tColours [['colour']] [t])
        }
      }
    }
  }
}
legend (x = 0, y = 90, box.lty = 0, pch = c (1, 19), col = tColours [['colour']] [t], 
        legend = c ('2018-11-15 sample','2019-10-24 sample'), bg = 'transparent')


# plot cumulative cell-wall area over ring profile in 2017, 2018, and 2019
layout (matrix (1:30, nrow = 6, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1, 1, 1.3))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)) {
  
  # loop over trees
  for (i in 1:5) {
    
    # Determine plot margins
    if (i == 1 & h != 0.5) {
      par (mar = c (2, 5, 1, 1))
    } else if (i == 1 & h == 0.5) {
      par (mar = c (5, 5, 1, 1))
    } else if (i != 1 & h != 0.5) {
      par (mar = c (2, 1, 1, 1))
    } else if (i != 1 & h == 0.5) {
      par (mar = c (5, 1, 1, 1))
    }
    
    # loop over sampling dates
    for (d in as_date (c ('2018.11.15','2019.10.24'))) {
      
      # Condition to extract relevant data
      con <- anatomicalData [['TREE']] == i & anatomicalData [['sampleHeight']] == h & 
        anatomicalData [['YEAR']] %in% 2017:2019 & anatomicalData [['sampleDate']] == d
      
      # Extract treatment
      t <- unique (anatomicalData [['PLOT']] [con]) 
      
      # loop over three years
      for (y in 2017:2019) {
        
        # plot 
        if (d == as_date ('2018.11.15') & y == 2017) {
          plot (x = anatomicalData [['RRADDISTR']] [con & anatomicalData [['YEAR']] == y], 
                y = anatomicalData [['cumCWA']] [con & anatomicalData [['YEAR']] == y],
                xlab = ifelse (h == 0.5, 
                               expression (paste ('percent ring growth (%)', sep = '')),
                               ' '),
                ylab = ifelse (i == 1, 
                               expression (paste ('cumCWA (',mu,m^2,')', sep = '')), 
                               ' '), 
                las = 1, col = tColours [['colour']] [t], pch = 1, 
                xlim = c (0, 300), ylim = c (0, 15000),
                xaxt = 'n', yaxt = ifelse (i == 1, 't', 'n'))
          axis (side = 1, at = seq (0, 300, by = 50), labels = c (0, rep (c (50, 100), 3)))
          abline (v = seq (0, 300, by = 100), col = '#66666666')
        } else {
          if (d == as_date ('2018.11.15') & y == 2019) next
          dpch = ifelse (d == as_date ('2019.10.24'), 19, 1) 
          points (x = anatomicalData [['RRADDISTR']] [con & anatomicalData [['YEAR']] == y] + 
                    ifelse (y == 2017, 0, ifelse (y == 2018, 100, 200)), 
                  y = anatomicalData [['cumCWA']] [con & anatomicalData [['YEAR']] == y],
                  pch = dpch, col = tColours [['colour']] [t])
        }
      }
    }
  }
}
legend (x = 0, y = 15000, box.lty = 0, pch = c (1, 19), col = tColours [['colour']] [t], 
        legend = c ('2018-11-15 sample','2019-10-24 sample'), bg = 'transparent')


# plot cumulative cell-wall area over ring profile in 2017, 2018, and 2019
layout (matrix (1:30, nrow = 6, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1, 1, 1.3))
# loop over sampling heights for control trees
for (h in c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)) {
  
  # loop over trees
  for (i in 11:15) {
    
    # Determine plot margins
    if (i == 11 & h != 0.5) {
      par (mar = c (2, 5, 1, 1))
    } else if (i == 11 & h == 0.5) {
      par (mar = c (5, 5, 1, 1))
    } else if (i != 11 & h != 0.5) {
      par (mar = c (2, 1, 1, 1))
    } else if (i != 11 & h == 0.5) {
      par (mar = c (5, 1, 1, 1))
    }
    
    # loop over sampling dates
    for (d in as_date (c ('2018.11.15','2019.10.24'))) {
      
      # Condition to extract relevant data
      con <- anatomicalData [['TREE']] == i & anatomicalData [['sampleHeight']] == h & 
        anatomicalData [['YEAR']] %in% 2017:2019 & anatomicalData [['sampleDate']] == d
      
      # Extract treatment
      t <- unique (anatomicalData [['PLOT']] [con]) 
      
      # loop over three years
      for (y in 2017:2019) {
        
        # plot 
        if (d == as_date ('2018.11.15') & y == 2017) {
          plot (x = anatomicalData [['RRADDISTR']] [con & anatomicalData [['YEAR']] == y], 
                y = anatomicalData [['cumCWA']] [con & anatomicalData [['YEAR']] == y],
                xlab = ifelse (h == 0.5, 
                               expression (paste ('percent ring growth (%)', sep = '')),
                               ' '),
                ylab = ifelse (i == 11, 
                               expression (paste ('cumCWA (',mu,m^2,')', sep = '')), 
                               ' '), 
                las = 1, col = tColours [['colour']] [t], pch = 1, 
                xlim = c (0, 300), ylim = c (0, 15000),
                xaxt = 'n', yaxt = ifelse (i == 11, 't', 'n'))
          axis (side = 1, at = seq (0, 300, by = 50), labels = c (0, rep (c (50, 100), 3)))
          abline (v = seq (0, 300, by = 100), col = '#66666666')
        } else {
          if (d == as_date ('2018.11.15') & y == 2019) next
          dpch = ifelse (d == as_date ('2019.10.24'), 19, 1) 
          points (x = anatomicalData [['RRADDISTR']] [con & anatomicalData [['YEAR']] == y] + 
                    ifelse (y == 2017, 0, ifelse (y == 2018, 100, 200)), 
                  y = anatomicalData [['cumCWA']] [con & anatomicalData [['YEAR']] == y],
                  pch = dpch, col = tColours [['colour']] [t])
        }
      }
    }
  }
}
legend (x = 0, y = 15000, box.lty = 0, pch = c (1, 19), col = tColours [['colour']] [t], 
        legend = c ('2018-11-15 sample','2019-10-24 sample'), bg = 'transparent')
