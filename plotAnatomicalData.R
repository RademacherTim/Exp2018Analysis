#========================================================================================
# Script to explore anatomical data for the 2018 chilling and compression experiment at 
# Harvard Forest. All data is publicly available on the Harvard Forest Data Archive.
#
# Data repository url:
# Code repository url:
#
#----------------------------------------------------------------------------------------

# Get the anatomical and ring width data
#----------------------------------------------------------------------------------------
source ('processAnatomicalData.R')

# plot lumen area over ring profile in 2017, 2018, and 2019 from 2018-11-15 and 
# 2019-10-24 slide
png (filename = './fig/medianLumenAreaChilledTrees.png', width = 1250, height = 850)
layout (matrix (1:30, nrow = 6, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1, 1, 1.4))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)) {
  # loop over trees
  for (i in 1:5) {
  
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & 
      anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['sampleDate']] == as_date ('2018-11-15') &
      anatomicalData [['YEAR']] %in% 2017:2018
    
    # Extract treatment
    t <- unique (anatomicalData [['PLOT']] [con]) 
    
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
    
    # plot measurements
    plot (x = anatomicalData [['RRADDISTR']] [con] + 
            ifelse (anatomicalData [['YEAR']] [con] == 2017, 0, 
                    ifelse (anatomicalData [['YEAR']] [con] == 2018, 100, 200)), 
          y = anatomicalData [['LA']] [con],
          xlab = ifelse (h == 0.5, 'percentage ring width (%)',' '),
          ylab = ifelse (i == 1, 
                         expression (paste ('median lumen area (',mu,m^2,')', sep = '')), 
                         ' '), 
          las = 1, col = tColours [['colour']] [t], pch = 1, 
          xlim = c (0, 300), ylim = c (0, 2300),
          yaxt = ifelse (i == 1, 't', 'n'), typ = 'l')
    
    abline (v = c (100, 200), col = '#66666666')
    
    if (h %in% c (1.0, 2.0)) {
      # Condition to extract relevant data
      con <- anatomicalData [['TREE']] == i & 
        anatomicalData [['sampleHeight']] == h & 
        anatomicalData [['sampleDate']] == as_date ('2018-11-15') &
        anatomicalData [['YEAR']] %in% 2017:2018
      
      
      lines (x = anatomicalData [['RRADDISTR']] [con] + 
                  ifelse (anatomicalData [['YEAR']] [con] == 2017, 0, 
                          ifelse (anatomicalData [['YEAR']] [con] == 2018, 100, 200)), 
              y = anatomicalData [['LA']] [con],
              col = tColours [['colour']] [t], lty = 1)
    }      
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & 
      anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['sampleDate']] == as_date ('2019-10-24') &
      anatomicalData [['YEAR']] %in% 2017:2019
    
    
    lines (x = anatomicalData [['RRADDISTR']] [con] + 
              ifelse (anatomicalData [['YEAR']] [con] == 2017, 0, 
                      ifelse (anatomicalData [['YEAR']] [con] == 2018, 100, 200)), 
            y = anatomicalData [['LA']] [con],
            col = tColours [['colour']] [t], lty = 3)
  }
}
legend (x = 150, y = 2200, box.lty= 0, lty = c (1, 3), bg = 'transparent', 
        legend = c ('2018-11-15','2019-10-24'),
        col = tColours [['colour']] [t])
dev.off ()

png (filename = './fig/medianLumenAreaCompressedTrees.png', width = 1250, height = 850)
layout (matrix (1:30, nrow = 6, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1, 1, 1.4))
# loop over sampling heights for compressed trees
for (h in c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)) {
  # loop over trees
  for (i in 6:10) {
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & 
      anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['sampleDate']] == as_date ('2019-10-24') &
      anatomicalData [['YEAR']] %in% 2017:2018
    
    # Extract treatment
    t <- unique (anatomicalData [['PLOT']] [con]) 
    
    # Determine plot margins
    if (i == 6 & h != 0.5) {
      par (mar = c (2, 5, 1, 1))
    } else if (i == 6 & h == 0.5) {
      par (mar = c (5, 5, 1, 1))
    } else if (i != 6 & h != 0.5) {
      par (mar = c (2, 1, 1, 1))
    } else if (i != 6 & h == 0.5) {
      par (mar = c (5, 1, 1, 1))
    }
    
    # plot measurements
    plot (x = anatomicalData [['RRADDISTR']] [con] + 
            ifelse (anatomicalData [['YEAR']] [con] == 2017, 0, 
                    ifelse (anatomicalData [['YEAR']] [con] == 2018, 100, 200)), 
          y = anatomicalData [['LA']] [con],
          xlab = ifelse (h == 0.5, 'percentage ring width (%)',' '),
          ylab = ifelse (i == 6, 
                         expression (paste ('median lumen area (',mu,m^2,')', sep = '')), 
                         ' '), 
          las = 1, col = tColours [['colour']] [t], 
          xlim = c (0, 300), ylim = c (0, 2300),
          yaxt = ifelse (i == 6, 't', 'n'), typ = 'l', lty = 2)
    
    abline (v = c (100, 200), col = '#66666666')
    
    if (h %in% c (1.0, 2.0)) {
      # Condition to extract relevant data
      con <- anatomicalData [['TREE']] == i & 
        anatomicalData [['sampleHeight']] == h & 
        anatomicalData [['sampleDate']] == as_date ('2018-11-15') &
        anatomicalData [['YEAR']] %in% 2017:2018
      
      
      lines (x = anatomicalData [['RRADDISTR']] [con] + 
               ifelse (anatomicalData [['YEAR']] [con] == 2017, 0, 
                       ifelse (anatomicalData [['YEAR']] [con] == 2018, 100, 200)), 
             y = anatomicalData [['LA']] [con],
             col = tColours [['colour']] [t], lty = 1)
    }      
  }
}
legend (x = 150, y = 2200, box.lty= 0, lty = c (1, 3), bg = 'transparent', 
        legend = c ('2018-11-15','2019-10-24'),
        col = tColours [['colour']] [t])
dev.off ()

png (filename = './fig/medianLumenAreaControlTrees.png', width = 1250, height = 850)
layout (matrix (1:30, nrow = 6, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1, 1, 1.4))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)) {
  # loop over trees
  for (i in 11:15) {
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & 
      anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['sampleDate']] == as_date ('2018-11-15') &
      anatomicalData [['YEAR']] %in% 2017:2018
    
    # Extract treatment
    t <- unique (anatomicalData [['PLOT']] [con]) 
    
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
    
    # plot measurements
    plot (x = anatomicalData [['RRADDISTR']] [con] + 
            ifelse (anatomicalData [['YEAR']] [con] == 2017, 0, 
                    ifelse (anatomicalData [['YEAR']] [con] == 2018, 100, 200)), 
          y = anatomicalData [['LA']] [con],
          xlab = ifelse (h == 0.5, 'percentage ring width (%)',' '),
          ylab = ifelse (i == 11, 
                         expression (paste ('median lumen area (',mu,m^2,')', sep = '')), 
                         ' '), 
          las = 1, col = tColours [['colour']] [t], pch = 1, 
          xlim = c (0, 300), ylim = c (0, 2300),
          yaxt = ifelse (i == 11, 't', 'n'), typ = 'l')
    
    abline (v = c (100, 200), col = '#66666666')
    
    if (h %in% c (1.0, 2.0)) {
      # Condition to extract relevant data
      con <- anatomicalData [['TREE']] == i & 
        anatomicalData [['sampleHeight']] == h & 
        anatomicalData [['sampleDate']] == as_date ('2018-11-15') &
        anatomicalData [['YEAR']] %in% 2017:2018
      
      
      lines (x = anatomicalData [['RRADDISTR']] [con] + 
               ifelse (anatomicalData [['YEAR']] [con] == 2017, 0, 
                       ifelse (anatomicalData [['YEAR']] [con] == 2018, 100, 200)), 
             y = anatomicalData [['LA']] [con],
             col = tColours [['colour']] [t], lty = 1)
    }      
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & 
      anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['sampleDate']] == as_date ('2019-10-24') &
      anatomicalData [['YEAR']] %in% 2017:2019
    
    
    lines (x = anatomicalData [['RRADDISTR']] [con] + 
             ifelse (anatomicalData [['YEAR']] [con] == 2017, 0, 
                     ifelse (anatomicalData [['YEAR']] [con] == 2018, 100, 200)), 
           y = anatomicalData [['LA']] [con],
           col = tColours [['colour']] [t], lty = 3)
  }
}
legend (x = 150, y = 2200, box.lty= 0, lty = c (1, 3), bg = 'transparent', 
        legend = c ('2018-11-15','2019-10-24'),
        col = tColours [['colour']] [t])
dev.off ()


# # Condition to extract relevant data
# con <- anatomicalData [['TREE']] %in% 1:5 & 
#   anatomicalData [['sampleHeight']] == 4 & 
#   anatomicalData [['YEAR']] %in% 2018
# 
# # Plot the smoothed treatment average
# plot (x = anatomicalData [['RRADDISTR']] [con], 
#       y = anatomicalData [['LA']] [con],
#       xlab = 'percentage ring width (%)',
#       ylab = expression (paste ('median lumen area (',mu,m^2,')', sep = '')),
#       las = 1, col = tColours [['colour']] [t], pch = 1, 
#       xlim = c (0, 100), ylim = c (0, 2300),
#       yaxt = ifelse (i == 11, 't', 'n'))

# plot median cell-wall area over ring profile in 2017, 2018, and 2019
png (filename = './fig/medianCWAChilledTrees.png', width = 1250, height = 850)
layout (matrix (1:30, nrow = 6, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1, 1, 1.3))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)) {
  # loop over trees
  for (i in 1:5) 
  {
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & 
      anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['sampleDate']] == as_date ('2018-11-15') &
      anatomicalData [['YEAR']] %in% 2017:2018
    
    # Extract treatment
    t <- unique (anatomicalData [['PLOT']] [con]) 
    
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
    
    # plot
    plot (x = anatomicalData [['RRADDISTR']] [con] + 
            ifelse (anatomicalData [['YEAR']] [con] == 2017, 0, 
                    ifelse (anatomicalData [['YEAR']] [con], 100, 200)), 
          y = anatomicalData [['CWA']] [con],
          xlab = ifelse (h == 0.5, 'percentage ring width (%)',' '),
          ylab = ifelse (i == 1, 
                         expression (paste ('median cell-wall area (',mu,m^2,')', sep = '')), 
                         ' '), 
          las = 1, col = tColours [['colour']] [t], typ = 'l', 
          xlim = c (0, 300), ylim = c (0, 1200),
          yaxt = ifelse (i == 1, 't', 'n'))
    
    abline (v = c (100, 200), col = '#66666666')
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & 
      anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['sampleDate']] == as_date ('2019-10-24') &
      anatomicalData [['YEAR']] %in% 2017:2019
    
    
    lines (x = anatomicalData [['RRADDISTR']] [con] + 
             ifelse (anatomicalData [['YEAR']] [con] == 2017, 0, 
                     ifelse (anatomicalData [['YEAR']] [con] == 2018, 100, 200)), 
           y = anatomicalData [['CWA']] [con],
           col = tColours [['colour']] [t], lty = 3)
    
    
  }
}
legend (x = 150, y = 1000, box.lty= 0, lty = c (1, 3), bg = 'transparent', 
        legend = c ('2018-11-15','2019-10-24'),
        col = tColours [['colour']] [t])
dev.off ()

png (filename = './fig/medianCWACompressedTrees.png', width = 1250, height = 850)
layout (matrix (1:30, nrow = 6, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1, 1, 1.3))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)) {
  # loop over trees
  for (i in 6:10) 
  {
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & 
      anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['sampleDate']] == as_date ('2019-10-24') &
      anatomicalData [['YEAR']] %in% 2017:2019
    
    # Extract treatment
    t <- unique (anatomicalData [['PLOT']] [con]) 
    
    # Determine plot margins
    if (i == 6 & h != 0.5) {
      par (mar = c (2, 5, 1, 1))
    } else if (i == 6 & h == 0.5) {
      par (mar = c (5, 5, 1, 1))
    } else if (i != 6 & h != 0.5) {
      par (mar = c (2, 1, 1, 1))
    } else if (i != 6 & h == 0.5) {
      par (mar = c (5, 1, 1, 1))
    }
    
    # plot
    plot (x = anatomicalData [['RRADDISTR']] [con] + 
            ifelse (anatomicalData [['YEAR']] [con] == 2017, 0, 
                    ifelse (anatomicalData [['YEAR']] [con], 100, 200)), 
          y = anatomicalData [['CWA']] [con],
          xlab = ifelse (h == 0.5, 'percentage ring width (%)',' '),
          ylab = ifelse (i == 1, 
                         expression (paste ('median cell-wall area (',mu,m^2,')', sep = '')), 
                         ' '), 
          las = 1, col = tColours [['colour']] [t], typ = 'l', lty = 3,
          xlim = c (0, 300), ylim = c (0, 1200),
          yaxt = ifelse (i == 1, 't', 'n'))
    
    abline (v = c (100, 200), col = '#66666666')
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & 
      anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['sampleDate']] == as_date ('2018-11-15') &
      anatomicalData [['YEAR']] %in% 2017:2018
    
    
    lines (x = anatomicalData [['RRADDISTR']] [con] + 
             ifelse (anatomicalData [['YEAR']] [con] == 2017, 0, 
                     ifelse (anatomicalData [['YEAR']] [con] == 2018, 100, 200)), 
           y = anatomicalData [['CWA']] [con],
           col = tColours [['colour']] [t], lty = 1)
    
    
  }
}
legend (x = 150, y = 1000, box.lty= 0, lty = c (1, 3), bg = 'transparent', 
        legend = c ('2018-11-15','2019-10-24'),
        col = tColours [['colour']] [t])
dev.off ()

png (filename = './fig/medianCWAControlTrees.png', width = 1250, height = 850)
layout (matrix (1:30, nrow = 6, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1, 1, 1.3))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 2.0, 1.5, 1.0, 0.5)) {
  # loop over trees
  for (i in 11:15) 
  {
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & 
      anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['sampleDate']] == as_date ('2018-11-15') &
      anatomicalData [['YEAR']] %in% 2017:2018
    
    # Extract treatment
    t <- unique (anatomicalData [['PLOT']] [con]) 
    
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
    
    # plot
    plot (x = anatomicalData [['RRADDISTR']] [con] + 
            ifelse (anatomicalData [['YEAR']] [con] == 2017, 0, 
                    ifelse (anatomicalData [['YEAR']] [con], 100, 200)), 
          y = anatomicalData [['CWA']] [con],
          xlab = ifelse (h == 0.5, 'percentage ring width (%)',' '),
          ylab = ifelse (i == 11, 
                         expression (paste ('median cell-wall area (',mu,m^2,')', sep = '')), 
                         ' '), 
          las = 1, col = tColours [['colour']] [t], typ = 'l', 
          xlim = c (0, 300), ylim = c (0, 1200),
          yaxt = ifelse (i == 11, 't', 'n'))
    
    abline (v = c (100, 200), col = '#66666666')
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & 
      anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['sampleDate']] == as_date ('2019-10-24') &
      anatomicalData [['YEAR']] %in% 2017:2019
    
    
    lines (x = anatomicalData [['RRADDISTR']] [con] + 
             ifelse (anatomicalData [['YEAR']] [con] == 2017, 0, 
                     ifelse (anatomicalData [['YEAR']] [con] == 2018, 100, 200)), 
           y = anatomicalData [['CWA']] [con],
           col = tColours [['colour']] [t], lty = 3)
    
    
  }
}
legend (x = 150, y = 1000, box.lty= 0, lty = c (1, 3), bg = 'transparent', 
        legend = c ('2018-11-15','2019-10-24'),
        col = tColours [['colour']] [t])
dev.off ()

# plot median cumulative number of cells formed over ring profile in 2017, 2018, and 2019
layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1.3))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 1.5, 0.5))
{
  # loop over trees
  for (i in 1:5) 
  {
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['YEAR']] %in% 2017:2019
    
    # Extract treatment
    t <- unique (anatomicalData [['PLOT']] [con]) 
    
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
    
    # plot 4.0m
    plot (x = anatomicalData [['RADDIST.CONT']] [con], 
          y = anatomicalData [['cumNCells']] [con],
          xlab = ifelse (h == 0.5, 
                         expression (paste ('cumulative radial distance (',mu,m,')', sep = '')),
                         ' '),
          ylab = ifelse (i == 1, 'cumulative cell number', ' '), 
          las = 1, col = tColours [['colour']] [t], pch = 19, ylim = c (0, 90),
          yaxt = ifelse (i == 1, 't', 'n'))
    
    abline (v = anatomicalData [con, ] %>% group_by (YEAR) %>% 
              filter (RADDIST.CONT == max (RADDIST.CONT)) %>% ungroup () %>% 
              select (RADDIST.CONT) %>% unlist (),
            col = tColours [['colour']] [t])
    
  }
}

layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1.3))
# loop over sampling heights for compressed trees
for (h in c (4.0, 2.5, 1.5, 0.5))
{
  # loop over trees
  for (i in 6:10) 
  {
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['YEAR']] %in% 2017:2019
    
    # Extract treatment
    t <- unique (anatomicalData [['PLOT']] [con]) 
    
    # Determine plot margins
    if (i == 6 & h != 0.5) {
      par (mar = c (2, 5, 1, 1))
    } else if (i == 6 & h == 0.5) {
      par (mar = c (5, 5, 1, 1))
    } else if (i != 6 & h != 0.5) {
      par (mar = c (2, 1, 1, 1))
    } else if (i != 6 & h == 0.5) {
      par (mar = c (5, 1, 1, 1))
    }
    
    # plot 4.0m
    plot (x = anatomicalData [['RADDIST.CONT']] [con], 
          y = anatomicalData [['cumNCells']] [con],
          xlab = ifelse (h == 0.5, 
                         expression (paste ('cumulative radial distance (',mu,m,')', sep = '')),
                         ' '),
          ylab = ifelse (i == 6,'cumulative cell number',' '), 
          las = 1, col = tColours [['colour']] [t], pch = 19, ylim = c (0, 90),
          yaxt = ifelse (i == 6, 't', 'n'))
    
    abline (v = anatomicalData [con, ] %>% group_by (YEAR) %>% 
              filter (RADDIST.CONT == max (RADDIST.CONT)) %>% ungroup () %>% 
              select (RADDIST.CONT) %>% unlist (),
            col = tColours [['colour']] [t])
  }
}

layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1.3))
# loop over sampling heights for control trees
for (h in c (4.0, 2.5, 1.5, 0.5))
{
  # loop over trees
  for (i in 11:15) 
  {
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['YEAR']] %in% 2017:2019
    
    # Extract treatment
    t <- unique (anatomicalData [['PLOT']] [con]) 
    
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
    
    # plot 4.0m
    plot (x = anatomicalData [['RADDIST.CONT']] [con], 
          y = anatomicalData [['cumNCells']] [con],
          xlab = ifelse (h == 0.5, 
                         expression (paste ('cumulative radial distance (',mu,m,')', sep = '')),
                         ' '),
          ylab = ifelse (i == 11,'cumulative cell number' ,' '), 
          las = 1, col = tColours [['colour']] [t], pch = 19, ylim = c (0, 90),
          yaxt = ifelse (i == 11, 't', 'n'))
    
    abline (v = anatomicalData [con, ] %>% group_by (YEAR) %>% 
              filter (RADDIST.CONT == max (RADDIST.CONT)) %>% ungroup () %>% 
              select (RADDIST.CONT) %>% unlist (),
            col = tColours [['colour']] [t])
    
  }
}

# Plot anatomical traits of the 2018 ring over time of formation with treatment dates
#----------------------------------------------------------------------------------------
# plot median lumen area over time for chilled trees
#----------------------------------------------------------------------------------------
layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1.3))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 1.5, 0.5)) {
  # loop over trees
  for (i in 1:5) {
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & 
      anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['sampleDate']] == as_date ('2018-11-15') & 
      anatomicalData [['YEAR']] == 2018
    
    # Extract treatment
    t <- unique (anatomicalData [['PLOT']] [con]) 
    
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
    
    # plot 4.0m
    plot (x = as_date (anatomicalData [['period']] [con]), 
          y = anatomicalData [['LA']] [con],
          xlim = c (as_date ('2018-04-01'), as_date ('2018-12-31')),
          xlab = ifelse (h == 0.5, 'date of formation (doy)', ''),
          ylab = ifelse (i == 1, 
                         expression (paste ('median lumen area (',mu,m^2,')', sep = '')), 
                         ' '), 
          las = 1, col = tColours [['colour']] [t], ylim = c (0, 2500),
          yaxt = ifelse (i == 1, 't', 'n'), typ = 'l')
    
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & 
      anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['sampleDate']] == as_date ('2019-10-24') & 
      anatomicalData [['YEAR']] == 2018
    lines (x = as_date (anatomicalData [['period']] [con]), 
           y = anatomicalData [['LA']] [con], lty = 3,
           col = tColours [['colour']] [t])
    
    criticalDates (group = t)
    
  }
}
legend (x = as_date ('2018-08-01'), y = 2500, box.lty= 0, lty = c (1, 3), 
        bg = 'transparent', 
        legend = c ('2018-11-15','2019-10-24'),
        col = tColours [['colour']] [t])

# plot median lumen area over time for compressed trees
#----------------------------------------------------------------------------------------
layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1.3))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 1.5, 0.5)) {
  # loop over trees
  for (i in 6:10) {
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & 
      anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['sampleDate']] == as_date ('2019-10-24') & 
      anatomicalData [['YEAR']] == 2018
    
    # Extract treatment
    t <- 4
    
    # Determine plot margins
    if (i == 6 & h != 0.5) {
      par (mar = c (2, 5, 1, 1))
    } else if (i == 6 & h == 0.5) {
      par (mar = c (5, 5, 1, 1))
    } else if (i != 6 & h != 0.5) {
      par (mar = c (2, 1, 1, 1))
    } else if (i != 6 & h == 0.5) {
      par (mar = c (5, 1, 1, 1))
    }
    
    # plot 4.0m
    plot (x = as_date (anatomicalData [['period']] [con]), 
          y = anatomicalData [['LA']] [con],
          xlim = c (as_date ('2018-04-01'), as_date ('2018-12-31')),
          xlab = ifelse (h == 0.5, 'date of formation (doy)', ''),
          ylab = ifelse (i == 6, 
                         expression (paste ('median lumen area (',mu,m^2,')', sep = '')), 
                         ' '), 
          las = 1, col = tColours [['colour']] [t], ylim = c (0, 2500),
          yaxt = ifelse (i == 6, 't', 'n'), typ = 'l')
    
    criticalDates (group = t)
    
  }
}
legend (x = as_date ('2018-08-01'), y = 2500, box.lty= 0, lty = c (1, 3), 
        bg = 'transparent', 
        legend = c ('2018-11-15','2019-10-24'),
        col = tColours [['colour']] [t])

# plot median lumen area over time for control trees
#----------------------------------------------------------------------------------------
layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1.3))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 1.5, 0.5)) {
  # loop over trees
  for (i in 11:15) {
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & 
      anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['sampleDate']] == as_date ('2018-11-15') & 
      anatomicalData [['YEAR']] == 2018
    
    # Extract treatment
    t <- unique (anatomicalData [['PLOT']] [con]) 
    
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
    
    # plot 4.0m
    plot (x = as_date (anatomicalData [['period']] [con]), 
          y = anatomicalData [['LA']] [con],
          xlim = c (as_date ('2018-04-01'), as_date ('2018-12-31')),
          xlab = ifelse (h == 0.5, 'date of formation (doy)', ''),
          ylab = ifelse (i == 11, 
                         expression (paste ('median lumen area (',mu,m^2,')', sep = '')), 
                         ' '), 
          las = 1, col = tColours [['colour']] [t], ylim = c (0, 2500),
          yaxt = ifelse (i == 11, 't', 'n'), typ = 'l')
    
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & 
      anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['sampleDate']] == as_date ('2019-10-24') & 
      anatomicalData [['YEAR']] == 2018
    lines (x = as_date (anatomicalData [['period']] [con]), 
           y = anatomicalData [['LA']] [con], lty = 3,
           col = tColours [['colour']] [t])
    
    criticalDates (group = t)
    
  }
}
legend (x = as_date ('2018-08-01'), y = 2500, box.lty= 0, lty = c (1, 3), 
        bg = 'transparent', 
        legend = c ('2018-11-15','2019-10-24'),
        col = tColours [['colour']] [t])

# plot median cell-wall area over time for chilled trees
#----------------------------------------------------------------------------------------
layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1.3))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 1.5, 0.5)) {
  # loop over trees
  for (i in 1:5) 
  {
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['YEAR']] == 2018
    
    # Extract treatment
    t <- unique (anatomicalData [['PLOT']] [con]) 
    
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
    
    # plot 4.0m
    plot (x = as_date (anatomicalData [['period']] [con]), 
          y = anatomicalData [['CWA']] [con],
          xlim = c (as_date ('2018-04-01'), as_date ('2018-12-31')),
          xlab = ifelse (h == 0.5, 'date of formation (doy)', ''),
          ylab = ifelse (i == 1, 
                         expression (paste ('median cell-wall area (',mu,m^2,')', sep = '')), 
                         ' '), 
          las = 1, col = tColours [['colour']] [t], pch = 19, ylim = c (0, 1000),
          yaxt = ifelse (i == 1, 't', 'n'))
    
    criticalDates (group = t)
    
  }
}

# plot median cell-wall area over time for compressed trees
#----------------------------------------------------------------------------------------
layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1.3))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 1.5, 0.5)) {
  # loop over trees
  for (i in 6:10) 
  {
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['YEAR']] == 2018
    
    # Extract treatment
    t <- unique (anatomicalData [['PLOT']] [con]) 
    
    # Determine plot margins
    if (i == 6 & h != 0.5) {
      par (mar = c (2, 5, 1, 1))
    } else if (i == 6 & h == 0.5) {
      par (mar = c (5, 5, 1, 1))
    } else if (i != 6 & h != 0.5) {
      par (mar = c (2, 1, 1, 1))
    } else if (i != 6 & h == 0.5) {
      par (mar = c (5, 1, 1, 1))
    }
    
    
    # jump iteration when 09 I which does not have data
    if (i == 9 & h == 0.5) {
      plot (x = as_date ('2018-07-20'), 
            y = 1,
            xlim = c (as_date ('2018-04-01'), as_date ('2018-12-31')),
            xlab = ifelse (h == 0.5, 'date of formation (doy)', ''),
            ylab = ifelse (i == 1, 
                           expression (paste ('median cell-wall area (',mu,m^2,')', sep = '')), 
                           ' '), 
            las = 1, col = tColours [['colour']] [t], pch = 19, ylim = c (0, 1000),
            yaxt = ifelse (i == 6, 't', 'n'))
    } else { # Else plot data
      plot (x = as_date (anatomicalData [['period']] [con]), 
            y = anatomicalData [['CWA']] [con],
            xlim = c (as_date ('2018-04-01'), as_date ('2018-12-31')),
            xlab = ifelse (h == 0.5, 'date of formation (doy)', ''),
            ylab = ifelse (i == 1, 
                           expression (paste ('median cell-wall area (',mu,m^2,')', sep = '')), 
                           ' '), 
            las = 1, col = tColours [['colour']] [t], pch = 19, ylim = c (0, 1000),
            yaxt = ifelse (i == 6, 't', 'n'))
      criticalDates (group = t)
    }
  }
}

# plot median cell-wall area over time for control trees
#----------------------------------------------------------------------------------------
layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1.3))
# loop over sampling heights for control trees
for (h in c (4.0, 2.5, 1.5, 0.5)) {
  # loop over trees
  for (i in 11:15) 
  {
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['YEAR']] == 2018
    
    # Extract treatment
    t <- unique (anatomicalData [['PLOT']] [con]) 
    
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
    
    # plot 4.0m
    plot (x = as_date (anatomicalData [['period']] [con]), 
          y = anatomicalData [['CWA']] [con],
          xlim = c (as_date ('2018-04-01'), as_date ('2018-12-31')),
          xlab = ifelse (h == 0.5, 'date of formation (doy)', ''),
          ylab = ifelse (i == 1, 
                         expression (paste ('median cell-wall area (',mu,m^2,')', sep = '')), 
                         ' '), 
          las = 1, col = tColours [['colour']] [t], pch = 19, ylim = c (0, 1200),
          yaxt = ifelse (i == 11, 't', 'n'))
    
    criticalDates (group = t)
    
  }
}

# plot cumulative number of cells formes over time for chilled trees
#----------------------------------------------------------------------------------------
layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1.3))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 1.5, 0.5)) {
  # loop over trees
  for (i in 1:5) 
  {
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['YEAR']] == 2018
    
    # Extract treatment
    t <- unique (anatomicalData [['PLOT']] [con]) 
    
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
    
    # plot 4.0m
    plot (x = as_date (anatomicalData [['period']] [con]), 
          y = anatomicalData [['cumNCells']] [con],
          xlim = c (as_date ('2018-04-01'), as_date ('2018-12-31')),
          xlab = ifelse (h == 0.5, 'date of formation (doy)', ''),
          ylab = ifelse (i == 1, 'cumulative cell number', ' '), 
          las = 1, col = tColours [['colour']] [t], pch = 19, ylim = c (0, 90),
          yaxt = ifelse (i == 1, 't', 'n'))
    
    criticalDates (group = t)
    
  }
}

# plot median cell-wall area over time for compressed trees
#----------------------------------------------------------------------------------------
layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1.3))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 1.5, 0.5))
{
  # loop over trees
  for (i in 6:10) 
  {
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['YEAR']] == 2018
    
    # Extract treatment
    t <- unique (anatomicalData [['PLOT']] [con]) 
    
    # Determine plot margins
    if (i == 6 & h != 0.5) {
      par (mar = c (2, 5, 1, 1))
    } else if (i == 6 & h == 0.5) {
      par (mar = c (5, 5, 1, 1))
    } else if (i != 6 & h != 0.5) {
      par (mar = c (2, 1, 1, 1))
    } else if (i != 6 & h == 0.5) {
      par (mar = c (5, 1, 1, 1))
    }
    
    
    # jump iteration when 09 I which does not have data
    if (i == 9 & h == 0.5) {
      plot (x = as_date ('2018-07-20'), 
            y = 1,
            xlim = c (as_date ('2018-04-01'), as_date ('2018-12-31')),
            xlab = ifelse (h == 0.5, 'date of formation (doy)', ''),
            ylab = ifelse (i == 6, 'cumulative cell number', ' '), 
            las = 1, col = tColours [['colour']] [t], pch = 19, ylim = c (0, 90),
            yaxt = ifelse (i == 6, 't', 'n'))
    } else { # Else plot data
      plot (x = as_date (anatomicalData [['period']] [con]), 
            y = anatomicalData [['cumNCells']] [con],
            xlim = c (as_date ('2018-04-01'), as_date ('2018-12-31')),
            xlab = ifelse (h == 0.5, 'date of formation (doy)', ''),
            ylab = ifelse (i == 6, 'cumulative cell number', ' '), 
            las = 1, col = tColours [['colour']] [t], pch = 19, ylim = c (0, 90),
            yaxt = ifelse (i == 6, 't', 'n'))
      criticalDates (group = t)
    }
  }
}

# plot median cell-wall area over time for control trees
#----------------------------------------------------------------------------------------
layout (matrix (1:20, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1, 1, 1, 1), 
        heights = c (1, 1, 1, 1.3))
# loop over sampling heights for chilled trees
for (h in c (4.0, 2.5, 1.5, 0.5))
{
  # loop over trees
  for (i in 11:15) 
  {
    
    # Condition to extract relevant data
    con <- anatomicalData [['TREE']] == i & anatomicalData [['sampleHeight']] == h & 
      anatomicalData [['YEAR']] == 2018
    
    # Extract treatment
    t <- unique (anatomicalData [['PLOT']] [con]) 
    
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
    
    # plot 4.0m
    plot (x = as_date (anatomicalData [['period']] [con]), 
          y = anatomicalData [['cumNCells']] [con],
          xlim = c (as_date ('2018-04-01'), as_date ('2018-12-31')),
          xlab = ifelse (h == 0.5, 'date of formation (doy)', ''),
          ylab = ifelse (i == 11, 'cumulative cell number', ' '), 
          las = 1, col = tColours [['colour']] [t], pch = 19, ylim = c (0, 90),
          yaxt = ifelse (i == 11, 't', 'n'))
    
    criticalDates (group = t)
    
  }
}

#========================================================================================