#========================================================================================
# This script plots anatomical vairables to deduce any treatment effects
#----------------------------------------------------------------------------------------

# Source anatomical data
#----------------------------------------------------------------------------------------
source ('processAnatomicalData.R')

# Plot cell-wall thickness by period of formation
#----------------------------------------------------------------------------------------
layout (matrix (1:3, nrow = 1), widths = c (1.3, 1, 1))
par (mar = c (5, 12, 5, 1))
boxplot (CWTALL ~ PLOT * sampleHeight * desc (period), 
         data = filter (anatomicalData, YEAR == 2018, PLOT == 1, 
                        period != as_date ('2018-07-12')), 
         horizontal = TRUE, col = tColours [['colour']] [1], xlab = '', ylab = '', 
         main = 'control', axes = FALSE, at = c (1:4,6:9,11:14,16:19), ylim = c (1, 5))
axis (side = 1)
axis (side = 2, at = 1:4, labels = c (0.5, 1.5, 2.5, 4.0), las = 1)
axis (side = 2, at = 6:9, labels = c (0.5, 1.5, 2.5, 4.0), las = 1)
axis (side = 2, at = 11:14, labels = c (0.5, 1.5, 2.5, 4.0), las = 1)
axis (side = 2, at = 16:19, labels = c (0.5, 1.5, 2.5, 4.0), las = 1)
mtext (side = 2, line = 5, at = c (2.5, 7.5, 12.5, 17.5), text = c ('11 Nov', '6 Sep', '26 Aug', '19 Jul')) 

# Add panel for compression 
par (mar = c (5, 5, 5, 1))
boxplot (CWTALL ~ PLOT * sampleHeight * desc (period), 
         data = filter (anatomicalData, YEAR == 2018, PLOT == 4, 
                        period != as_date ('2018-07-12')), 
         horizontal = TRUE, col = tColours [['colour']] [4], xlab = '', ylab = '', 
         main = 'compressed', axes = FALSE, at = c (1:4,6:9,11:14,16:19), ylim = c (1, 5))
axis (side = 1)
axis (side = 2, at = 1:4, labels = c (0.5, 1.5, 2.5, 4.0), las = 1)
axis (side = 2, at = 6:9, labels = c (0.5, 1.5, 2.5, 4.0), las = 1)
axis (side = 2, at = 11:14, labels = c (0.5, 1.5, 2.5, 4.0), las = 1)
axis (side = 2, at = 16:19, labels = c (0.5, 1.5, 2.5, 4.0), las = 1)

# Add panel for chilling 
par (mar = c (5, 5, 5, 1))
boxplot (CWTALL ~ PLOT * sampleHeight * desc (period), 
         data = filter (anatomicalData, YEAR == 2018, PLOT == 5, 
                        period %notin% as_date (c ('2018-07-12','2018-06-14'))), 
         horizontal = TRUE, col = tColours [['colour']] [5], xlab = '', ylab = '', 
         main = 'chilled', axes = FALSE, at = c (1:4,6:9,11:14,16:19), ylim = c (1, 5))
axis (side = 1)
axis (side = 2, at = 1:4, labels = c (0.5, 1.5, 2.5, 4.0), las = 1)
axis (side = 2, at = 6:9, labels = c (0.5, 1.5, 2.5, 4.0), las = 1)
axis (side = 2, at = 11:14, labels = c (0.5, 1.5, 2.5, 4.0), las = 1)
axis (side = 2, at = 16:19, labels = c (0.5, 1.5, 2.5, 4.0), las = 1)
#========================================================================================