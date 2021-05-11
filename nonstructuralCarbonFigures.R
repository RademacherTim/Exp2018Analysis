#========================================================================================
# script to plot nonstructural carbon data from the 2018 chilling experiment on white 
# pine at Harvard Forest. 
#----------------------------------------------------------------------------------------

# source colour schemes and ploting functions
#----------------------------------------------------------------------------------------
if (!exists ('tColours')) source ('./plotingFunctions.R')

# source processed data
#----------------------------------------------------------------------------------------
source ('./readNonstructuralCarbonData.R') 

# sort the stemData2018 and rootData2018 tibbles to makes lines in graphs look decent
#----------------------------------------------------------------------------------------
stemData2018 <- stemData2018 %>% 
  group_by (treeID, sampleHeight) %>% 
  arrange (DateOfSampleCollection)
rootData2018 <- rootData2018 %>% 
  group_by (treeID, sampleHeight) %>% 
  arrange (DateOfSampleCollection)

# plot the 2018 stem sugar concentration data by tree
#----------------------------------------------------------------------------------------
layout (matrix (1:15, nrow = 3, byrow = TRUE))
for (t in 1:15) {
  # plot below treatment
  #--------------------------------------------------------------------------------------
  con <- stemData2018 [['treeID']] == t & stemData2018 [['sampleHeight']] == 1
  plot (x = stemData2018 [['DateOfSampleCollection']] [con],
        y = stemData2018 [['ConcentrationSugarPerDW']] [con], typ = 'l',
        xlab = 'date', ylab = 'sugar concentration (%DW)', las = 1,
        ylim = c (0, 1.75), lty = 3, lwd = 2,
        col = tColours [['colour']] [unique (stemData2018 [['treatment']] [con])])
  # add in between the treatments
  #--------------------------------------------------------------------------------------
  con <- stemData2018 [['treeID']] == t & stemData2018 [['sampleHeight']] == 2
  lines (x = stemData2018 [['DateOfSampleCollection']] [con],
         y = stemData2018 [['ConcentrationSugarPerDW']] [con], lty = 2, lwd = 2,
         col = tColours [['colour']] [unique (stemData2018 [['treatment']] [con])])
  # add in above the treatments
  #--------------------------------------------------------------------------------------
  con <- stemData2018 [['treeID']] == t & stemData2018 [['sampleHeight']] == 3
  lines (x = stemData2018 [['DateOfSampleCollection']] [con],
         y = stemData2018 [['ConcentrationSugarPerDW']] [con], lty = 1, lwd = 2,
         col = tColours [['colour']] [unique (stemData2018 [['treatment']] [con])])
  
  # add a tree panel identifier
  #--------------------------------------------------------------------------------------
  text (x = as_datetime ('2018-05-25'), y = 1.55, labels = t, cex = 2)
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  criticalDates (group = unique (stemData2018 [['treatment']] [con]), asDate = FALSE)
  
  # add legend
  #--------------------------------------------------------------------------------------
  if (t == 15) {
    legend (x = as.POSIXct ('2018-06-30'), cex = 0.6,
            y = 0.75, box.lty = 0, lty = 1:3, lwd = 1, legend = c ('above','middle','below'))}
}

# plot the 2018 stem starch concentration data by tree
#----------------------------------------------------------------------------------------
layout (matrix (1:15, nrow = 3, byrow = TRUE))
for (t in 1:15) {
  # plot below treatment
  con <- stemData2018 [['treeID']] == t & stemData2018 [['sampleHeight']] == 1
  #--------------------------------------------------------------------------------------
  plot (x = stemData2018 [['DateOfSampleCollection']] [con],
        y = stemData2018 [['ConcentrationStarchPerDW']] [con], typ = 'l',
        xlab = 'date', ylab = 'starch concentration (%DW)', las = 1,
        ylim = c (0, 0.8), lty = 3, lwd = 2,
        col = tColours [['colour']] [unique (stemData2018 [['treatment']] [con])])
  
  # add in between the treatments
  #--------------------------------------------------------------------------------------
  con <- stemData2018 [['treeID']] == t & stemData2018 [['sampleHeight']] == 2
  lines (x = stemData2018 [['DateOfSampleCollection']] [con],
         y = stemData2018 [['ConcentrationStarchPerDW']] [con], lty = 2, lwd = 2,
         col = tColours [['colour']] [unique (stemData2018 [['treatment']] [con])])
  
  # add in above the treatments
  #--------------------------------------------------------------------------------------
  con <- stemData2018 [['treeID']] == t & stemData2018 [['sampleHeight']] == 3
  lines (x = stemData2018 [['DateOfSampleCollection']] [con],
         y = stemData2018 [['ConcentrationStarchPerDW']] [con], lty = 1, lwd = 2,
         col = tColours [['colour']] [unique (stemData2018 [['treatment']] [con])])
  
  # add a tree panel identifier
  #--------------------------------------------------------------------------------------
  text (x = as_datetime ('2018-05-25'), y = 0.75, labels = t, cex = 2)
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  criticalDates (group = unique (stemData2018 [['treatment']] [con]), asDate = FALSE)
  
  # Add legend
  #--------------------------------------------------------------------------------------
  if (t == 15) {
    legend (x = as.POSIXct ('2018-06-30'), cex = 0.6,
            y = 0.8, box.lty = 0, lty = 1:3, lwd = 1, legend = c ('above','middle','below'))}
}

# plot the 2018 root sugar concentration data by tree
#----------------------------------------------------------------------------------------
layout (matrix (1:15, nrow = 3, byrow = TRUE))
for (t in 1:15) {
  # plot root sugar concentation
  #--------------------------------------------------------------------------------------
  con <- rootData2018 [['treeID']] == t
  plot (x = rootData2018 [['DateOfSampleCollection']] [con],
        y = rootData2018 [['ConcentrationSugarPerDW']] [con], typ = 'l',
        xlab = 'date', ylab = 'sugar concentration (%DW)', las = 1,
        ylim = c (0, 1.75), lwd = 2,
        col = tColours [['colour']] [unique (rootData2018 [['treatment']] [con])])

  # add a tree panel identifier
  #--------------------------------------------------------------------------------------
  text (x = as_datetime ('2018-05-25'), y = 1.55, labels = t, cex = 2)
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  criticalDates (group = unique (rootData2018 [['treatment']] [con]), asDate = FALSE)
  
}

# plot the 2018 root starch concentration data by tree
#----------------------------------------------------------------------------------------
layout (matrix (1:15, nrow = 3, byrow = TRUE))
for (t in 1:15) {
  # plot root sugar concentation
  #--------------------------------------------------------------------------------------
  con <- rootData2018 [['treeID']] == t
  plot (x = rootData2018 [['DateOfSampleCollection']] [con],
        y = rootData2018 [['ConcentrationStarchPerDW']] [con], typ = 'l',
        xlab = 'date', ylab = 'starch concentration (%DW)', las = 1,
        ylim = c (0, 0.8), lwd = 2,
        col = tColours [['colour']] [unique (rootData2018 [['treatment']] [con])])
  
  # add a tree panel identifier
  #--------------------------------------------------------------------------------------
  text (x = as_datetime ('2018-05-25'), y = 1.55, labels = t, cex = 2)
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  criticalDates (group = unique (rootData2018 [['treatment']] [con]), asDate = FALSE)
  
}

# summarise the stem data by treatment and sampling height
#----------------------------------------------------------------------------------------
summaryDataStem <- stemData2018 %>% 
  group_by (DateOfSampleCollection, treatment, sampleHeight) %>% 
  summarise (meanSugar = mean (ConcentrationSugarPerDW), 
             sdSugar   = sd (ConcentrationSugarPerDW), 
             seSugar   = se (ConcentrationSugarPerDW),
             meanStarch = mean (ConcentrationStarchPerDW), 
             sdStarch   = sd (ConcentrationStarchPerDW), 
             seStarch   = se (ConcentrationStarchPerDW),
             .groups = 'keep')

# plot the 2018 stem sugar concentration data by treatment
#----------------------------------------------------------------------------------------
layout (matrix (1:3, nrow = 1, byrow = TRUE))
for (t in c (1, 4, 5)) {
  par (mar = c (5, 5, 1, 1))
  con <- summaryDataStem [['treatment']] == 1 &
    summaryDataStem [['sampleHeight']] == 1 &
    !is.na (summaryDataStem [['meanSugar']])
  plot (x = summaryDataStem [['DateOfSampleCollection']] [con],
        y = summaryDataStem [['meanSugar']] [con], 
        typ = 'l', xlab = 'date', ylab = 'wood sugar concentration (% dry weight)', las = 1,
        ylim = c (0, 1.5), col = 'white')
  
  for (h in 1:3) {
    con <- summaryDataStem [['treatment']] == t &
      summaryDataStem [['sampleHeight']] == h &
      !is.na (summaryDataStem [['meanSugar']])
    polygon (x = c (summaryDataStem [['DateOfSampleCollection']] [con], 
                    rev (summaryDataStem [['DateOfSampleCollection']] [con])),
             y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con], 
                    rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
             col = addOpacity (tColours [['colour']] [t], 0.3), lty = 0)
    lines (x = summaryDataStem [['DateOfSampleCollection']] [con],
           y = summaryDataStem [['meanSugar']] [con], lty = ifelse (h == 1, 3, ifelse (h == 2, 2, 1)), 
           lwd = 2, col = tColours [['colour']] [t])
  }
  
  # add panel descriptor
  #--------------------------------------------------------------------------------------
  text (x = as.POSIXct ('2018-05-20'),
        y = 1.45, cex = 2, pos = 4, 
        labels = ifelse (t == 1, 'control', ifelse (t == 4, 'compressed', 'chilled')))
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  criticalDates (group = t, asDate = FALSE)
  
  # add legend
  #--------------------------------------------------------------------------------------
  if (t == 5) {
    legend (x = as.POSIXct ('2018-09-03'), bg = 'transparent',
            y = 1.45, box.lty = 0, lty = 1:3, lwd = 2, legend = c ('above','middle','below'))}

}

# plot the 2018 stem starch concentration data by treatment
#----------------------------------------------------------------------------------------
layout (matrix (1:3, nrow = 1, byrow = TRUE))
for (t in c (1, 4, 5)) {
  par (mar = c (5, 5, 1, 1))
  con <- summaryDataStem [['treatment']] == 1 &
    summaryDataStem [['sampleHeight']] == 1 &
    !is.na (summaryDataStem [['meanStarch']])
  plot (x = summaryDataStem [['DateOfSampleCollection']] [con],
        y = summaryDataStem [['meanStarch']] [con], 
        typ = 'l', xlab = 'date', ylab = 'wood starch concentration (% dry weight)', las = 1,
        ylim = c (0, 0.6), col = 'white')
  
  for (h in 1:3) {
    con <- summaryDataStem [['treatment']] == t &
      summaryDataStem [['sampleHeight']] == h &
      !is.na (summaryDataStem [['meanStarch']])
    polygon (x = c (summaryDataStem [['DateOfSampleCollection']] [con], 
                    rev (summaryDataStem [['DateOfSampleCollection']] [con])),
             y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con], 
                    rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
             col = addOpacity (tColours [['colour']] [t], 0.3), lty = 0)
    lines (x = summaryDataStem [['DateOfSampleCollection']] [con],
           y = summaryDataStem [['meanStarch']] [con], lty = ifelse (h == 1, 3, ifelse (h == 2, 2, 1)), 
           lwd = 2, col = tColours [['colour']] [t])
  }
  
  # add panel descriptor
  #--------------------------------------------------------------------------------------
  text (x = as.POSIXct ('2018-05-20'),
        y = 0.58, cex = 2, pos = 4, 
        labels = ifelse (t == 1, 'control', ifelse (t == 4, 'compressed', 'chilled')))
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  criticalDates (group = t, asDate = FALSE)
  
  # add legend
  #--------------------------------------------------------------------------------------
  if (t == 5) {
    legend (x = as.POSIXct ('2018-09-03'),
            y = 0.58, box.lty = 0, lty = 1:3, lwd = 2, legend = c ('above','middle','below'))}
}   

# plot stem xylem soluble sugar concentration by treatment and sampling height
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2018ChillingSolubleSugarConcentrations.png', width = 400, height = 600)
layout (matrix (1:3, nrow = 3, byrow = TRUE), height = c (1, 1, 1.2))
for (h in 3:1) {
  
  # determine margins
  if (h == 1) {
    par (mar = c (5, 5, 1, 1))
  } else {
    par (mar = c (1, 5, 1, 1))
  }
  con <- summaryDataStem [['treatment']] == 1 &
    summaryDataStem [['sampleHeight']] == h &
    !is.na (summaryDataStem [['meanSugar']])
  plot (x = summaryDataStem [['DateOfSampleCollection']] [con],
        y = summaryDataStem [['meanSugar']] [con], 
        typ = 'l', xlab = '', ylab = 'Wood sugar concentration (% dry weight)', las = 1,
        ylim = c (0, 1.5), col = 'white', axes = FALSE)
  
  # plot mean and standard error of soluble sugar concentrations for control trees
  con <- summaryDataStem [['treatment']] == 1 &
      summaryDataStem [['sampleHeight']] == h &
      !is.na (summaryDataStem [['meanSugar']])
  polygon (x = c (summaryDataStem [['DateOfSampleCollection']] [con], 
                  rev (summaryDataStem [['DateOfSampleCollection']] [con])),
           y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con],  
                  rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
  lines (x = summaryDataStem [['DateOfSampleCollection']] [con],
         y = summaryDataStem [['meanSugar']] [con], lty = 1, 
         lwd = 2, col = tColours [['colour']] [1])
  
  
  # plot mean and standard error of soluble sugar concentrations for chilled trees
  con <- summaryDataStem [['treatment']] == 5 &
    summaryDataStem [['sampleHeight']] == h &
    !is.na (summaryDataStem [['meanSugar']])
  polygon (x = c (summaryDataStem [['DateOfSampleCollection']] [con], 
                  rev (summaryDataStem [['DateOfSampleCollection']] [con])),
           y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con],  
                  rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [5], 0.3), lty = 0)
  lines (x = summaryDataStem [['DateOfSampleCollection']] [con],
         y = summaryDataStem [['meanSugar']] [con], lty = 2, 
         lwd = 2, col = tColours [['colour']] [5])
  
  # adda axes
  #--------------------------------------------------------------------------------------
  if (h == 1) {
    axis (side = 1, labels = c ('May','Jun','Jul','Aug','Sep','Oct','Nov'),
          at = c (as_datetime ('2018-05-01'), as_datetime ('2018-06-01'), as_datetime ('2018-07-01'), 
                  as_datetime ('2018-08-01'), as_datetime ('2018-09-01'), as_datetime ('2018-10-01'), 
                  as_datetime ('2018-11-01')))
  } else {
    axis (side = 1, labels = rep ('', 7),
          at = c (as_datetime ('2018-05-01'), as_datetime ('2018-06-01'), as_datetime ('2018-07-01'), 
                  as_datetime ('2018-08-01'), as_datetime ('2018-09-01'), as_datetime ('2018-10-01'), 
                  as_datetime ('2018-11-01')))
  }
  axis (side = 2, las = 1, at = seq (0, 1.5, 0.4))
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  criticalDates (group = t, asDate = FALSE)
  
  # add legend
  #--------------------------------------------------------------------------------------
  if (h == 1) {
    legend (x = as_datetime ('2018-05-01'), y = 1.2, box.lty = 0, bg = 'transparent', 
            legend = c ('control', 'chilled'), lty = 1:2, lwd = 2, 
            col = tColours [['colour']] [c (1, 5)])
  }
}   
dev.off ()

# plot stem xylem starch concentration by treatment and sampling height
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2018ChillingStarchConcentrations.png', width = 400, height = 600)
layout (matrix (1:3, nrow = 3, byrow = TRUE), height = c (1, 1, 1.2))
for (h in 3:1) {
  
  # determine margins
  if (h == 1) {
    par (mar = c (5, 5, 1, 1))
  } else {
    par (mar = c (1, 5, 1, 1))
  }
  con <- summaryDataStem [['treatment']] == 1 &
    summaryDataStem [['sampleHeight']] == h &
    !is.na (summaryDataStem [['meanStarch']])
  plot (x = summaryDataStem [['DateOfSampleCollection']] [con],
        y = summaryDataStem [['meanStarch']] [con], 
        typ = 'l', xlab = '', ylab = 'Wood starch concentration (% dry weight)', las = 1,
        ylim = c (0, 0.5), col = 'white', axes = FALSE)
  
  # plot mean and standard error of soluble starch concentrations for control trees
  con <- summaryDataStem [['treatment']] == 1 &
    summaryDataStem [['sampleHeight']] == h &
    !is.na (summaryDataStem [['meanStarch']])
  polygon (x = c (summaryDataStem [['DateOfSampleCollection']] [con], 
                  rev (summaryDataStem [['DateOfSampleCollection']] [con])),
           y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con],  
                  rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
  lines (x = summaryDataStem [['DateOfSampleCollection']] [con],
         y = summaryDataStem [['meanStarch']] [con], lty = 1, 
         lwd = 2, col = tColours [['colour']] [1])
  
  
  # plot mean and standard error of soluble starch concentrations for chilled trees
  con <- summaryDataStem [['treatment']] == 5 &
    summaryDataStem [['sampleHeight']] == h &
    !is.na (summaryDataStem [['meanStarch']])
  polygon (x = c (summaryDataStem [['DateOfSampleCollection']] [con], 
                  rev (summaryDataStem [['DateOfSampleCollection']] [con])),
           y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con],  
                  rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [5], 0.3), lty = 0)
  lines (x = summaryDataStem [['DateOfSampleCollection']] [con],
         y = summaryDataStem [['meanStarch']] [con], lty = 2, 
         lwd = 2, col = tColours [['colour']] [5])
  
  # adda axes
  #--------------------------------------------------------------------------------------
  if (h == 1) {
    axis (side = 1, labels = c ('May','Jun','Jul','Aug','Sep','Oct','Nov'),
          at = c (as_datetime ('2018-05-01'), as_datetime ('2018-06-01'), as_datetime ('2018-07-01'), 
                  as_datetime ('2018-08-01'), as_datetime ('2018-09-01'), as_datetime ('2018-10-01'), 
                  as_datetime ('2018-11-01')))
  } else {
    axis (side = 1, labels = rep ('', 7),
          at = c (as_datetime ('2018-05-01'), as_datetime ('2018-06-01'), as_datetime ('2018-07-01'), 
                  as_datetime ('2018-08-01'), as_datetime ('2018-09-01'), as_datetime ('2018-10-01'), 
                  as_datetime ('2018-11-01')))
  }
  axis (side = 2, las = 1, at = seq (0, 0.5, 0.1))
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  criticalDates (group = 5, asDate = FALSE)
  
  # add legend
  #--------------------------------------------------------------------------------------
  if (h == 1) {
    legend (x = as_datetime ('2018-05-01'), y = 0.2, box.lty = 0, bg = 'transparent', 
            legend = c ('control', 'chilled'), lty = 1:2, lwd = 2, 
            col = tColours [['colour']] [c (1, 5)])
  }
}   
dev.off ()

# summarise the root data by treatment 
#----------------------------------------------------------------------------------------
summaryDataStem <- rootData2018 %>% 
  group_by (DateOfSampleCollection, treatment) %>% 
  summarise (meanSugar = mean (ConcentrationSugarPerDW), 
             sdSugar   = sd (ConcentrationSugarPerDW), 
             seSugar   = se (ConcentrationSugarPerDW),
             meanStarch = mean (ConcentrationStarchPerDW), 
             sdStarch   = sd (ConcentrationStarchPerDW), 
             seStarch   = se (ConcentrationStarchPerDW),
             .groups = 'keep')

# plot root starch concentration by treatment
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2018ChillingSolubleSugarConcentrationsRoots.png', 
     width = 400, height = 600)
par (mar = c (5, 5, 1, 1))
con <- summaryDataStem [['treatment']] == 1
plot (x = summaryDataStem [['DateOfSampleCollection']] [con],
      y = summaryDataStem [['meanSugar']] [con], 
      typ = 'l', xlab = '', ylab = 'Root soluble sugar concentration (% dry weight)', las = 1,
      ylim = c (0, 1.5), col = 'white', axes = FALSE)

# plot mean and standard error of soluble starch concentrations for control trees
con <- summaryDataStem [['treatment']] == 1
polygon (x = c (summaryDataStem [['DateOfSampleCollection']] [con], 
                rev (summaryDataStem [['DateOfSampleCollection']] [con])),
         y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con],  
                rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
         col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
lines (x = summaryDataStem [['DateOfSampleCollection']] [con],
       y = summaryDataStem [['meanSugar']] [con], lty = 1, 
       lwd = 2, col = tColours [['colour']] [1])

# plot mean and standard error of soluble starch concentrations for chilled trees
con <- summaryDataStem [['treatment']] == 5
polygon (x = c (summaryDataStem [['DateOfSampleCollection']] [con], 
                rev (summaryDataStem [['DateOfSampleCollection']] [con])),
         y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con],  
                rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
         col = addOpacity (tColours [['colour']] [5], 0.3), lty = 0)
lines (x = summaryDataStem [['DateOfSampleCollection']] [con],
       y = summaryDataStem [['meanSugar']] [con], lty = 2, 
       lwd = 2, col = tColours [['colour']] [5])

# adda axes
#--------------------------------------------------------------------------------------
axis (side = 1, labels = c ('May','Jun','Jul','Aug','Sep','Oct','Nov'),
      at = c (as_datetime ('2018-05-01'), as_datetime ('2018-06-01'), as_datetime ('2018-07-01'), 
              as_datetime ('2018-08-01'), as_datetime ('2018-09-01'), as_datetime ('2018-10-01'), 
              as_datetime ('2018-11-01')))
axis (side = 2, las = 1, at = seq (0, 1.5, 0.5))

# add critical dates
#--------------------------------------------------------------------------------------
criticalDates (group = 5, asDate = FALSE)

dev.off ()

# plot root starch concentration by treatment
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2018ChillingStarchConcentrationsRoots.png', 
     width = 400, height = 600)
par (mar = c (5, 5, 1, 1))
con <- summaryDataStem [['treatment']] == 1
plot (x = summaryDataStem [['DateOfSampleCollection']] [con],
      y = summaryDataStem [['meanStarch']] [con], 
      typ = 'l', xlab = '', ylab = 'Root starch concentration (% dry weight)', las = 1,
      ylim = c (0, 0.5), col = 'white', axes = FALSE)

# plot mean and standard error of soluble starch concentrations for control trees
con <- summaryDataStem [['treatment']] == 1
polygon (x = c (summaryDataStem [['DateOfSampleCollection']] [con], 
                rev (summaryDataStem [['DateOfSampleCollection']] [con])),
         y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con],  
                rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
         col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
lines (x = summaryDataStem [['DateOfSampleCollection']] [con],
       y = summaryDataStem [['meanStarch']] [con], lty = 1, 
       lwd = 2, col = tColours [['colour']] [1])

# plot mean and standard error of soluble starch concentrations for chilled trees
con <- summaryDataStem [['treatment']] == 5
polygon (x = c (summaryDataStem [['DateOfSampleCollection']] [con], 
                rev (summaryDataStem [['DateOfSampleCollection']] [con])),
         y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con],  
                rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
         col = addOpacity (tColours [['colour']] [5], 0.3), lty = 0)
lines (x = summaryDataStem [['DateOfSampleCollection']] [con],
       y = summaryDataStem [['meanStarch']] [con], lty = 2, 
       lwd = 2, col = tColours [['colour']] [5])

# adda axes
#--------------------------------------------------------------------------------------
axis (side = 1, labels = c ('May','Jun','Jul','Aug','Sep','Oct','Nov'),
      at = c (as_datetime ('2018-05-01'), as_datetime ('2018-06-01'), as_datetime ('2018-07-01'), 
              as_datetime ('2018-08-01'), as_datetime ('2018-09-01'), as_datetime ('2018-10-01'), 
              as_datetime ('2018-11-01')))
axis (side = 2, las = 1, at = seq (0, 0.5, 0.1))

# add critical dates
#--------------------------------------------------------------------------------------
criticalDates (group = 5, asDate = FALSE)

# add legend
#--------------------------------------------------------------------------------------
legend (x = as_datetime ('2018-05-01'), y = 0.2, box.lty = 0, bg = 'transparent', 
        legend = c ('control', 'chilled'), lty = 1:2, lwd = 2, 
        col = tColours [['colour']] [c (1, 5)])
dev.off ()

#========================================================================================
