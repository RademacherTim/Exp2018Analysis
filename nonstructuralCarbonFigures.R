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
stemData <- stemData %>% 
  group_by (tree, sampleHeight) %>% 
  arrange (date)
rootData <- rootData %>% 
  group_by (tree) %>% 
  arrange (date)

# plot the 2018 stem sugar concentration data by tree
#----------------------------------------------------------------------------------------
layout (matrix (1:15, nrow = 3, byrow = TRUE))
for (t in 1:15) {
  # plot below treatment
  #--------------------------------------------------------------------------------------
  con <- stemData [['tree']] == t & stemData [['sampleHeight']] == 1
  plot (x = stemData [['date']] [con],
        y = stemData [['sugar']] [con], typ = 'l',
        xlab = 'date', ylab = 'sugar concentration (%DW)', las = 1,
        ylim = c (0, 1.75), lty = 3, lwd = 2,
        col = tColours [['colour']] [unique (stemData [['treatment']] [con])])
  # add in between the treatments
  #--------------------------------------------------------------------------------------
  con <- stemData [['tree']] == t & stemData [['sampleHeight']] == 2
  lines (x = stemData [['date']] [con],
         y = stemData [['sugar']] [con], lty = 2, lwd = 2,
         col = tColours [['colour']] [unique (stemData [['treatment']] [con])])
  # add in above the treatments
  #--------------------------------------------------------------------------------------
  con <- stemData [['tree']] == t & stemData [['sampleHeight']] == 3
  lines (x = stemData [['date']] [con],
         y = stemData [['sugar']] [con], lty = 1, lwd = 2,
         col = tColours [['colour']] [unique (stemData [['treatment']] [con])])
  
  # add a tree panel identifier
  #--------------------------------------------------------------------------------------
  text (x = as_date ('2018-05-25'), y = 1.55, labels = t, cex = 2)
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  criticalDates (group = unique (stemData [['treatment']] [con]), asDate = FALSE)
  
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
  con <- stemData [['tree']] == t & stemData [['sampleHeight']] == 1
  #--------------------------------------------------------------------------------------
  plot (x = stemData [['date']] [con],
        y = stemData [['starch']] [con], typ = 'l',
        xlab = 'date', ylab = 'starch concentration (%DW)', las = 1,
        ylim = c (0, 0.8), lty = 3, lwd = 2,
        col = tColours [['colour']] [unique (stemData [['treatment']] [con])])
  
  # add in between the treatments
  #--------------------------------------------------------------------------------------
  con <- stemData [['tree']] == t & stemData [['sampleHeight']] == 2
  lines (x = stemData [['date']] [con],
         y = stemData [['starch']] [con], lty = 2, lwd = 2,
         col = tColours [['colour']] [unique (stemData [['treatment']] [con])])
  
  # add in above the treatments
  #--------------------------------------------------------------------------------------
  con <- stemData [['tree']] == t & stemData [['sampleHeight']] == 3
  lines (x = stemData [['date']] [con],
         y = stemData [['starch']] [con], lty = 1, lwd = 2,
         col = tColours [['colour']] [unique (stemData [['treatment']] [con])])
  
  # add a tree panel identifier
  #--------------------------------------------------------------------------------------
  text (x = as_date ('2018-05-25'), y = 0.75, labels = t, cex = 2)
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  criticalDates (group = unique (stemData [['treatment']] [con]), asDate = FALSE)
  
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
  con <- rootData [['tree']] == t
  plot (x = rootData [['date']] [con],
        y = rootData [['sugar']] [con], typ = 'l',
        xlab = 'date', ylab = 'sugar concentration (%DW)', las = 1,
        ylim = c (0, 1.75), lwd = 2,
        col = tColours [['colour']] [unique (rootData [['treatment']] [con])])

  # add a tree panel identifier
  #--------------------------------------------------------------------------------------
  text (x = as_date ('2018-05-25'), y = 1.55, labels = t, cex = 2)
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  criticalDates (group = unique (rootData [['treatment']] [con]), asDate = FALSE)
  
}

# plot the 2018 root starch concentration data by tree
#----------------------------------------------------------------------------------------
layout (matrix (1:15, nrow = 3, byrow = TRUE))
for (t in 1:15) {
  # plot root sugar concentation
  #--------------------------------------------------------------------------------------
  con <- rootData [['tree']] == t
  plot (x = rootData [['date']] [con],
        y = rootData [['starch']] [con], typ = 'l',
        xlab = 'date', ylab = 'starch concentration (%DW)', las = 1,
        ylim = c (0, 0.8), lwd = 2,
        col = tColours [['colour']] [unique (rootData [['treatment']] [con])])
  
  # add a tree panel identifier
  #--------------------------------------------------------------------------------------
  text (x = as_date ('2018-05-25'), y = 1.55, labels = t, cex = 2)
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  criticalDates (group = unique (rootData [['treatment']] [con]), asDate = FALSE)
  
}

# summarise the stem data by treatment and sampling height
#----------------------------------------------------------------------------------------
summaryDataStem <- stemData %>% 
  group_by (date, treatment, sampleHeight) %>% 
  summarise (meanSugar = mean (sugar), 
             sdSugar   = sd (sugar), 
             seSugar   = se (sugar),
             meanStarch = mean (starch), 
             sdStarch   = sd (starch), 
             seStarch   = se (starch),
             .groups = 'keep')

# plot the 2018 stem sugar concentration data by treatment
#----------------------------------------------------------------------------------------
layout (matrix (1:3, nrow = 1, byrow = TRUE))
for (t in c (1, 4, 5)) {
  par (mar = c (5, 5, 1, 1))
  con <- summaryDataStem [['treatment']] == 1 &
    summaryDataStem [['sampleHeight']] == 1 &
    !is.na (summaryDataStem [['meanSugar']])
  plot (x = summaryDataStem [['date']] [con],
        y = summaryDataStem [['meanSugar']] [con], 
        typ = 'l', xlab = 'date', ylab = 'wood sugar concentration (% dry weight)', las = 1,
        ylim = c (0, 1.5), col = 'white')
  
  for (h in 1:3) {
    con <- summaryDataStem [['treatment']] == t &
      summaryDataStem [['sampleHeight']] == h &
      !is.na (summaryDataStem [['meanSugar']])
    polygon (x = c (summaryDataStem [['date']] [con], 
                    rev (summaryDataStem [['date']] [con])),
             y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con], 
                    rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
             col = addOpacity (tColours [['colour']] [t], 0.3), lty = 0)
    lines (x = summaryDataStem [['date']] [con],
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
  plot (x = summaryDataStem [['date']] [con],
        y = summaryDataStem [['meanStarch']] [con], 
        typ = 'l', xlab = 'date', ylab = 'wood starch concentration (% dry weight)', las = 1,
        ylim = c (0, 0.6), col = 'white')
  
  for (h in 1:3) {
    con <- summaryDataStem [['treatment']] == t &
      summaryDataStem [['sampleHeight']] == h &
      !is.na (summaryDataStem [['meanStarch']])
    polygon (x = c (summaryDataStem [['date']] [con], 
                    rev (summaryDataStem [['date']] [con])),
             y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con], 
                    rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
             col = addOpacity (tColours [['colour']] [t], 0.3), lty = 0)
    lines (x = summaryDataStem [['date']] [con],
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
png (filename = './fig/Exp2018ChillingSolubleSugarConcentrationsWood.png', width = 400, 
     height = 640)
layout (matrix (1:3, nrow = 3, byrow = TRUE), height = c (1, 1, 1.15))
for (h in 3:1) {
  
  # determine margins
  if (h == 1) {
    par (mar = c (4, 5, 1, 1))
  } else {
    par (mar = c (1, 5, 1, 1))
  }
  con <- summaryDataStem [['treatment']] == 1 &
    summaryDataStem [['sampleHeight']] == h &
    !is.na (summaryDataStem [['meanSugar']])
  plot (x = summaryDataStem [['date']] [con],
        y = summaryDataStem [['meanSugar']] [con], 
        typ = 'l', xlab = '', ylab = 'Wood sugar concentration (% dry weight)', las = 1,
        xlim = c (as_date ('2018-05-01'), as_date ('2018-11-15')), ylim = c (0, 1.5), 
        col = 'white', axes = FALSE)
  
  # plot mean and standard error of soluble sugar concentrations for control trees
  con <- summaryDataStem [['treatment']] == 1 &
      summaryDataStem [['sampleHeight']] == h &
      !is.na (summaryDataStem [['meanSugar']])
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con],  
                  rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con],
         y = summaryDataStem [['meanSugar']] [con], lty = 1, 
         lwd = 2, col = tColours [['colour']] [1])
  
  
  # plot mean and standard error of soluble sugar concentrations for chilled trees
  con <- summaryDataStem [['treatment']] == 5 &
    summaryDataStem [['sampleHeight']] == h &
    !is.na (summaryDataStem [['meanSugar']])
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con],  
                  rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [5], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con],
         y = summaryDataStem [['meanSugar']] [con], lty = 2, 
         lwd = 2, col = tColours [['colour']] [5])
  
  # add x-axis
  #--------------------------------------------------------------------------------------
  if (h == 1) {
    axis (side = 1, labels = c ('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov', 'Dec'),
          at = c (as_date ('2018-04-01'), as_date ('2018-05-01'), as_date ('2018-06-01'), 
                  as_date ('2018-07-01'), as_date ('2018-08-01'), as_date ('2018-09-01'), 
                  as_date ('2018-10-01'), as_date ('2018-11-01'), as_date ('2018-12-01')))
  
    # add tick marks for sampling dates
    points (x = unique (summaryDataStem [['date']]),
            y = rep (-0.07, length (unique (summaryDataStem [['date']]))),
             pch = 3, cex = 1.5, col = '#e37222', lwd = 3)
    
  } else {
    axis (side = 1, labels = rep ('', 9),
          at = c (as_date ('2018-04-01'), as_date ('2018-05-01'), as_date ('2018-06-01'), 
                  as_date ('2018-07-01'), as_date ('2018-08-01'), as_date ('2018-09-01'), 
                  as_date ('2018-10-01'), as_date ('2018-11-01'), as_date ('2018-12-01')))
  }
  
  # add y-axis
  axis (side = 2, las = 1, at = seq (0, 1.5, 0.4))
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  criticalDates (group = t, asDate = TRUE)
  
  # add legend
  #--------------------------------------------------------------------------------------
  if (h == 1) {
    legend (x = as_date ('2018-05-01'), y = 1.2, box.lty = 0, bg = 'transparent', 
            legend = c ('control', 'chilled'), lty = 1:2, lwd = 2, 
            col = tColours [['colour']] [c (1, 5)])
  }
}   
dev.off ()

# plot stem xylem starch concentration by treatment and sampling height
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2018ChillingStarchConcentrationsWood.png', width = 400, height = 640)
layout (matrix (1:3, nrow = 3, byrow = TRUE), height = c (1, 1, 1.15))
for (h in 3:1) {
  
  # determine margins
  if (h == 1) {
    par (mar = c (4, 5, 1, 1))
  } else {
    par (mar = c (1, 5, 1, 1))
  }
  con <- summaryDataStem [['treatment']] == 1 &
    summaryDataStem [['sampleHeight']] == h &
    !is.na (summaryDataStem [['meanStarch']])
  plot (x = summaryDataStem [['date']] [con],
        y = summaryDataStem [['meanStarch']] [con], 
        typ = 'l', xlab = '', ylab = 'Wood starch concentration (% dry weight)', las = 1,
        xlim = c (as_date ('2018-05-01'), as_date ('2018-11-15')), ylim = c (0, 0.5),
        col = 'white', axes = FALSE)
  
  # plot mean and standard error of soluble starch concentrations for control trees
  con <- summaryDataStem [['treatment']] == 1 &
    summaryDataStem [['sampleHeight']] == h &
    !is.na (summaryDataStem [['meanStarch']])
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con],  
                  rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con],
         y = summaryDataStem [['meanStarch']] [con], lty = 1, 
         lwd = 2, col = tColours [['colour']] [1])
  
  
  # plot mean and standard error of soluble starch concentrations for chilled trees
  con <- summaryDataStem [['treatment']] == 5 &
    summaryDataStem [['sampleHeight']] == h &
    !is.na (summaryDataStem [['meanStarch']])
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con],  
                  rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [5], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con],
         y = summaryDataStem [['meanStarch']] [con], lty = 2, 
         lwd = 2, col = tColours [['colour']] [5])
  
  # add x-axis
  #--------------------------------------------------------------------------------------
  if (h == 1) {
    axis (side = 1, labels = c ('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov', 'Dec'),
          at = c (as_date ('2018-04-01'), as_date ('2018-05-01'), as_date ('2018-06-01'), 
                  as_date ('2018-07-01'), as_date ('2018-08-01'), as_date ('2018-09-01'), 
                  as_date ('2018-10-01'), as_date ('2018-11-01'), as_date ('2018-12-01')))
    
    # add tick marks for sampling dates
    points (x = unique (summaryDataStem [['date']]),
            y = rep (-0.02, length (unique (summaryDataStem [['date']]))),
            pch = 3, cex = 1.5, col = '#e37222', lwd = 3)
    
  } else {
    axis (side = 1, labels = rep ('', 9),
          at = c (as_date ('2018-04-01'), as_date ('2018-05-01'), as_date ('2018-06-01'), 
                  as_date ('2018-07-01'), as_date ('2018-08-01'), as_date ('2018-09-01'), 
                  as_date ('2018-10-01'), as_date ('2018-11-01'), as_date ('2018-12-01')))
  }
  
  # add y-axis
  axis (side = 2, las = 1, at = seq (0, 0.5, 0.1))
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  criticalDates (group = 5, asDate = TRUE)
  
  # add legend
  #--------------------------------------------------------------------------------------
  if (h == 1) {
    legend (x = as_date ('2018-05-01'), y = 0.2, box.lty = 0, bg = 'transparent', 
            legend = c ('control', 'chilled'), lty = 1:2, lwd = 2, 
            col = tColours [['colour']] [c (1, 5)])
  }
}   
dev.off ()

# summarise the root data by treatment 
#----------------------------------------------------------------------------------------
summaryDataRoots <- rootData %>% 
  group_by (date, treatment) %>% 
  summarise (meanSugar = mean (sugar), 
             sdSugar   = sd (sugar), 
             seSugar   = se (sugar),
             meanStarch = mean (starch), 
             sdStarch   = sd (starch), 
             seStarch   = se (starch),
             .groups = 'keep')

# plot root starch concentration by treatment
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2018ChillingSolubleSugarConcentrationsRoots.png', 
     width = 400, height = 200)
par (mfrow = c (1, 1))
par (mar = c (3, 5, 1, 1))
con <- summaryDataRoots [['treatment']] == 1
plot (x = summaryDataRoots [['date']] [con],
      y = summaryDataRoots [['meanSugar']] [con], 
      typ = 'l', xlab = '', ylab = 'Root soluble sugar concentration (% dry weight)', las = 1,
      xlim = c (as_date ('2018-05-01'), as_date ('2018-11-15')), ylim = c (0, 1.2), 
      col = 'white', axes = FALSE)

# plot mean and standard error of soluble starch concentrations for control trees
con <- summaryDataRoots [['treatment']] == 1
polygon (x = c (summaryDataRoots [['date']] [con], 
                rev (summaryDataRoots [['date']] [con])),
         y = c (summaryDataRoots [['meanSugar']] [con] - summaryDataRoots [['seSugar']] [con],  
                rev (summaryDataRoots [['meanSugar']] [con] + summaryDataRoots [['seSugar']] [con])),
         col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
lines (x = summaryDataRoots [['date']] [con],
       y = summaryDataRoots [['meanSugar']] [con], lty = 1, 
       lwd = 2, col = tColours [['colour']] [1])

# plot mean and standard error of soluble starch concentrations for chilled trees
con <- summaryDataRoots [['treatment']] == 5
polygon (x = c (summaryDataRoots [['date']] [con], 
                rev (summaryDataRoots [['date']] [con])),
         y = c (summaryDataRoots [['meanSugar']] [con] - summaryDataRoots [['seSugar']] [con],  
                rev (summaryDataRoots [['meanSugar']] [con] + summaryDataRoots [['seSugar']] [con])),
         col = addOpacity (tColours [['colour']] [5], 0.3), lty = 0)
lines (x = summaryDataRoots [['date']] [con],
       y = summaryDataRoots [['meanSugar']] [con], lty = 2, 
       lwd = 2, col = tColours [['colour']] [5])

# add axes
#--------------------------------------------------------------------------------------
axis (side = 1, labels = c ('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'),
      at = c (as_date ('2018-04-01'), as_date ('2018-05-01'), as_date ('2018-06-01'), as_date ('2018-07-01'), 
              as_date ('2018-08-01'), as_date ('2018-09-01'), as_date ('2018-10-01'), 
              as_date ('2018-11-01'), as_date ('2018-12-01')))
axis (side = 2, las = 1, at = seq (0, 1.2, 0.4))

# add tick marks with sampling dates
#--------------------------------------------------------------------------------------
points (x = unique (summaryDataRoots [['date']]),
        y = rep (-0.05, length (unique (summaryDataRoots [['date']]))),
        pch = 3, cex = 1.5, col = '#e37222', lwd = 3)

# add critical dates
#--------------------------------------------------------------------------------------
criticalDates (group = 5, asDate = TRUE)

# add legend
#--------------------------------------------------------------------------------------
legend (x = as_date ('2018-05-01'), y = 1.2, box.lty = 0, bg = 'transparent', 
        legend = c ('control', 'chilled'), lty = 1:2, lwd = 2, 
        col = tColours [['colour']] [c (1, 5)])
dev.off ()

# plot root starch concentration by treatment
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2018ChillingStarchConcentrationsRoots.png', 
     width = 400, height = 240)
par (mar = c (3, 5, 1, 1))
con <- summaryDataRoots [['treatment']] == 1
plot (x = summaryDataRoots [['date']] [con],
      y = summaryDataRoots [['meanStarch']] [con], 
      typ = 'l', xlab = '', ylab = 'Root starch concentration (% dry weight)', las = 1,
      xlim = c (as_date ('2018-05-01'), as_date ('2018-11-15')), ylim = c (0, 0.5), 
      col = 'white', axes = FALSE)

# plot mean and standard error of soluble starch concentrations for control trees
con <- summaryDataRoots [['treatment']] == 1
polygon (x = c (summaryDataRoots [['date']] [con], 
                rev (summaryDataRoots [['date']] [con])),
         y = c (summaryDataRoots [['meanStarch']] [con] - summaryDataRoots [['seStarch']] [con],  
                rev (summaryDataRoots [['meanStarch']] [con] + summaryDataRoots [['seStarch']] [con])),
         col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
lines (x = summaryDataRoots [['date']] [con],
       y = summaryDataRoots [['meanStarch']] [con], lty = 1, 
       lwd = 2, col = tColours [['colour']] [1])

# plot mean and standard error of soluble starch concentrations for chilled trees
con <- summaryDataRoots [['treatment']] == 5
polygon (x = c (summaryDataRoots [['date']] [con], 
                rev (summaryDataRoots [['date']] [con])),
         y = c (summaryDataRoots [['meanStarch']] [con] - summaryDataRoots [['seStarch']] [con],  
                rev (summaryDataRoots [['meanStarch']] [con] + summaryDataRoots [['seStarch']] [con])),
         col = addOpacity (tColours [['colour']] [5], 0.3), lty = 0)
lines (x = summaryDataRoots [['date']] [con],
       y = summaryDataRoots [['meanStarch']] [con], lty = 2, 
       lwd = 2, col = tColours [['colour']] [5])

# adda axes
#--------------------------------------------------------------------------------------
axis (side = 1, labels = c ('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'),
      at = c (as_date ('2018-04-01'), as_date ('2018-05-01'), as_date ('2018-06-01'), 
              as_date ('2018-07-01'), as_date ('2018-08-01'), as_date ('2018-09-01'), 
              as_date ('2018-10-01'), as_date ('2018-11-01'), as_date ('2018-12-01')))
axis (side = 2, las = 1, at = seq (0, 0.5, 0.1))

# add tick marks with sampling dates
#--------------------------------------------------------------------------------------
points (x = unique (summaryDataRoots [['date']]),
        y = rep (-0.025, length (unique (summaryDataRoots [['date']]))),
        pch = 3, cex = 1.5, col = '#e37222', lwd = 3)


# add critical dates
#--------------------------------------------------------------------------------------
criticalDates (group = 5, asDate = TRUE)

# add legend
#--------------------------------------------------------------------------------------
legend (x = as_date ('2018-05-01'), y = 0.45, box.lty = 0, bg = 'transparent', 
        legend = c ('control', 'chilled'), lty = 1:2, lwd = 2, 
        col = tColours [['colour']] [c (1, 5)])
dev.off ()

#========================================================================================

