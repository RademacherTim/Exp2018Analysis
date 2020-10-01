#========================================================================================
# script to plot nonstructural carbon data from the 2018 chilling experiment on white 
# pine at Harvard Forest. 
#----------------------------------------------------------------------------------------

# source colour schemes and ploting functions
#----------------------------------------------------------------------------------------
source ('./plotingFunctions.R')

# source processed data
#----------------------------------------------------------------------------------------
source ('./readNSCData.R') 

# sort the stemData2018 tibble to makes lines in graphs look decent
#----------------------------------------------------------------------------------------
stemData2018 <- stemData2018 %>% 
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

# summarise the data by treatment and sampling height
#----------------------------------------------------------------------------------------
summaryDataStem <- stemData2018 %>% 
  group_by (DateOfSampleCollection, treatment, sampleHeight) %>% 
  summarise (meanSugar = mean (ConcentrationSugarPerDW), 
             sdSugar   = sd (ConcentrationSugarPerDW), 
             seSugar   = se (ConcentrationSugarPerDW),
             meanStarch = mean (ConcentrationStarchPerDW), 
             sdStarch   = sd (ConcentrationStarchPerDW), 
             seStarch   = se (ConcentrationStarchPerDW))

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
#========================================================================================
