#========================================================================================
# Script to plot repeat measurements of root samples for quality control. This script 
# depends on the intermediaryy rootData2018 variable from the 
# readNonstructuralCarbonData.R script. 
#----------------------------------------------------------------------------------------
tmp <- rootData2018 %>% 
  filter ((treeID == 1  & DateOfSampleCollection == as_datetime ('2018-06-13')) |
          (treeID == 5  & DateOfSampleCollection == as_datetime ('2018-07-05')) |
          (treeID == 9  & DateOfSampleCollection == as_datetime ('2018-08-02')) |
          (treeID == 11 & DateOfSampleCollection == as_datetime ('2018-08-29')) |
          (treeID == 15 & DateOfSampleCollection == as_datetime ('2018-09-27'))) %>% 
  select (ConcentrationSugarPerDW, ConcentrationStarchPerDW)

# get the values for each batch
tmp1 <- tmp [seq (0, 10, by = 2), ]
tmp2 <- tmp [seq (1, 10, by = 2), ]

# plot values of one batch against the other
par (mar = c (5, 5, 1, 1))
plot (tmp1 %>% select (ConcentrationSugarPerDW) %>% unlist (), 
      tmp2 %>% select (ConcentrationSugarPerDW) %>% unlist (),
      xlim = c (0, 2.0), ylim = c (0, 2),
      xlab = ' Batch 1 root concentration (% DW)',
      ylab = ' Batch 2 root concentration (% DW)', 
      pch = 19, col = '#106470', las = 1)
abline (a = 0, b = 1, col = '#666666')
points (tmp1 %>% select (ConcentrationStarchPerDW) %>% unlist (), 
        tmp2 %>% select (ConcentrationStarchPerDW) %>% unlist (),
        pch = 17, col = '#00B1C1')
legend (x = 0, y = 2, 
        legend = c ('sugar','starch'), pch = c (19, 17), col = c ('#106470','#00B1C1'), 
        box.lty = 0)
#========================================================================================