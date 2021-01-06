# Script to collate the extracted tangential band statistics (20 mu) 

# Load dependencies
library (ggplot2)
library (plyr)

dir <- '/media/tim/dataDisk/PlantGrowth/data/microcores/woodAnatomy/Exp2018/ROXAS/'
setwd (dir)

# Collate data from each radial wood piece
micronband20_file <- grep ("IntraannualProfiles_median_20mu", list.files (path = dir), value = TRUE)  #find the summary cell files
ALL <- NULL
for (i in c (1:length (micronband20_file)) []) {
  print (micronband20_file [i])
  micron20_profile <- read.table (micronband20_file [i], header = T) 
  ALL <- rbind (ALL, micron20_profile)
}
  
# Add tree, plot and position columns 
#head (ALL)
ALL$TREE <- as.numeric (substr (ALL$WOODID, 1, 2))
ALL$PLOT <- ALL$TREE
ALL$PLOT [which (ALL$TREE > 10 & ALL$TREE <= 15)] <- 1
ALL$PLOT [which (ALL$TREE >  5 & ALL$TREE <= 10)] <- 4
ALL$PLOT [which (ALL$TREE >= 1 & ALL$TREE <=  5)] <- 5
ALL$POS <- substr (ALL$WOODID, 4, nchar (as.character (ALL$WOODID)))

# Write single output file
write.table (ALL, "20muband_ALL.txt",  row.names = FALSE)
