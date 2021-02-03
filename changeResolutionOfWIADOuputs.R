# change resolution of the of WIAD output files and change old versions from TRIAD to WIAD
library ('jsonlite')
inputFolder <- '/media/tim/dataDisk/PlantGrowth/data/microcores/woodAnatomy/Exp2018/ringWidthTRIAD/'
outputFolder <- '/media/tim/dataDisk/PlantGrowth/data/microcores/woodAnatomy/Exp2018/ringWidthTRIAD/new/'
jsonFiles <- list.files (inputFolder, pattern = '.json')

# Loop over files
for (j in 1:length (jsonFiles)) {
  
  print (jsonFiles [j])
  print (j)
  
  # read file
  labels <- read_json (paste0 (inputFolder, jsonFiles [j]))
  
  # check if resolution has already been changed
  if (labels$sampleDPI == 38100) {
    labels$sampleDPI [[1]] <- 57596 
  } else if (labels$sampleDPI != 57596) {
    print (paste (j, jsonFiles [j], labels$sampleDPI))
    stop ('Error: Resolution is wrong!')
  }
  
  # Change old versions
  if (labels$version  != "Generated with the Wood Image Analysis and Database (WIAD) v0.0.1") {
    labels$version [[1]] <- "Generated with the Wood Image Analysis and Database (WIAD) v0.0.1"
  }
  
  # update marker table from json file 
  growth_table <- data.table::rbindlist (labels$markerData, 
                                        fill = TRUE)
  
  # recalculate growth in the markerTable
  #------------------------------------------------------------------------------------
  growth_table [, growth := pixels / as.numeric (labels$sampleDPI) * 25.4 * 1000]
  
  # update markerData 
  labels$markerData <- growth_table
  
  # write new json file in output folder
  file <- paste0 (outputFolder,jsonFiles [j])
  labels %>% 
    toJSON (null = 'null', auto_unbox = TRUE) %>%
    write_lines (file)
}

