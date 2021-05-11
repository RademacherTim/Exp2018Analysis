#========================================================================================
# Script to read and process nonstructural carbon data from colorimetric assays for the 
# 2018 chilling experiment on white pine at Harvard Forest.
# Processing is done using the NSCProcessR package.
#----------------------------------------------------------------------------------------

# get original working directory and switch to directory with NSCprocessR package 
#----------------------------------------------------------------------------------------
originalDir <- getwd ()
packageDir <- '/home/tim/projects/PlantGrowth/nonstructuralCarbon/' 
setwd (packageDir)

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('dplyr::filter')) library ('tidyverse')
if (!existsFunction ('NSCprocessR::processNSC')) library ('NSCprocessR')
if (!existsFunction ('lubridate::month')) library ('lubridate')
if (!existsFunction ('write.xlsx')) library ('openxlsx')

# change working directory
#----------------------------------------------------------------------------------------
setwd (originalDir)

# read data from csv files with new column names
#----------------------------------------------------------------------------------------
temp1 <- read_csv (file = './data/nonstructuralCarbonData_LCS_HF_Exp2018.csv', 
                   col_types = cols (
                     rc.lab.number             = col_character (),
                     sample.id                 = col_character (),
                     tissue                    = col_character (),
                     batch.id                  = col_integer (),
                     sample.location           = col_character (),
                     date.of.sample.collection = col_date (format = '%d-%b-%y'), 
                     date.of.sugar.analysis    = col_date (format = '%d-%b-%y'), 
                     date.of.starch.analysis   = col_date (format = '%d-%b-%y'), 
                     mass.of.empty.tube        = col_double (), 
                     mass.of.tube.and.sample   = col_double (), 
                     absorbance490.1           = col_double (),
                     absorbance490.2           = col_double (),
                     absorbance490.blank       = col_double (), 
                     absorbance525.1           = col_double (),
                     absorbance525.2           = col_double (), 
                     dilution.factor.sugar     = col_integer (),
                     volume.sugar              = col_integer (), 
                     dilution.factor.starch    = col_integer (), 
                     volume.starch             = col_integer (), 
                     comments                  = col_character () 
                   ))

temp2 <- read_csv (file = './data/nonstructuralCarbonData_wood_HF_Exp2018.csv', 
                   col_types = cols (
                     rc.lab.number             = col_character (),
                     sample.id                 = col_character (),
                     tissue                    = col_character (),
                     batch.id                  = col_integer (),
                     sample.location           = col_character (),
                     date.of.sample.collection = col_date (format = '%d-%b-%y'), 
                     date.of.sugar.analysis    = col_date (format = '%d-%b-%y'), 
                     date.of.starch.analysis   = col_date (format = '%d-%b-%y'), 
                     mass.of.empty.tube        = col_double (), 
                     mass.of.tube.and.sample   = col_double (), 
                     absorbance490.1           = col_double (),
                     absorbance490.2           = col_double (),
                     absorbance490.blank       = col_double (), 
                     absorbance525.1           = col_double (),
                     absorbance525.2           = col_double (), 
                     dilution.factor.sugar     = col_integer (),
                     volume.sugar              = col_integer (), 
                     dilution.factor.starch    = col_integer (), 
                     volume.starch             = col_integer (), 
                     comments                  = col_character () 
                   ))

temp3 <- read_csv (file = './data/nonstructuralCarbonData_roots_HF_Exp2018.csv', 
                   col_types = cols (
                     rc.lab.number             = col_character (),
                     sample.id                 = col_character (),
                     tissue                    = col_character (),
                     batch.id                  = col_integer (),
                     sample.location           = col_character (),
                     date.of.sample.collection = col_date (format = '%d-%b-%y'), 
                     date.of.sugar.analysis    = col_date (format = '%d-%b-%y'), 
                     date.of.starch.analysis   = col_date (format = '%d-%b-%y'), 
                     mass.of.empty.tube        = col_double (), 
                     mass.of.tube.and.sample   = col_double (), 
                     absorbance490.1           = col_double (),
                     absorbance490.2           = col_double (),
                     absorbance490.blank       = col_double (), 
                     absorbance525.1           = col_double (),
                     absorbance525.2           = col_double (), 
                     dilution.factor.sugar     = col_integer (),
                     volume.sugar              = col_integer (), 
                     dilution.factor.starch    = col_integer (), 
                     volume.starch             = col_integer (), 
                     comments                  = col_character () 
                   ))

# change column names to the NSCprocessR standards using CamelCasing and underscores
#----------------------------------------------------------------------------------------
names (temp1) <- c ('RCLabNumber','SampleID','Tissue','BatchID','SampleLocation',
                    'DateOfSampleCollection','DateOfSugarAnalysis','DateOfStarchAnalysis',
                    'MassOfEmptyTube','MassOfTubeAndSample','Absorbance490_1',
                    'Absorbance490_2','Absorbance490_Blank','Absorbance525_1',
                    'Absorbance525_2','DilutionFactorSugar','VolumeSugar',
                    'DilutionFactorStarch','VolumeStarch','Comments')
names (temp2) <- c ('RCLabNumber','SampleID','Tissue','BatchID','SampleLocation',
                    'DateOfSampleCollection','DateOfSugarAnalysis','DateOfStarchAnalysis',
                    'MassOfEmptyTube','MassOfTubeAndSample','Absorbance490_1',
                    'Absorbance490_2','Absorbance490_Blank','Absorbance525_1',
                    'Absorbance525_2','DilutionFactorSugar','VolumeSugar',
                    'DilutionFactorStarch','VolumeStarch','Comments')
names (temp3) <- c ('RCLabNumber','SampleID','Tissue','BatchID','SampleLocation',
                    'DateOfSampleCollection','DateOfSugarAnalysis','DateOfStarchAnalysis',
                    'MassOfEmptyTube','MassOfTubeAndSample','Absorbance490_1',
                    'Absorbance490_2','Absorbance490_Blank','Absorbance525_1',
                    'Absorbance525_2','DilutionFactorSugar','VolumeSugar',
                    'DilutionFactorStarch','VolumeStarch','Comments')

# write data to .xlsx format for NSCprocessR readRawNSCData () function
#----------------------------------------------------------------------------------------
openxlsx::write.xlsx (temp1, file = './data/nonstructuralCarbonData_LCS_HF_Exp2018.xlsx')
openxlsx::write.xlsx (temp2, file = './data/nonstructuralCarbonData_wood_HF_Exp2018.xlsx')
openxlsx::write.xlsx (temp3, file = './data/nonstructuralCarbonData_roots_HF_Exp2018.xlsx')

# read data from the spreadsheets
#----------------------------------------------------------------------------------------
rawDataWoodLCS2018 <- readRawNSCData (fileDir = getwd (),
                                      fileName = './data/nonstructuralCarbonData_LCS_HF_Exp2018.xlsx',
                                      workSheet = 'Sheet 1',
                                      IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))
rawDataWood2018  <- readRawNSCData (fileDir = getwd (),
                                    fileName = './data/nonstructuralCarbonData_wood_HF_Exp2018.xlsx',
                                    workSheet = 'Sheet 1',
                                    IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))
rawDataRoots2018  <- readRawNSCData (fileDir = getwd (),
                                     fileName = './data/nonstructuralCarbonData_roots_HF_Exp2018.xlsx',
                                     workSheet = 'Sheet 1',
                                     IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))

#----------------------------------------------------------------------------------------
# process the raw data taking into account different calibration curve for the wood 
# samples and the LCS Oak and LCS Potato
#----------------------------------------------------------------------------------------
processedDataWoodLCS2018 <- processNSCs (rawData = rawDataWoodLCS2018,
                                         cvLimitSample = 0.25,
                                         cvLimitTube = 0.25,
                                         LCS = 'Oak')

# Compile a list of unique batches and dates for starch extractions
#----------------------------------------------------------------------------------------
batches <- unique (rawDataWood2018 [['BatchID']])
for (batch in batches) {
  dates <- unique (rawDataWood2018 [['DateOfStarchAnalysis']] [rawDataWood2018 [['BatchID']] == batch])
  prescribedStarch <- unique (processedDataWoodLCS2018 [['MeanStarchRecovery']] [processedDataWoodLCS2018 [['BatchID']] == batch])
  if (batch == batches [1] & !is.na (dates [1])) {
    extractionsStarch <- tibble (batch = batch, date = dates, starchRecovery = prescribedStarch)
  } else if (!is.na (dates [1])) {
    extractionsStarch <- add_row (extractionsStarch, batch = batch, date = dates, starchRecovery = prescribedStarch)
  }
}

# Process each batch with prescribed starch recovery from the processedDataWoodLCS2018 data
#--------------------------------------------------------------------------------------
for (i in 1:dim (extractionsStarch) [1]) {
  temp <- processNSCs (rawData = rawDataWood2018,
                       cvLimitSample = 0.25,
                       cvLimitTube = 0.25,
                       LCS = '',
                       prescribedStarchRecoveryFraction = extractionsStarch [['starchRecovery']] [i])
  # Only choose the data from the appropraite batch 
  #------------------------------------------------------------------------------------
  if (i == 1) {
    processedDataWood2018 <- temp [temp [['BatchID']] == extractionsStarch [['batch']] [i] & 
                                     temp [['DateOfStarchAnalysis']] == extractionsStarch [['date']] [i], ]
  } else {
    processedDataWood2018 <- rbind (processedDataWood2018, 
                                    temp [temp [['BatchID']] == extractionsStarch [['batch']] [i] & 
                                            temp [['DateOfStarchAnalysis']] == extractionsStarch [['date']] [i], ])
  }
}
rm (rawDataWood2018, rawDataWoodLCS2018)

# process root data
#----------------------------------------------------------------------------------------
processedDataRoots2018 <- processNSCs (rawData = rawDataRoots2018,
                                       cvLimitSample = 0.25,
                                       cvLimitTube = 0.25,
                                       LCS = 'Oak')
rm (rawDataRoots2018)

# produce pdf files with calibration curves, when needed
#----------------------------------------------------------------------------------------
PLOTCAL <- FALSE
if (PLOTCAL) {
  res <- plotCalibrationCurves (data = processedDataWoodLCS2018)
  if (res != 0) print ('Error: plotCalibrationCurves () did not work.')
  res <- plotCalibrationCurves (data = processedDataWood2018)
  if (res != 0) print ('Error: plotCalibrationCurves () did not work.')
  res <- plotCalibrationCurves (data = processedDataRoots2018)
  if (res != 0) print ('Error: plotCalibrationCurves () did not work.')
  rm (res)
}
rm (PLOTCAL)

# combine all processed data into one tibble
#----------------------------------------------------------------------------------------
dataExp2018 <- rbind (processedDataWood2018, 
                      select (processedDataWoodLCS2018, -SRFHigh, -LCSOakDeviation),
                      select (processedDataRoots2018, -SRFHigh, -LCSOakDeviation))
rm (processedDataWood2018, processedDataWoodLCS2018)

# add and fill a column for sample height for stem tissue samples
#----------------------------------------------------------------------------------------
condition <- substr (dataExp2018 [['SampleID']], 1, 3) == 'Exp' & dataExp2018 [['Tissue']] != 'Root'
dataExp2018 [['sampleHeight']] [condition] <- as.numeric (substr (dataExp2018 [['SampleID']] [condition], 13, 13))

#  add and fill columns for treeID and treatment to 2018 data tibbles 
#----------------------------------------------------------------------------------------
condition <- substr (dataExp2018 [['SampleID']], 1, 3) == 'Exp'
dataExp2018 [['treeID']] [condition] <- 
  as.numeric (substr (dataExp2018 [['SampleID']] [condition], 5, 6))
dataExp2018 [['treatment']] [condition] <- dataExp2018 [['treeID']] [condition]
dataExp2018 [['treatment']] [dataExp2018 [['treatment']] >= 1  &
                               dataExp2018 [['treatment']] <= 5]  <- 5
dataExp2018 [['treatment']] [dataExp2018 [['treatment']] >= 6  &
                               dataExp2018 [['treatment']] <= 10] <- 4
dataExp2018 [['treatment']] [dataExp2018 [['treatment']] >= 11 &
                               dataExp2018 [['treatment']] <= 15] <- 1

# extract tissue-specific data, currently only stems
#----------------------------------------------------------------------------------------
stemData2018 <- dataExp2018 [substr (dataExp2018 [['Tissue']], 1, 4) == 'Stem', ]
rootData2018 <- dataExp2018 [substr (dataExp2018 [['Tissue']], 1, 4) == 'Root', ]

# get the coefficient of variation for lab controls 
#--------------------------------------------------------------------------------------
COV <- filter (dataExp2018, substr (SampleID, 1, 7) == 'LCS Oak' |
                 substr (SampleID, 1, 7) == 'LCS Pot') %>%
  group_by (SampleID) %>% 
  select (SampleID, ConcentrationSugarPerDW, ConcentrationStarchPerDW) %>% 
  summarise (covSugar  = sd (ConcentrationSugarPerDW) / mean (ConcentrationSugarPerDW),
             covStarch = sd (ConcentrationStarchPerDW) / mean (ConcentrationStarchPerDW),
             .groups = 'keep')
  
# Get the number of blanks and controls per batch
#----------------------------------------------------------------------------------------
temp <- filter (dataExp2018, SampleID == 'B' | SampleID == 'TB') %>% 
  group_by (BatchID, DateOfSugarAnalysis) %>% 
  summarise (n = sum (SampleID == 'B' | SampleID == 'TB'),
             .groups = 'keep')
# range (temp [['n']])
temp <- filter (dataExp2018, substr (SampleID, 1, 6) == 'REF100' | 
                  substr (SampleID, 1, 3) == 'LCS') %>%
  group_by (BatchID, DateOfSugarAnalysis) %>% 
  summarise (n = sum (substr (SampleID, 1, 6) == 'REF100' | 
                        substr (SampleID, 1, 3) == 'LCS'),
             .groups = 'keep')
# temp [['n']]

# Get number of samples per batch
#----------------------------------------------------------------------------------------
temp <- dataExp2018 %>% 
  group_by (BatchID, DateOfSugarAnalysis) %>% 
  filter (substr (SampleID, 1, 3) != 'LCS' & SampleID != 'B' & 
          SampleID != 'TB' & substr (SampleID, 1, 3) != 'REF') %>% 
  summarise (n = sum (!is.na (SampleID)),
             .groups = 'keep')
# temp [['n']]

# switch back to original working directory
#----------------------------------------------------------------------------------------
setwd (originalDir)

# clean up
#----------------------------------------------------------------------------------------
rm (i, temp, COV, originalDir, temp1, temp2, packageDir)
rm (extractionsStarch, batch, batches, condition, dates, prescribedStarch)
if (exists ('dataExp2018')) rm (dataExp2018)
if (exists ('res')) rm (res)
#========================================================================================