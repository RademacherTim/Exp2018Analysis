#========================================================================================
# Script to read the temperature data from the project chill shed and the phloem of all 
# trees at 1.0 and 2.0 m for the 2018 chilling experiment on the white pines.
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')

# read temperature data file
#----------------------------------------------------------------------------------------
tempData <- read_csv (file = './data/temperatureData_HF_Exp2018.csv', na = "NA",
                      col_types = cols (datetime  = col_datetime (),
                                        record    = col_integer (),
                                        u.battery = col_double (),
                                        t.panel   = col_double (),
                                        t.01.l4   = col_double (),
                                        t.01.l3   = col_double (),
                                        t.01.l2   = col_double (),
                                        t.01.l1   = col_double (),
                                        t.02.l4   = col_double (),
                                        t.02.l3   = col_double (),
                                        t.02.l2   = col_double (),
                                        t.02.l1   = col_double (),
                                        t.03.l4   = col_double (),
                                        t.03.l3   = col_double (),
                                        t.03.l2   = col_double (),
                                        t.03.l1   = col_double (),
                                        t.04.l4   = col_double (),
                                        t.04.l3   = col_double (),
                                        t.04.l2   = col_double (),
                                        t.04.l1   = col_double (),
                                        t.05.l4   = col_double (),
                                        t.05.l3   = col_double (),
                                        t.05.l2   = col_double (),
                                        t.05.l1   = col_double (),
                                        t.end.of.line.01.blue.tape = col_double (),
                                        t.end.of.line.03.red.tape = col_double (),
                                        t.line.backflow = col_double (),
                                        t.air.2p0m = col_double (),
                                        t.01.2p0m  = col_double (),
                                        t.01.1p5m  = col_double (),
                                        t.01.1p0m  = col_double (),
                                        t.02.2p0m  = col_double (),
                                        t.02.1p5m  = col_double (),
                                        t.02.1p0m  = col_double (),
                                        t.03.2p0m  = col_double (),
                                        t.03.1p5m  = col_double (),
                                        t.03.1p0m  = col_double (),
                                        t.04.2p0m  = col_double (),
                                        t.04.1p5m  = col_double (),
                                        t.04.1p0m  = col_double (),
                                        t.05.2p0m  = col_double (),
                                        t.05.1p5m  = col_double (),
                                        t.05.1p0m  = col_double (),
                                        t.06.1p5m  = col_double (),
                                        t.07.1p5m  = col_double (),
                                        t.07.1p5m.air = col_double (),
                                        t.08.1p5m     = col_double (),
                                        t.08.1p5m.air = col_double (),
                                        t.10.1p5m    = col_double ()))
#========================================================================================