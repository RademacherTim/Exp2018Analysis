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
                                        t.c1.l4   = col_double (),
                                        t.c1.l3   = col_double (),
                                        t.c1.l2   = col_double (),
                                        t.c1.l1   = col_double (),
                                        t.c2.l4   = col_double (),
                                        t.c2.l3   = col_double (),
                                        t.c2.l2   = col_double (),
                                        t.c2.l1   = col_double (),
                                        t.c3.l4   = col_double (),
                                        t.c3.l3   = col_double (),
                                        t.c3.l2   = col_double (),
                                        t.c3.l1   = col_double (),
                                        t.c4.l4   = col_double (),
                                        t.c4.l3   = col_double (),
                                        t.c4.l2   = col_double (),
                                        t.c4.l1   = col_double (),
                                        t.c5.l4   = col_double (),
                                        t.c5.l3   = col_double (),
                                        t.c5.l2   = col_double (),
                                        t.c5.l1   = col_double (),
                                        t.end.of.line.c1.blue.tape = col_double (),
                                        t.end.of.line.c3.red.tape = col_double (),
                                        t.line.backflow = col_double (),
                                        t.air.2p0m = col_double (),
                                        t.c1.2p0m  = col_double (),
                                        t.c1.1p5m  = col_double (),
                                        t.c1.1p0m  = col_double (),
                                        t.c2.2p0m  = col_double (),
                                        t.c2.1p5m  = col_double (),
                                        t.c2.1p0m  = col_double (),
                                        t.c3.2p0m  = col_double (),
                                        t.c3.1p5m  = col_double (),
                                        t.c3.1p0m  = col_double (),
                                        t.c4.2p0m  = col_double (),
                                        t.c4.1p5m  = col_double (),
                                        t.c4.1p0m  = col_double (),
                                        t.c5.2p0m  = col_double (),
                                        t.c5.1p5m  = col_double (),
                                        t.c5.1p0m  = col_double (),
                                        t.c6.1p5m  = col_double (),
                                        t.c7.1p5m  = col_double (),
                                        t.c7.1p5m.air = col_double (),
                                        t.c8.1p5m     = col_double (),
                                        t.c8.1p5m.air = col_double (),
                                        t.c10.1p5m    = col_double ()))

# # read old files
# #----------------------------------------------------------------------------------------
# header <- strsplit (readLines ('~/Desktop/cold trees/2018/Cold Trees_Outputs6_07.dat', n = 2), ',')
# header <- substr (header [[2]], 2, nchar (header [[2]])-1)
# temp1 <- read_csv (file = '~/Desktop/cold trees/2018/Cold Trees_Outputs6_07.dat', skip = 4, 
#                    col_types = cols (), col_names = header)
# #----------------------------------------------------------------------------------------
# header <- strsplit (readLines ('~/Desktop/cold trees/2018/Cold Trees_Outputs6_08.dat', n = 2), ',')
# header <- substr (header [[2]], 2, nchar (header [[2]])-1)
# temp2 <- read_csv (file = '~/Desktop/cold trees/2018/Cold Trees_Outputs6_08.dat', skip = 4, 
#                    col_types = cols (), col_names = header)
# #----------------------------------------------------------------------------------------
# header <- strsplit (readLines ('~/Desktop/cold trees/2018/Cold Trees_Outputs6_11.dat', n = 2), ',')
# header <- substr (header [[2]], 2, nchar (header [[2]])-1)
# temp3 <- read_csv (file = '~/Desktop/cold trees/2018/Cold Trees_Outputs6_11.dat', skip = 4, 
#                    col_types = cols (), col_names = header)
# #----------------------------------------------------------------------------------------
# header <- strsplit (readLines ('~/Desktop/cold trees/2018/Cold Trees_Outputs6_21.dat', n = 2), ',')
# header <- substr (header [[2]], 2, nchar (header [[2]])-1)
# temp4 <- read_csv (file = '~/Desktop/cold trees/2018/Cold Trees_Outputs6_21.dat', skip = 4, 
#                    col_types = cols (), col_names = header)
# #----------------------------------------------------------------------------------------
# header <- strsplit (readLines ('~/Desktop/cold trees/2018/Cold Trees_Outputs6_25.dat', n = 2), ',')
# header <- substr (header [[2]], 2, nchar (header [[2]])-1)
# temp5 <- read_csv (file = '~/Desktop/cold trees/2018/Cold Trees_Outputs6_25.dat', skip = 4, 
#                    col_types = cols (), col_names = header)
# #----------------------------------------------------------------------------------------
# header <- strsplit (readLines ('~/Desktop/cold trees/2018/Cold Trees_Outputs6_28.dat', n = 2), ',')
# header <- substr (header [[2]], 2, nchar (header [[2]])-1)
# temp6 <- read_csv (file = '~/Desktop/cold trees/2018/Cold Trees_Outputs6_28.dat', skip = 4, 
#                    col_types = cols (), col_names = header)
# #----------------------------------------------------------------------------------------
# header <- strsplit (readLines ('~/Desktop/cold trees/2018/Cold Trees_Outputs7_19.dat', n = 2), ',')
# header <- substr (header [[2]], 2, nchar (header [[2]])-1)
# temp7 <- read_csv (file = '~/Desktop/cold trees/2018/Cold Trees_Outputs7_19.dat', skip = 4, 
#                    col_types = cols (), col_names = header)
# #----------------------------------------------------------------------------------------
# header <- strsplit (readLines ('~/Desktop/cold trees/2018/Cold Trees_Outputs_1a.dat.1.backup', n = 2), ',')
# header <- substr (header [[2]], 2, nchar (header [[2]])-1)
# temp8 <- read_csv (file = '~/Desktop/cold trees/2018/Cold Trees_Outputs_1a.dat.1.backup', skip = 4, 
#                    col_types = cols (), col_names = header)
# #----------------------------------------------------------------------------------------
# header <- strsplit (readLines ('~/Desktop/cold trees/2018/Cold Trees_Outputs_1a.dat', n = 2), ',')
# header <- substr (header [[2]], 2, nchar (header [[2]])-1)
# temp9 <- read_csv (file = '~/Desktop/cold trees/2018/Cold Trees_Outputs_1a.dat', skip = 4, 
#                    col_types = cols (), col_names = header)