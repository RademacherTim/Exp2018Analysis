#========================================================================================
# This script tests for differences between treatments and sampling heights in 
# stem CO2 efflux for the 2018 phloem chilling and compression experiment at Harvard 
# Forest. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lme4')
library ('tidyverse')
library ('lubridate')

# read processed respiration data
#----------------------------------------------------------------------------------------
source ('./readProcessedRespData.R')

# make sure all variables are factors
#----------------------------------------------------------------------------------------
respDataExp2018 [['date']]      <- factor (as_date (respDataExp2018 [['datetime']]))
respDataExp2018 [['treatment']] <- factor (respDataExp2018 [['treatment']], levels = c (5, 4, 1))
respDataExp2018 [['tree']]      <- factor (respDataExp2018 [['tree']])
respDataExp2018 [['height']]    <- factor (respDataExp2018 [['chamber']])

# fit mixed effects model with tree as random effect
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = flux.raw ~ (1 | tree) + date + height + date:treatment:height, 
            data = respDataExp2018,
            REML = TRUE)
summary (M1)

# divide period into before, during and after
#----------------------------------------------------------------------------------------
respDataExp2018 [['period']] <- 2
respDataExp2018 [['period']] [respDataExp2018 [['datetime']] <= as_datetime ('2018-08-09')] <- 1
respDataExp2018 [['period']] [respDataExp2018 [['datetime']] <= as_datetime ('2018-06-25')] <- 0
respDataExp2018 [['period']] <- factor (respDataExp2018 [['period']], levels = 0:2)

# fit mixed effects model with tree as random effect
#----------------------------------------------------------------------------------------
M2 <- lmer (formula = flux.raw ~ (1 | tree) + period + height + period:treatment:height, 
            data = respDataExp2018,
            REML = TRUE)
summary (M2)

#========================================================================================