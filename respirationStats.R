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

# drop unnecessary variable
#----------------------------------------------------------------------------------------
respDataExp2018 <- respDataExp2018 %>% 
  select (treatment, tree, chamber, datetime, flux.raw)

# make sure all variables are factors
#----------------------------------------------------------------------------------------
respDataExp2018 [['date']]      <- factor (as_date (respDataExp2018 [['datetime']]))
respDataExp2018 [['treatment']] <- factor (respDataExp2018 [['treatment']], levels = c (5, 4, 1))
respDataExp2018 [['tree']]      <- factor (respDataExp2018 [['tree']])
respDataExp2018 [['height']]    <- factor (respDataExp2018 [['chamber']], levels = c (3, 1, 2))

# add column based on period (e.g., before, during, and after treatment)
#----------------------------------------------------------------------------------------
period <- ifelse (respDataExp2018 [['datetime']] < startDate, 'before', 
                  ifelse (respDataExp2018 [['datetime']] > endDate, 'after','during'))
periodAlt <- ifelse (respDataExp2018 [['datetime']] < startDate | 
                     respDataExp2018 [['datetime']] > endDate,'non-chilling','chilling')
respDataExp2018 <- respDataExp2018 %>% 
  mutate (period    = factor (period,    levels = c ('during','after','before')), 
          periodAlt = factor (periodAlt, levels = c ('non-chilling','chilling')))

# fit mixed effects model with tree and height as random effects to account for 
# idiosyncratic differences due to factors such as variations in exact azimuth or solar 
# exposure due to varying thickness of overlaying canopy 
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = flux.raw ~ (tree | height) + date + period:treatment:height, 
            data = respDataExp2018,
            REML = TRUE)
summary (M1)

#========================================================================================