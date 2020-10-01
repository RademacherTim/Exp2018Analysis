#========================================================================================
# This script tests for differences between treatments and sampling heights in 
# stem CO2 efflux for the 2018 phloem chilling and compression experiment at Harvard 
# Forest. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lme4')

# read water potential data
#----------------------------------------------------------------------------------------
source ('./readWaterPotential.R')
source ('./plotingFunctions.R')

phi [['year']] <- year (phi [['date']])

# wrangle data 
#----------------------------------------------------------------------------------------
phi <- phi %>% mutate (tree      = factor (tree),
                       treatment = factor (treatment),
                       date      = factor (date))

# fit a mixed effects model to account for individual tree effects to nranch water 
# potential measurement dates from 2018 only
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = phi.branch ~ (1|tree) + date + treatment, 
            data = filter (phi, year == 2018),
            REML = TRUE)
summary (M1)

# fit a mixed effects model to account for individual tree effects to nranch water 
# potential measurement dates from 2019 only
#----------------------------------------------------------------------------------------
M2 <- lmer (formula = phi.branch ~ (1|tree) + date + treatment, 
            data = filter (phi, year == 2019),
            REML = TRUE)
summary (M2)

# fit a mixed effects model to account for individual tree effects to nranch water 
# potential measurement dates from 2018 only
#----------------------------------------------------------------------------------------
M3 <- lmer (formula = phi.needles ~ (1|tree) + date + treatment, 
            data = filter (phi, year == 2018),
            REML = TRUE)
summary (M3)

# fit a mixed effects model to account for individual tree effects to nranch water 
# potential measurement dates from 2019 only
#----------------------------------------------------------------------------------------
M4 <- lmer (formula = phi.needles ~ (1|tree) + date + treatment, 
            data = filter (phi, year == 2019),
            REML = TRUE)
summary (M4)

#========================================================================================