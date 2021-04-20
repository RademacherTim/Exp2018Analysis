#========================================================================================
# Script with variables and functions relevant for ploting figuresin R
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')

# function for calculate the standard error
#----------------------------------------------------------------------------------------
se <-  function (x) {
  sd (x, na.rm = TRUE) / sqrt (sum (!is.na (x)))
}

# Create %notin% operator
#----------------------------------------------------------------------------------------
`%notin%` <- Negate (`%in%`)

# set colours for treatments: control, girdled, compressed, double compressed and chilled
#----------------------------------------------------------------------------------------
# tColours <- tibble (colour = c ('#91b9a4','#C0334D','#F18904','#5C4A72','#0073cf'),
#                     treatment = c ('control','girdled','compressed','double compressed',
#                                    'chilled'))
tColours <- tibble (colour = c ('#aab300','#55a51c','#445026','#68ace5','#0072cf','#003e74','#C0334D'),
                    treatment = c ('control','control2','control3','chilled','chilled2',
                                   'chilled3','air'))

# set colours for carbon sinks: growth, respiration, change in NSC concentrations. 
#----------------------------------------------------------------------------------------
sColours <- tibble (colour   = c ('#8073ac','#f3bd48','#aab300'), 
                    variable = c ('NSC'    ,'resp'   ,'SC'))

# function to add opacity to a colour
#----------------------------------------------------------------------------------------
addOpacity <- function (colour, alpha = 1) {
  if (missing (colour)) stop ("Please provide a colour or vector of colours.")
  apply (sapply (colour, col2rgb) / 255, 2, 
         function (x) rgb (x [1], x [2], x [3], alpha = alpha))
}

# y-axis positions of treatment and sampling height combinations
#----------------------------------------------------------------------------------------
yPositions <- c (0.8, 1.8, 2.3, 3.3, 3.8, 4.8, 5.3, 5.8)

# Function to plot critical dates
#----------------------------------------------------------------------------------------
criticalDates <- function (group, asDate = TRUE, startOnly = FALSE, endOnly = FALSE) {
  
  # if only start date is requested, return start date
  #--------------------------------------------------------------------------------------
  if (startOnly) {
    if (asDate & group %in% c (1, 5)) {
      return (as_date ('2018-06-25'))
    } else if (asDate & group == 4) {
      return (as_date ('2018-06-26'))
    } else if (!asDate & group %in% c (1, 5)) {
      return (as_datetime ('2018-06-25'))
    } else if (!asDate & group == 4) {
      return (as_datetime ('2018-06-26'))
    }
  }
  
  # if only end date is requested, return end date
  #--------------------------------------------------------------------------------------
  if (endOnly) {
    if (asDate & group == 5) {
      return (as_date ('2018-09-03'))
    } else if (asDate & group == 4) {
      return (as_date ('2018-09-04'))
    } else if (!asDate & group == 5) {
      return (as_datetime ('2018-09-03'))
    } else if (!asDate & group == 4) {
      return (as_datetime ('2018-09-04'))
    } else {
      stop ('Error: the specified group had no end date.') 
    }
  }
  
  # plot critical dates (control groups have no end dates)
  #--------------------------------------------------------------------------------------
  if (group == 4) { # double compressed
    if (asDate) {
      abline (v = as_date ('2018-06-26'), col = '#99999999', lty = 2) # start date
      abline (v = as_date ('2018-09-04'), col = '#99999999', lty = 2) # end  # TR Needs to be updated
    } else {
      abline (v = as_datetime ('2018-06-26'), col = '#99999999', lty = 2) # start date
      abline (v = as_datetime ('2018-09-04'), col = '#99999999', lty = 2) # end  # TR Needs to be updated
    }
  } else if (group == 5) { # chilled
    if (asDate) {
      abline (v = as_date ('2018-06-25'), col = '#99999999', lty = 2) # start date
      abline (v = as_date ('2018-09-03'), col = '#99999999', lty = 2) # end date # TR Needs to be updated
    } else {
      abline (v = as_datetime ('2018-06-25'), col = '#99999999', lty = 2) # start date
      abline (v = as_datetime ('2018-09-03'), col = '#99999999', lty = 2) # end date # TR Needs to be updated
    }
  } else { # control
    if (asDate) {
      abline (v = as_date ('2018-06-25'), col = '#99999999', lty = 2) # start date
    } else {
      abline (v = as_datetime ('2018-06-25'), col = '#99999999', lty = 2) # start date
    }
  }
  
  # Return zero exit status
  #----------------------------------------------------------------------------------------
  return (0)
}
#========================================================================================