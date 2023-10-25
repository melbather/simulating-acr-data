#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SIMULATION EXAMPLE #1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load data and libraries -------------------------------------------------------
library(mvtnorm)
library(fields)
library(gstat)
library(CircStats)
library(dplyr)

#Call functions ----------------------------------------------------------------
source("functions/create_villages.R")
source("functions/find_bearings.R")
source("functions/format_capture_histories.R")
source("functions/simulate_capture_histories_with_sessions.R")
source("functions/generate_session_information.R")
source("functions/generate_session_locations.R")
source("functions/create_mask.R")

#Create two villages -----------------------------------------------------------
village_locations1 <- create_villages(2, c(-200, 500), c(-300, 250))

#Set number of sessions --------------------------------------------------------
num_sessions1 <- 3

#Generate session information --------------------------------------------------
sessions1 <- generate_session_information(num_sessions1, 
                                         c("rain", "sun", "snow", "overcast"), 
                                         c("mountain", "non-mountain"), 
                                         2012:2018, 1000, 1000, 40)

#Create a mask -----------------------------------------------------------------
mask1 <- create_mask(900, 600, 66, 66)

#Set detector locations --------------------------------------------------------
detectors1 <- matrix(c(rep(-1:1, each = 3)*100, rep(-1:1, 3)*100), ncol=2)

#Test with the first session only ----------------------------------------------
single_session_demo <- simulate_capture_histories_with_sessions(
  session_info = sessions1[1,], 
  alpha_forest = 400, 
  alpha_protected = 300, 
  alpha_altitude = 400, 
  beta0 = -1, beta1 = 0.0001, 
  mask_locations = mask1, 
  detector_locations = detectors1, 
  g0_base = 0.85, 
  sigma = 85, 
  x_range = c(sessions1[1,5], sessions1[1,6]), 
  y_range = c(sessions1[1,7], sessions1[1,8]), 
  beta2 = -0.0002, beta3 = 1.6, beta4 = 0.0001, 
  beta5 = 0.4, beta6 = -0.2, beta7 = 1.8, 
  beta8 = 0.00001, beta9 = 0.000001,
  village_locations = village_locations1, 
  cp = 50, cp_beta0 = log(50), cp_beta1 = -1)


#Run simulation with all sessions ----------------------------------------------
session_sim_capt_hist <- vector(mode = "list", length = num_sessions1)
session_animal_locations <- vector(mode = "list", length = num_sessions1)
session_sim_forest <- vector(mode = "list", length = num_sessions1)
session_sim_altitude <- vector(mode = "list", length = num_sessions1)
session_sim_protected <- vector(mode = "list", length = num_sessions1)
session_sim_mask <- vector(mode = "list", length = num_sessions1)
session_sim_traps <- vector(mode = "list", length = num_sessions1)

for(i in 1:num_sessions1) {
  session_sim <- simulate_capture_histories_with_sessions(
    session_info = sessions1[i,], 
    alpha_forest = 400, 
    alpha_protected = 300, 
    alpha_altitude = 400, 
    beta0 = -1, beta1 = 0.0001, 
    mask_locations = mask1, 
    detector_locations = detectors1, 
    g0_base = 0.85, 
    sigma = 85, 
    x_range = c(sessions1[i,5], sessions1[i,6]), 
    y_range = c(sessions1[i,7], sessions1[i,8]), 
    beta2 = -0.0002, beta3 = 1.6, beta4 = 0.0001, 
    beta5 = 0.4, beta6 = -0.2, beta7 = 1.8, 
    beta8 = 0.00001, beta9 = 0.000001,
    village_locations = village_locations1, 
    cp = 50, cp_beta0 = log(50), cp_beta1 = -1)
  session_sim_capt_hist[[i]] <- session_sim$long_form_capture_history
  session_animal_locations[[i]] <- session_sim$animal_locations
  session_sim_forest[[i]] <- session_sim$forest
  session_sim_altitude[[i]] <- session_sim$altitude
  session_sim_protected[[i]] <- session_sim$protected_areas
  session_sim_mask[[i]] <- session_sim$mask
  session_sim_traps[[i]] <- session_sim$traps
}

all_animal_locations1 <- do.call(rbind, session_animal_locations)
all_capt_hist1 <- do.call(rbind, session_sim_capt_hist)
all_forest1 <- do.call(rbind, session_sim_forest)
all_altitude1 <- do.call(rbind, session_sim_altitude)
all_protected1 <- do.call(rbind, session_sim_protected)
all_mask1 <- do.call(rbind, session_sim_mask)
all_traps1 <- lapply(session_sim_traps, function(dat) data.frame(x = dat[, 1], y = dat[, 2]))

#Save workspace image for later use --------------------------------------------
save.image("simulation_example1_output.RData")
