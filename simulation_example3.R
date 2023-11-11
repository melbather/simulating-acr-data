#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SIMULATION EXAMPLE #3
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

#No villages created in this example

#Set number of sessions --------------------------------------------------------
num_sessions3 <- 5

#Generate session information --------------------------------------------------
sessions3 <- generate_session_information(num_sessions3, 
                                          c("rain", "sun", "snow", "overcast"), 
                                          c("mountain", "non-mountain"), 
                                          2005:2010, 800, 800, 20)

#Create a mask -----------------------------------------------------------------
mask3 <- create_mask(700, 400, 50, 50)

#Set detector locations --------------------------------------------------------
detectors3 <- matrix(c(rep(-1:1, each = 3)*100, rep(-1:1, 3)*100), ncol=2)

#Test with the first session only ----------------------------------------------
single_session_demo <- simulate_capture_histories_with_sessions(
  session_info = sessions3[1,], 
  alpha_forest = 100, 
  alpha_protected = 80, 
  alpha_altitude = 120, 
  beta0 = -1, beta1 = -0.0001, 
  mask_locations = mask3, 
  detector_locations = detectors3, 
  g0_base = 0.7, 
  sigma = 95, 
  x_range = c(sessions3[1,5], sessions3[1,6]), 
  y_range = c(sessions3[1,7], sessions3[1,8]), 
  beta2 = -0.0001, beta3 = 2.5, beta4 = 0.0002, 
  beta5 = 0.45, beta6 = -0.3, beta7 = 2.3, 
  beta8 = -0.000015, beta9 = 0.000001,
  cp_beta0 = log(45), cp_beta1 = -1)


#Run simulation with all sessions ----------------------------------------------
session_sim_capt_hist <- vector(mode = "list", length = num_sessions3)
session_sim_bin_capt_hist <- vector(mode = "list", length = num_sessions3)
session_animal_locations <- vector(mode = "list", length = num_sessions3)
session_sim_forest <- vector(mode = "list", length = num_sessions3)
session_sim_altitude <- vector(mode = "list", length = num_sessions3)
session_sim_protected <- vector(mode = "list", length = num_sessions3)
session_sim_mask <- vector(mode = "list", length = num_sessions3)
session_sim_traps <- vector(mode = "list", length = num_sessions3)

for(i in 1:num_sessions3) {
  session_sim <- simulate_capture_histories_with_sessions(
    session_info = sessions3[i,], 
    alpha_forest = 100, 
    alpha_protected = 80, 
    alpha_altitude = 120, 
    beta0 = -1, beta1 = -0.0001, 
    mask_locations = mask3, 
    detector_locations = detectors3, 
    g0_base = 0.7, 
    sigma = 95, 
    x_range = c(sessions3[i,5], sessions3[i,6]), 
    y_range = c(sessions3[i,7], sessions3[i,8]), 
    beta2 = -0.0001, beta3 = 2.5, beta4 = 0.0002, 
    beta5 = 0.45, beta6 = -0.3, beta7 = 2.3, 
    beta8 = -0.000015, beta9 = 0.000001,
    cp_beta0 = log(45), cp_beta1 = -1)
  session_sim_capt_hist[[i]] <- session_sim$long_form_capture_history
  session_sim_bin_capt_hist[[i]] <- session_sim$binary_capture_history
  session_animal_locations[[i]] <- session_sim$animal_locations
  session_sim_forest[[i]] <- session_sim$forest
  session_sim_altitude[[i]] <- session_sim$altitude
  session_sim_protected[[i]] <- session_sim$protected_areas
  session_sim_mask[[i]] <- session_sim$mask
  session_sim_traps[[i]] <- session_sim$traps
}

all_animal_locations3 <- do.call(rbind, session_animal_locations)
all_capt_hist3 <- do.call(rbind, session_sim_capt_hist)
all_bin_capt_hist3 <- do.call(rbind, session_sim_bin_capt_hist)
all_forest3 <- do.call(rbind, session_sim_forest)
all_altitude3 <- do.call(rbind, session_sim_altitude)
all_protected3 <- do.call(rbind, session_sim_protected)
all_mask3 <- do.call(rbind, session_sim_mask)
all_traps3 <- lapply(session_sim_traps, function(dat) data.frame(x = dat[, 1], y = dat[, 2]))

#Save workspace image for later use --------------------------------------------
save.image("simulation_example3_output.RData")
