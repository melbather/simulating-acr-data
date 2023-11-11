#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SIMULATION EXAMPLE #2
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

#Create four villages ----------------------------------------------------------
village_locations2 <- create_villages(4, c(100, 4000), c(300, 3000))

#Set number of sessions --------------------------------------------------------
num_sessions2 <- 4

#Generate session information --------------------------------------------------
sessions2 <- generate_session_information(num_sessions2, 
                                          c("rain", "sun", "snow", "overcast"), 
                                          c("mountain", "non-mountain"), 
                                          2010:2023, 1000, 1000, 50)

#Create a mask -----------------------------------------------------------------
mask2 <- create_mask(1000, 500, 70, 70)

#Set detector locations --------------------------------------------------------
detectors2 <- matrix(c(rep(-2:2, each = 5)*100, rep(-2:2, 5)*100), ncol=2)

#Test with the first session only ----------------------------------------------
single_session_demo <- simulate_capture_histories_with_sessions(
  session_info = sessions2[1,], 
  alpha_forest = 200, 
  alpha_protected = 250, 
  alpha_altitude = 300, 
  beta0 = -1, beta1 = -0.00003,
  mask_locations = mask2, 
  detector_locations = detectors2, 
  g0_base = 0.7, 
  sigma = 70, 
  x_range = c(sessions2[1,5], sessions2[1,6]), 
  y_range = c(sessions2[1,7], sessions2[1,8]), 
  beta2 = -0.0002, beta3 = 1.5, beta4 = 0.0002, 
  beta5 = 0.6, beta6 = -0.3, beta7 = 2.1, 
  beta8 = -0.000015, beta9 = 0.0000002,
  village_locations = village_locations2, 
  cp_beta0 = log(40), cp_beta1 = -1)


#Run simulation with all sessions ----------------------------------------------
session_sim_capt_hist <- vector(mode = "list", length = num_sessions2)
session_sim_bin_capt_hist <- vector(mode = "list", length = num_sessions2)
session_animal_locations <- vector(mode = "list", length = num_sessions2)
session_sim_forest <- vector(mode = "list", length = num_sessions2)
session_sim_altitude <- vector(mode = "list", length = num_sessions2)
session_sim_protected <- vector(mode = "list", length = num_sessions2)
session_sim_mask <- vector(mode = "list", length = num_sessions2)
session_sim_traps <- vector(mode = "list", length = num_sessions2)

for(i in 1:num_sessions2) {
  session_sim <- simulate_capture_histories_with_sessions(
    session_info = sessions2[i,], 
    alpha_forest = 200, 
    alpha_protected = 250, 
    alpha_altitude = 300, 
    beta0 = -1, beta1 = -0.00003,
    mask_locations = mask2, 
    detector_locations = detectors2, 
    g0_base = 0.7, 
    sigma = 70, 
    x_range = c(sessions2[i,5], sessions2[i,6]), 
    y_range = c(sessions2[i,7], sessions2[i,8]), 
    beta2 = -0.0002, beta3 = 1.5, beta4 = 0.0002, 
    beta5 = 0.6, beta6 = -0.3, beta7 = 2.1, 
    beta8 = -0.000015, beta9 = 0.0000002,
    village_locations = village_locations2, 
    cp_beta0 = log(40), cp_beta1 = -1)
  session_sim_capt_hist[[i]] <- session_sim$long_form_capture_history
  session_sim_bin_capt_hist[[i]] <- session_sim$binary_capture_history
  session_animal_locations[[i]] <- session_sim$animal_locations
  session_sim_forest[[i]] <- session_sim$forest
  session_sim_altitude[[i]] <- session_sim$altitude
  session_sim_protected[[i]] <- session_sim$protected_areas
  session_sim_mask[[i]] <- session_sim$mask
  session_sim_traps[[i]] <- session_sim$traps
}

all_animal_locations2 <- do.call(rbind, session_animal_locations)
all_capt_hist2 <- do.call(rbind, session_sim_capt_hist)
all_bin_capt_hist2 <- do.call(rbind, session_sim_bin_capt_hist)
all_forest2 <- do.call(rbind, session_sim_forest)
all_altitude2 <- do.call(rbind, session_sim_altitude)
all_protected2 <- do.call(rbind, session_sim_protected)
all_mask2 <- do.call(rbind, session_sim_mask)
all_traps2 <- lapply(session_sim_traps, function(dat) data.frame(x = dat[, 1], y = dat[, 2]))

#Save workspace image for later use --------------------------------------------
save.image("simulation_example2_output.RData")
