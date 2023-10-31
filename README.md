# Simulating realistic acoustic capture-recapture data
Functions to simulate acoustic capture-recapture data, with examples for how to use the resulting data with the `acre` and `ascr` R packages.

This code was written for my project for STATS793 as part of my Master of Science in Statistics at the University of Auckland. The project was supervised by Ben Stevenson. 

# The functions
The functions used in the simulation can be found in the following files:
- functions/create_villages.R
- functions/find_bearings.R
- functions/format_capture_histories.R
- functions/simulate_capture_histories_with_sessions.R
- functions/generate_session_information.R
- functions/generate_session_locations.R
- functions/create_mask.R
*You do not have to run all these files individually, as they are called within the simulation.*

# Requirements
To run the simulation and create the models, the following R packages must be installed:
- `acre`
- `ascr`
- `mtvnorm`
- `fields`
- `gstat`
- `CircStats`
- `dplyr`

# Running the simulations
Examples of how the simulation can be used can be found in simulation_example1.R, simulation_example2.R, and simulation_example3.R. ***Run these files before any other files.***

# Creating `acre` models
Examples of how the simulated data can be used to create models with the `acre` package can be found in acre_model_examples.R. ***Please note that this file can take a large amount of computational resources to run, and it is recommended to run it in small chunks, perhaps one model at a time, to avoid a termination of your R session. You may also wish to only load one simulated dataset into your R session at a time. Expect each model to take anywhere between a few seconds to 10+ minutes to run.***

# Comparing `acre` models to `ascr` models
A comparison of models in `acre` to models in `ascr` can be found in the file acre_vs_ascr.R.

