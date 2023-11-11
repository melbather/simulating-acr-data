#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#USING acre TO CREATE MODELS WITH SIMULATION DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(acre)

#Load data created from first simulation example -------------------------------
load("simulation_example1_output.RData")

#-------------------------------------------------------------------------------
#Use output of the FIRST simulation --------------------------------------------
#-------------------------------------------------------------------------------

#Create covariates data frame --------------------------------------------------
cov_df1 <- data.frame(x = all_mask1[,1],
                     y = all_mask1[,2],
                     forest = as.vector(t(all_forest1)),
                     altitude = as.vector(t(all_altitude1)),
                     protected = as.vector(t(all_protected1)))

#Create an acre object ---------------------------------------------------------
acre_object1 <- read.acre(all_capt_hist1, all_traps1, 
                          control_create_mask = list(buffer = 85*5),
                          loc_cov = cov_df1, 
                          session_cov = sessions1,
                          dist_cov = list(villages = village_locations1))

#CREATE MODELS -----------------------------------------------------------------

#Create a null model -----------------------------------------------------------
acre_model1.1 <- fit.acre(acre_object1)

#Summary output & AIC ----------------------------------------------------------
summary(acre_model1.1)
AIC(acre_model1.1)


#Create a model where density depends on forest coverage  ----------------------
acre_model1.2 <- fit.acre(acre_object1, par_extend_model = list(D =~forest))

#Summary output & AIC ----------------------------------------------------------
summary(acre_model1.2)
AIC(acre_model1.2)


#Create a model where density depends on protected area  -----------------------
acre_model1.3 <- fit.acre(acre_object1, par_extend_model = list(D =~protected))

#Summary output & AIC ----------------------------------------------------------
summary(acre_model1.3)
AIC(acre_model1.3)


#Create a model where density depends on altitude  -----------------------------
acre_model1.4 <- fit.acre(acre_object1, par_extend_model = list(D =~altitude))

#Summary output & AIC ----------------------------------------------------------
summary(acre_model1.4)
AIC(acre_model1.4)


#Create a model where density depends on village locations  --------------------
acre_model1.5 <- fit.acre(acre_object1, par_extend_model = list(D =~villages))

#Summary output & AIC ----------------------------------------------------------
summary(acre_model1.5)
AIC(acre_model1.5)


#Create a full model  ----------------------------------------------------------
acre_model1.6 <- fit.acre(acre_object1, 
                          par_extend_model = list(D =~forest +
                                                    protected +
                                                    altitude +
                                                    villages,
                                                  g0 =~weather + terrain,
                                                  kappa =~terrain))

#Summary output & AIC ----------------------------------------------------------
summary(acre_model1.6)
AIC(acre_model1.6)

#Create a full model (minus villages) ------------------------------------------
#This is so visual density estimates of the full model can be plotted despite
#the current bug in show_Dsurf() that prevents plotting when distance covariates
#are used within the model.
acre_model1.6.1 <- fit.acre(acre_object1, 
                            par_extend_model = list(D =~forest +
                                                      protected +
                                                      altitude,
                                                    g0 =~weather + 
                                                      terrain,
                                                    kappa =~terrain))
summary(acre_model1.6.1)

#Create a model where density depends on forest, protected areas and altitude
#This is used to compare acre to ascr since a "full model" in ascr can only
#accept covariates that model density

acre_model1.6.2 <- fit.acre(acre_object1, 
                            par_extend_model = list(D =~forest +
                                                      protected +
                                                      altitude))
summary(acre_model1.6.2)

#Full model where less covariate information is known --------------------------

#Create an acre object with incomplete covariate information -------------------
acre_object1_incomplete <- read.acre(all_capt_hist1, all_traps1, 
                          control_create_mask = list(buffer = 85*5),
                          loc_cov = cov_df1[sample(nrow(cov_df1), 500),], 
                          session_cov = sessions1,
                          dist_cov = list(villages = village_locations1))

#Create a full model with incomplete covariate information ---------------------
acre_model1.7 <- fit.acre(acre_object1_incomplete, 
                          par_extend_model = list(D =~forest +
                                                    protected +
                                                    altitude +
                                                    villages,
                                                  g0 =~weather + terrain,
                                                  kappa =~terrain))

#Summary output & AIC ----------------------------------------------------------
summary(acre_model1.7)
AIC(acre_model1.7)

#Create a full model with incomplete covariate information, without villages ---
#Again, this is so density estimates can still be plotted despite the bug with
#show_Dsurf() (see above)
acre_model1.7.1 <- fit.acre(acre_object1_incomplete, 
                            par_extend_model = list(D =~forest +
                                                      protected +
                                                      altitude,
                                                    g0 =~weather + 
                                                      terrain,
                                                    kappa =~terrain))
summary(acre_model1.7.1)

save.image("acre_model_examples1.RData")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#-------------------------------------------------------------------------------
#Use output of the SECOND simulation -------------------------------------------
#-------------------------------------------------------------------------------

#Load data created from second simulation example -------------------------------
load("simulation_example2_output.RData")

#Create covariates data frame --------------------------------------------------
cov_df2 <- data.frame(x = all_mask2[,1],
                      y = all_mask2[,2],
                      forest = as.vector(t(all_forest2)),
                      altitude = as.vector(t(all_altitude2)),
                      protected = as.vector(t(all_protected2)))

#Create an acre object ---------------------------------------------------------
acre_object2 <- read.acre(all_capt_hist2, all_traps2, 
                          control_create_mask = list(buffer = 70*5),
                          loc_cov = cov_df2, 
                          session_cov = sessions2,
                          dist_cov = list(villages = village_locations2))

#CREATE MODELS -----------------------------------------------------------------

#Create a null model -----------------------------------------------------------
acre_model2.1 <- fit.acre(acre_object2)

#Summary output & AIC ----------------------------------------------------------
summary(acre_model2.1)
AIC(acre_model2.1)


#Create a model where density depends on forest coverage  ----------------------
acre_model2.2 <- fit.acre(acre_object2, par_extend_model = list(D =~forest))

#Summary output & AIC ----------------------------------------------------------
summary(acre_model2.2)
AIC(acre_model2.2)


#Create a model where density depends on protected area  -----------------------
acre_model2.3 <- fit.acre(acre_object2, par_extend_model = list(D =~protected))

#Summary output & AIC ----------------------------------------------------------
summary(acre_model2.3)
AIC(acre_model2.3)


#Create a model where density depends on altitude  -----------------------------
acre_model2.4 <- fit.acre(acre_object2, par_extend_model = list(D =~altitude))

#Summary output & AIC ----------------------------------------------------------
summary(acre_model2.4)
AIC(acre_model2.4)


#Create a model where density depends on village locations  --------------------
acre_model2.5 <- fit.acre(acre_object2, par_extend_model = list(D =~villages))

#Summary output & AIC ----------------------------------------------------------
summary(acre_model2.5)
AIC(acre_model2.5)


#Create a full model  ----------------------------------------------------------
acre_model2.6 <- fit.acre(acre_object2, 
                          par_extend_model = list(D =~forest +
                                                    protected +
                                                    altitude +
                                                    villages,
                                                  g0 =~weather + terrain,
                                                  kappa =~terrain))

#Summary output & AIC ----------------------------------------------------------
summary(acre_model2.6)
AIC(acre_model2.6)

#Create a full model (minus villages) ------------------------------------------
#This is so visual density estimates of the full model can be plotted despite
#the current bug in show_Dsurf() that prevents plotting when distance covariates
#are used within the model.
acre_model2.6.1 <- fit.acre(acre_object2, 
                            par_extend_model = list(D =~forest +
                                                      protected +
                                                      altitude,
                                                    g0 =~weather + 
                                                      terrain,
                                                    kappa =~terrain))


#Full model where less covariate information is known --------------------------

#Create an acre object with incomplete covariate information -------------------
acre_object2_incomplete <- read.acre(all_capt_hist2, all_traps2, 
                                     control_create_mask = list(buffer = 70*5),
                                     loc_cov = cov_df2[sample(nrow(cov_df2), 500),], 
                                     session_cov = sessions2,
                                     dist_cov = list(villages = village_locations2))

#Create a full model with incomplete covariate information ---------------------
acre_model2.7 <- fit.acre(acre_object2_incomplete, 
                          par_extend_model = list(D =~forest +
                                                    protected +
                                                    altitude +
                                                    villages,
                                                  g0 =~weather + terrain,
                                                  kappa =~terrain))

#Summary output & AIC ----------------------------------------------------------
summary(acre_model2.7)
AIC(acre_model2.7)

#Create a full model with incomplete covariate information, without villages ---
#Again, this is so density estimates can still be plotted despite the bug with
#show_Dsurf() (see above)
acre_model2.7.1 <- fit.acre(acre_object2_incomplete, 
                            par_extend_model = list(D =~forest +
                                                      protected +
                                                      altitude,
                                                    g0 =~weather + 
                                                      terrain,
                                                    kappa =~terrain))

save.image("acre_model_examples2.RData")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#-------------------------------------------------------------------------------
#Use output of the THIRD simulation -------------------------------------------
#-------------------------------------------------------------------------------

#Load data created from third simulation example -------------------------------
load("simulation_example3_output.RData")


#Create covariates data frame --------------------------------------------------
cov_df3 <- data.frame(x = all_mask3[,1],
                      y = all_mask3[,2],
                      forest = as.vector(t(all_forest3)),
                      altitude = as.vector(t(all_altitude3)),
                      protected = as.vector(t(all_protected3)))

#Create an acre object ---------------------------------------------------------
#Note: there are no village locations in this simulation
acre_object3 <- read.acre(all_capt_hist3, all_traps3, 
                          control_create_mask = list(buffer = 95*5),
                          loc_cov = cov_df3, 
                          session_cov = sessions3)

#CREATE MODELS -----------------------------------------------------------------

#Create a null model -----------------------------------------------------------
acre_model3.1 <- fit.acre(acre_object3)

#Summary output & AIC ----------------------------------------------------------
summary(acre_model3.1)
AIC(acre_model3.1)


#Create a model where density depends on forest coverage  ----------------------
acre_model3.2 <- fit.acre(acre_object3, par_extend_model = list(D =~forest))

#Summary output & AIC ----------------------------------------------------------
summary(acre_model3.2)
AIC(acre_model3.2)


#Create a model where density depends on protected area  -----------------------
acre_model3.3 <- fit.acre(acre_object3, par_extend_model = list(D =~protected))

#Summary output & AIC ----------------------------------------------------------
summary(acre_model3.3)
AIC(acre_model3.3)


#Create a model where density depends on altitude  -----------------------------
acre_model3.4 <- fit.acre(acre_object3, par_extend_model = list(D =~altitude))

#Summary output & AIC ----------------------------------------------------------
summary(acre_model3.4)
AIC(acre_model3.4)


#There is no model where density depends on village locations, as village
#locations do not exist in this simulation

#Create a full model  ----------------------------------------------------------
acre_model3.5 <- fit.acre(acre_object3, 
                          par_extend_model = list(D =~forest +
                                                    protected +
                                                    altitude,
                                                  g0 =~weather + terrain,
                                                  kappa =~terrain))

#Summary output & AIC ----------------------------------------------------------
summary(acre_model3.5)
AIC(acre_model3.5)


#Full model where less covariate information is known --------------------------

#Create an acre object with incomplete covariate information -------------------
acre_object3_incomplete <- read.acre(all_capt_hist3, all_traps3, 
                                     control_create_mask = list(buffer = 95*5),
                                     loc_cov = cov_df3[sample(nrow(cov_df3), 500),], 
                                     session_cov = sessions3)

#Create a full model with incomplete covariate information ---------------------
acre_model3.6 <- fit.acre(acre_object3_incomplete, 
                          par_extend_model = list(D =~forest +
                                                    protected +
                                                    altitude,
                                                  g0 =~weather + terrain,
                                                  kappa =~terrain))

#Summary output & AIC ----------------------------------------------------------
summary(acre_model3.6)
AIC(acre_model3.6)

save.image("acre_model_examples3.RData")



