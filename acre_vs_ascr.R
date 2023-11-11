#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           COMPARING acre TO ascr 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(acre)
library(ascr)

#Load acre models --------------------------------------------------------------
load("acre_model_examples1.RData")

#ascr requires capture histories and traps to be in a named list format --------
ascr_capt <- create.capt(captures = all_capt_hist1, traps = all_traps1)

#Use the same mask used in acre to ensure a fair comparison --------------------
ascr_mask1 <- acre_object1$mask

#Create a list of data frames (one for each session) with covariate information 
cov_df_ascr <- list()
for(i in 1:nrow(sessions1)) {
  cov_df_ascr[[i]] <- subset(acre_object1$par.extend$data$mask, session == i)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create the null model ---------------------------------------------------------
ascr_model1.1 <- fit.ascr(capt = ascr_capt, traps = all_traps1, mask = ascr_mask1)

#View summary and compare to acre's null model summary -------------------------
summary(ascr_model1.1)
summary(acre_model1.1)

#View density surface and compare to acre's null model density surface ---------
par(mfrow=c(1, 2))
show.Dsurf(ascr_model1.1)
show_Dsurf(acre_model1.1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create model where density depends on forest coverage -------------------------
ascr_model1.2 <- fit.ascr(capt = ascr_capt, traps = all_traps1, mask = ascr_mask1,
                          ihd.opts = list(model = ~forest, 
                                          covariates = cov_df_ascr,
                                          scale = FALSE))

#View summary and compare to summary of corresponding acre model ---------------
summary(ascr_model1.2)
summary(acre_model1.2)

#View density surface and compare density surface of corresponding acre model --
par(mfrow=c(1, 2))
show.Dsurf(ascr_model1.2)
show_Dsurf(acre_model1.2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create model where density depends on protected areas -------------------------
ascr_model1.3 <- fit.ascr(capt = ascr_capt, traps = all_traps1, mask = ascr_mask1,
                          ihd.opts = list(model = ~protected, 
                                          covariates = cov_df_ascr,
                                          scale = FALSE))

#View summary and compare to summary of corresponding acre model ---------------
summary(ascr_model1.3)
summary(acre_model1.3)

#View density surface and compare density surface of corresponding acre model --
par(mfrow=c(1, 2))
show.Dsurf(ascr_model1.3)
show_Dsurf(acre_model1.3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create model where density depends on altitude --------------------------------
ascr_model1.4 <- fit.ascr(capt = ascr_capt, traps = all_traps1, mask = ascr_mask1,
                          ihd.opts = list(model = ~altitude, 
                                          covariates = cov_df_ascr,
                                          scale = FALSE))

#View summary and compare to summary of corresponding acre model ---------------
summary(ascr_model1.4)
summary(acre_model1.4)

#View density surface and compare density surface of corresponding acre model --
par(mfrow=c(1, 2))
show.Dsurf(ascr_model1.4)
show_Dsurf(acre_model1.4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create model where density depends on distance to nearest village -------------
ascr_model1.5 <- fit.ascr(capt = ascr_capt, traps = all_traps1, mask = ascr_mask1,
                          ihd.opts = list(model = ~villages, 
                                          covariates = cov_df_ascr,
                                          scale = FALSE))

#View summary and compare to summary of corresponding acre model ---------------
summary(ascr_model1.5)
summary(acre_model1.5)

#View density surface (cannot compare this to acre, due to bug)-----------------
par(mfrow=c(1, 2))
show.Dsurf(ascr_model1.5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create model where density depends on forest coverage, protected areas, and
#altitude ----------------------------------------------------------------------
ascr_model1.6 <- fit.ascr(capt = ascr_capt, traps = all_traps1, mask = ascr_mask1,
                          ihd.opts = list(model = ~forest + protected + altitude, 
                                          covariates = cov_df_ascr,
                                          scale = FALSE))

#View summary and compare to summary of corresponding acre model ---------------
summary(ascr_model1.6)
summary(acre_model1.6.2)

#View density surface and compare density surface of corresponding acre model --
par(mfrow=c(1, 2))
show.Dsurf(ascr_model1.6)
show_Dsurf(acre_model1.6.2)

save.image("acre_ascr_comparison1.RData")







