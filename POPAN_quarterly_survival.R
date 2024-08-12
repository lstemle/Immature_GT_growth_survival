#install.packages("marked") # first time only
#install.packages("Matrix") 
library(Matrix)
library(lme4)
library(marked)
library(parallel)

#Archbold tortoises mark recapture data to survival estimates
#code adapted from James E Paterson
#https://jamesepaterson.github.io/jamespatersonblog/2020-07-26_jolly_seber_models.html


#load data 

# Examine data structure
abs <- abs_markrecapture_all_tortoises 
head(abs)

#sandhill only
abs<- abs_markrecapture_all_sandhill
#ruderal only
abs<- abs_markrecapture_allruderalr


#shouldn't be necessary unless having issues with csv or excel formatting deleteing 0s
#abs_markrecapture_prop2$ch <- as.numeric(abs_markrecapture_prop2$ch)
#abs_markrecapture_allr$ch <- as.numeric(abs_markrecapture_allr$ch)
#abs <-abs_markrecapture_allr
head(abs)



#For the model fitting, we will use a function to fit a series of models that might be feasible (e.g. time-dependent differences in survival or constant survival, time-dependent entry probability or constant).

# Jolly-Seber models (POPAN formulation) are open population models, and 
# can be used to estimate abundance by including two more parameters than the CJS

# Additional parameters:
# Nsuper (or "superpopulation") = total number of individuals available to enter population throughout study
# pent ("probability of entry") =  the rate at which individuals enter the population from Nsuper (via births and immigration)

# WARNING: there is no adequate GOF tests for Jolly-Seber models. 
# One common method: Test equivalent structure of CJS model with R2ucare (previous tutorials).

# This tests *some* assumptions of Phi and p.
# Jolly-Seber models have an additional assumption:
# marked AND unmarked animals have same p (R2ucare doesn't test this)
# This assumption is required to estimate total abundance (sum of marked and unmarked animals in population)



####start analyses here 
#install.packages("jsonlite", type = "source")
# First, process data (Notice model = "JS", previous version = "CJS")
abs.js.proc <- process.data(abs, model = "JS")

# Second, make design data (from processed data)
abs.js.ddl <- make.design.data(abs.js.proc)

abs.js.abs.models <- function(){
  # Phi formulas
  #Phi.hab.time <- list(formula=~prop_sa*time)  # Just like in other linear models "*" includes main effects and an interaction
  Phi.time <- list(formula=~time) # differs between discrete times
  #Phi.hab <- list(formula=~prop_sa) # differs between males and females
  Phi.dot <- list(formula=~1) # constant survival
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas. pent estimates MUST SUM to 1 (for each group).
  # This is constained using a Multinomial Logit link
  pent.time <- list(formula=~time)
 # pent.hab <- list(formula=~prop_sa)
  pent.dot <- list(formula=~1)
  # Nsuper formulas. Don't confuse "N" from model with predicted population size
 # N.hab <- list(formula=~prop_sa)
  N.dot <- list(formula=~1)
  cml <- create.model.list(c("Phi","p", "pent", "N"))
  results <- crm.wrapper(cml, data = abs.js.proc, ddl = abs.js.ddl,
                         external = FALSE, accumulate = FALSE, hessian = TRUE)
  
  return(results)
}

# Run function
abs.js.models <- abs.js.abs.models()
# Display model table
abs.js.models
##                              model npar      AIC  DeltaAIC       weight   neg2lnl convergence
#2    Phi(~1)p(~1)pent(~time)N(~1)    8 115.0958  0.000000 9.668553e-01  99.09578           0
#4 Phi(~time)p(~1)pent(~time)N(~1)   12 121.8578  6.762051 3.288522e-02  97.85783           0
#1       Phi(~1)p(~1)pent(~1)N(~1)    4 131.6125 16.516669 2.505027e-04 123.61245           0
#3    Phi(~time)p(~1)pent(~1)N(~1)    8 138.2621 23.166314 9.012814e-06 122.26210           0
#The model table (abs.js.models) suggests the most supported model has constant survival, constant detection probability, and time dependent recapture probability
#/probability of entry (~1 = intercept only model). Although we can model-average the results, for simplicity we will just continue with the most-supported model.

#Let’s look at the model output of the most supported model, and then estimate real parameters (remember the coefficients are on different scales depending on the link function).

#We can use the predict function to estimate the real parameters, or calculate them ‘by hand’ using the link functions for each parameter.

# Look at estimates of top model (row number on left of model table, or using name)
#abs.js.models[[1]]  # or dipper.js.models[["Phi.dot.p.dot.pent.dot.N.dot"]] or dipper.js.models$Phi.dot.p.dot.pent.dot.N.dot
abs.js.predicted <- predict(abs.js.models[[2]]) # top model for all, sandhill, ruderal includes pent as time dependent


## 
## crm Model Summary
## 
## Npar :  8
#-2lnL:  105.2759
#AIC  :  121.2759

#Beta
                    Estimate         se         lcl          ucl
#Phi.(Intercept)    2.7875509  0.4059921    1.991806   3.58329541
#p.(Intercept)      2.9825405  0.4460128    2.108355   3.85672563
#pent.(Intercept)  -0.6200055  0.4190629   -1.441369   0.20135775
#pent.time3       -10.1948873 51.4208729 -110.979798  90.59002366
#pent.time4       -10.4019718 58.3118526 -124.693203 103.88925936
#pent.time5        -1.2191659  0.6652110   -2.522979   0.08464767
#pent.time6       -11.0720794 79.6476565 -167.181486 145.03732728
#N.(Intercept)     -9.4603201 86.6349869 -179.264894 160.34425420

#abs.js.predicted <- predict(abs.js.models[[1]]) # [[1]] just calls the model row according to the model table.
abs.js.predicted <- predict(abs.js.models[[2]]) # top model for all, sandhill, ruderal includes pent as time dependent

abs.js.predicted
# Look at predictions of real parameters abs.js.predicted 
## $Phi
#  occ  estimate         se       lcl       ucl
#1   1 0.9310295 0.03327355 0.8301934 0.9738708

#$p
#  occ  estimate           se          lcl ucl
#1   1 0.9419994 0.022182 0.8799341 0.9729671

#$p
#  occ  estimate         se       lcl       ucl
#1   1 0.9517791 0.02047004 0.8917126 0.9793004

#$pent
#  time occ     estimate           se          lcl       ucl
#1    2   2 3.170076e-01 0.0882397981 1.727371e-01 0.5078085
#2    3   3 1.184367e-05 0.0006089959 2.012804e-49 1.0000000
#3    4   4 9.628326e-06 0.0005614338 2.228748e-55 1.0000000
#4    5   5 9.366832e-02 0.0515168203 3.050047e-02 0.2534582
#5    6   6 4.926366e-06 0.0003923692 7.861142e-74 1.0000000

#$N
#      estimate          se          lcl          ucl
#1 7.788166e-05 0.006747276 1.400379e-78 4.331365e+69

#The output shows that survival between capture events is 0.93, detection probability is 0.94, pent is differnt between each capture event
#So what about population size? #this isn't relavant to this paper
#There is no direct estimate of population size in the model. The esitmate of “N” in the model output is for the number of unmarked individuals in the superpopulation.

#To estimate population size, we can derive it using the model estimates.

#Nt+1 = (Nt * Φt )+(Nsuper * pentt)

#In words, this equation says that the population size is the previous population size times the survival rate (to calculate how many individuals survive) plus the number of new individuals from births and immigration.

#To get the inital population size, we estimate pent1 by the constraint:
  
#pent1 = 1 - ∑(pent2..t)


#only carry on if you want abundance and N metrics but that is not relavant to this paper
# Abundance (N) is derived from the estimated parameters
# We will estimate population size at each time by making a dataframe of estimates and calculating N
# We will use the predicted estimates from the top-performing model (in this case: "dipper.js.predicted")

# NOTE: the below method will have to be adjusted based on your final model and the number of capture events
#####not used for the paper
N.derived <- data.frame(occ = c(1:6), # 6 events
                        Phi = c(rep(abs.js.predicted$Phi$estimate, 5), NA),   # 6 survival estimates all the same
                        Nsuper = rep(abs.js.predicted$N$estimate + nrow(abs), 6), # Nsuper estimate + number of marked animals
                        pent = c(1-sum(abs.js.predicted$pent$estimate), abs.js.predicted$pent$estimate)) # Sum of all pent must be 1

# Set-up empty vector for calculating N
N.derived$N <- NA

# The inital population size (N[1]) = Nsuper * (1 - sum(all other pent estimates))
# This is because of the link function for estimating pent.
# The sum of all pent parameters MUST equal 1 (therefore, one less must be estimated)
N.derived$N[1] <- (N.derived$Nsuper[1] * N.derived$pent[1])

# Subsequent population sizes are estimated by calculating surviving individuals (N[t-1] * Phi[t]), and
# Adding new births (Nsuper * pent[t])
for(i in 2:nrow(N.derived)){
  N.derived$N[i] <- (N.derived$N[i-1]*N.derived$Phi[i-1]) + (N.derived$Nsuper[i] * N.derived$pent[i])
}

# Look at what we did
N.derived
##   occ       Phi   Nsuper       pent         N



