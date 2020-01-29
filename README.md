# Royaute-Pruitt-Ecology-2015-Data-and-Code
Data and R script for the Ecology 2015 article (doi.org/10.1890/14-2424.1)

Dataset descriptions:

Community_effects_wide_P_R.csv
Data from mesocosm experiments seeded with spiders of known behavioral type (Pardosa1-8 column indicating the spiders' activity levels), 
followed by the abundance of each prey species after 1 week of contact with the spider inside the mesocosms. 
The "Initial Phase" rows indicate the species abundances used at before introduction of the spiders. 

Pardosa_cannibalism_P_R.csv
Number of spiders recovered in each mesocosm at the end of the experiment

Pardosa_mesocosm_activity_P_R.csv
Activity levels of each spiders used in the mesocosm (subset of the Community_effects_wide_P_R.csv sheet for easier analysis)

Pardosa_predation_single.csv
Result of single predator-single prey experiments testing wheter spider activity levels influcenced prey capture probability

Repeatability_long_P_R.csv
Repeated measures of activity levels for 19 spiders over 8 consecutive days aling with body-size measurements. 
These spiders were not used as part of the mesocosm experiment

R scripts:

Data_Import_P_R.R
Data loading in R and cleanup

Community_P_R.R
NMDS analysis of mesocosms communities

LMs_P_R.R
Linear models showing effects of spider behavioral type on prey abundance in mesocosms

Predation_single_prey.R
Analysis of prey preference as a function of spider activity level

Repeatability_P_R.R
Mixed models for estimation of spider repeatability in activity levels

multiplot_function.R 
Function for generating multiple plot pannels (old and likely deprecated)

Figures_P_R.R
Scripts for generating manuscript figures
