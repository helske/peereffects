# Codes used in "Heterogeneous workplace peer effects in fathers’ parental leave uptake in Finland" paper


* File `data_construction.R` contains the code used to create the data for the modelling. The source data is only accessible via Statistics Finland's remote access system [FIONA](https://stat.fi/tup/tutkijapalvelut/fiona-etakayttojarjestelma_en.html).
* File `model_estimation.R` contains the codes for estimating the main model of the paper, using the Stan model in `model.stan`.
* Files `model_estimation_from_2013.R` and `model_estimation_timegap_restriction.R` contain the codes for the models using the limited data of the robustness checks of the supplementary materials.
* File `causal_effects.R` was be used to compute the causal effect estimates, and `figures.R` was used to create the figures of the paper and supplementary material.git 