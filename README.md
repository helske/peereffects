Codes and data descriptives for *Heterogeneous workplace peer effects in
fathers’ parental leave uptake in Finland*
================

- File `sample_data_construction.R` contains the code used to create the
  data for the modelling, based on the full sample created in
  `full_data_construction.R`. The source data is only accessible via
  Statistics Finland’s remote access system
  [FIONA](https://stat.fi/tup/tutkijapalvelut/fiona-etakayttojarjestelma_en.html).
  Data description is available at [INVEST Data
  Catalog](https://investdata.utu.fi/catalog) (INVEST Data (e17)).
- File `model_estimation.R` contains the codes for estimating the main
  model of the paper, using the Stan model in `model.stan`.
- Files `model_estimation_from_2013.R` and
  `model_estimation_timegap_restriction.R` contain the codes for the
  models using the limited data of the robustness checks of the
  supplementary materials.
- File `causal_effects.R` was be used to compute the causal effect
  estimates, and `figures.R` was used to create the figures of the paper
  and supplementary material.
- File `industry_classifications.pdf` contains the industry classification used in the paper.
- File `descriptives.pdf` contains the descriptive statistics of the data used in the paper.



