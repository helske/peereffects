Codes used for the results of paper *Heterogeneous workplace peer effects in fathers’ parental leave uptake in Finland*
====================================================================

- File [`sample_data_construction.R`](sample_data_construction.R) contains the code used to create the
  data for the modelling, based on the full sample created in
  [`full_data_construction.R`](full_data_construction.R).
- File [`model_estimation.R`](model_estimation.R) contains the codes for estimating the main
  model of the paper, using the Stan model in [`model.stan`](model.stan).
- File [`causal_effects.R`](causal_effects.R) was be used to compute the causal effect
  estimates of file [`causal_effects.rda`](`causal_effects.rda`), and [`figures.R`](figures.R) was used to create the figures of the paper
  and supplementary material.
- Additional results in the supplement [supplement.pdf](supplement.pdf) are based on files [causal_effects_supplement.R](causal_effects_supplement.R) and [descriptives.R](descriptives.R).


Note that the source data is only accessible via Statistics Finland’s remote access system [FIONA](https://stat.fi/tup/tutkijapalvelut/fiona-etakayttojarjestelma_en.html).

Data description is available at [INVEST Data Catalog](https://investdata.utu.fi/catalog) (INVEST Data (e17)).

The estimated model and posterior samples of causal effects are not available in this repository due to their large size.




