# Analyses for: The extinct marine megafauna of the Phanerozoic
  
## File description  
  
### data  
  
**input/megafauna_clean.rds** - cleaned megafauna database  
**output/cmr_data.rds** - posteriors from the capture-mark-recapture model  
**output/pbdb_data_raw.rds** - PBDB download to estimate the PBDB coverage of the megafauna  
**output/pbdb_data_size.rds** - PBDB download to estimate sampling rates via capture-mark-recapture and to quantify extinction risk  
**output/synonym_list.csv** The taxa from the megafauna and their corresponding synonyms as noted in the PBDB

### models  
  
**logit_model.rds** - Bayesian model used to quantify extinction selectivity per group over time  
**risk_change_model.rds** - Bayesian model used to compare extinction risk of megafauna taxa to baseline group  
  
### R  
  
**R/crm_model.R** - Script to fit a capture-mark-recapture model in order to estimate rates  
**R/pbdb_comparison.R** - Script to download PBDB occurrences and to quantify coverage of the megafauna in the PBDB  
**R/preprocess_data.R** - Script to download the megafauna database and to clean it in order to create *input/megafauna_clean.rds*  
**R/sampling_through_time.R** - Script to create a gif of sampling sites over time  
**R/statistical_summary.R** - Script to get basic statistical values for the database that are reported in the manuscript
  
  


