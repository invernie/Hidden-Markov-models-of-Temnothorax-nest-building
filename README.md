# Hidden-Markov-models-of-Temnothorax-nest-building
This repository contains the data and code files used for the article "Identifying cues for self-organised nest wall building behaviour in the rock ant, _Temnothorax rugatulus_, using hidden Markov models", by Invernizzi E., Michelot T., Popov, V., Ng N., Macqueen E., Rouviere A., Webster, M. and Sasaki, T. (in publication).


## Files 
### Agent-based model (ABM)  
_rug_run_sims.py_ 
python script running ABM simulations and collecting output statistics
   
_rug_wall_funcs.py_ 
python script containing code base for wall-building ABM 

_SA.py_
python script containing sensitivity analysis

_rug - pars and statistics.csv_
containins the circular spread values from the sensitivity analysis


### HMM analysis

_1_model_selection.R_
R code used to build, fit and compare (through AIC) the statistical models)

_2_state_dependent_distr.R_
R code used to build Figure 5B

_3_mean_deposition_vs_stone_dens.R_
R code used to build Figure 6

_4_HMM_1_state.R_
R code used to fit the 1-state models

_5_predicted_state_sequence.R_
R code used to build Appendix Figure 4

_6_rates_over_time.R_
R code used to build Figure 4



## Data

_analyseData.R_   (**Edith Invernizzi**)   
R script cleaning and processing data   

_all_building_data.csv_   
raw data   

_brood_measurements - colonies_R34-R54-R29-R5 - anonymous measurers.xlsx_   
raw data of brood centroid coordinates over time. The file also contains additional brood data that were not used.   

_ROI_coordinates.xlsx_   
ROI centre pixel coordinates   

_rates_by_ROI_NAs.csv_   
processed data (rates per min)   

