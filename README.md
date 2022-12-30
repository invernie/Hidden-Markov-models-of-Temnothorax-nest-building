# Hidden-Markov-models-of-Temnothorax-nest-building
This repository contains the data and code files used for the manuscript "Identifying cues for self-organised nest wall building behaviour in the rock ant, _Temnothorax rugatulus_, using hidden Markov models", by Invernizzi E., Michelot T., Popov, V., Ng N., Macqueen E., Rouviere A., Webster, M. and Sasaki, T. (in preparation).


## Code authorship   
### Agent-based model (ABM)  
_rug_run_sims.py_   (**Edith Invernizzi**)   
python script running ABM simulations and collecting output statistics
   
_rug_wall_funcs.py_   (**Edith Invernizzi**)   
python script containing code base for wall-building ABM 

### Statistical analysis

_HMM_functions_for_model_fitting.R_   (**Valentin Popov**)  
The heart of the HMM coding work. Contains the functions that are used for fitting an HMM. The following functions can be found in this file:   
* pn2pw - transforms normal parameters into working parameters. Among other things it has the argument diff that allows for flexibility in terms of keeping the parameters of the covariates constant across colonies. The case diff=TRUE was never tested in a fitting procedure
* pw2pn - transforms working parameters into normal parameters
* get_allprobs - for given parameters and data, calculates the probabilities/pdfs of the observations for each state. There is a scope for improvement (more elegant coding) but it works fine.
* mllk - calculates the negative log likelihood. Note that the function calls an external function forward written in C++ to speed up the calculations.
* mle - a wrapper function to fit the model. Essential does the optimisation using nlm and presents the output in understandable way    

_Global decoding using Viterbi algorithm.R_   (**Valentin Popov**)   
R script with global decoding functions   

_Simulated samples.R_   (**Valentin Popov**)   
R script with function to generate samples from model. In the file there are some tests based on the model using a quadratic function of stone density for covariates

_Evaluating models_ms.rmd_   (**Edith Invernizzi**)   
R markdown script that renders a walkthrough (in pdf format) of how the four statisticals models were compared and evaluated   


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

