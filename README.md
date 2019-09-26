# README fire_model_analysis

This Github repository contains the data and code used to test the functionality of a new fire effects model being incorporated into RHESSys (https://github.com/RHESSys/RHESSys) for predicting future fire regimes.

Results from the analysis can be found in “Integrating fire effects on vegetation carbon cycling within an ecohydrologic model” by Bart, Kennedy, Tague and McKenzie. Ecological Modelling

---

Repository organization is as follows:
* R: This folder contains the scripts to run the analysis. The scripts are set up numerically so that they can be run in sequence..
* awks: This folder contains awk scripts that are occasionally called from RHESSys.
* outputs: This folder contains processed outputs from the analysis of fire effects. It includes figures and processed datasets.
* ws_hja: This folder contains the climate data, parameters, flowtable and worldfile needed, in combination with inputs from R scripts, to run RHESSys in H.J. Andrews. 
* ws_p300: This folder contains the climate data, parameters, flowtable and worldfile needed, in combination with inputs from R scripts, to run RHESSys in P300/P301. To clarify, in the manuscript, this site is labeled as P301. In the dataset, it is labeled as P300. P301 is a nested watershed within P300, such that they are both correct site names. For purposes of this analysis, they are one and the same.
* ws_rs: This folder contains the climate data, parameters, flowtable and worldfile needed, in combination with inputs from R scripts, to run RHESSys in Rattlesnake.
* ws_sf: This folder contains the climate data, parameters, flowtable and worldfile needed, in combination with inputs from R scripts, to run RHESSys in Santa Fe.

Note: None of the modeling folders contain any raw output from the RHESSys model, as RHESSys output files combined to over 5 GB in space.

---

Versions

To replicate the analysis, the following versions of RHESSys and RHESSysIOinR were used.

RHESSys version: https://github.com/RHESSys/RHESSys/tree/e31bc67. 
Code for specific to the fire-effects model is located in the compute_fire_effects.c file. 

RHESSysIOinR version: https://github.com/RHESSys/RHESSysIOinR/tree/9e43310. 
RHESSysIOinR is an R package used to organize RHESSys inputs and outputs. The package can be downloaded from the above link.

