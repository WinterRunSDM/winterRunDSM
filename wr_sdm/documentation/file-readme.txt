# Description of important files in winterRunDSM

* data-raw/
  * cache-data.R: storage of baseline cached values from literature, expert elicitation, other sources
  * explore-results.R: run model and explore spawner output, plot grandtab values
  * new.t.diver.rds: total diversion data updated in the 2024 LTO version of the model

* R/
  * data.R: documentation for data objects created in parameter creation script (wr_sdm_baseline_parameters.R in this project)
  * model.R: run winter run mode. outputs generated from the model are listed here.

* calibration/
  * run-calibration.R: run calibration; evaluate results
  * update_params.R: function to update parameter list with specified calibration results
  * evaluate.R: 
  * fitness.R:

* wr_sdm/
  * parameter_lists/
    * wr_sdm_baseline_parameters.R: baseline for WR SDM effort
    * wr_sdm_scen1.R: example scenario script, creating a new parameter list for scenario. 
  * evaluation/
  * documentation/