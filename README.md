**Data and code for a project on bias in crime statistics using simulated data.**

All scripts should be ran from within the 'crime_simulation.Rproj' R project file.
Take note of the steps detailed in the _data description_ file (in the 'docs' folder) before using the scripts.

1. R script files named 'replicate_*' (e.g. replicate_age.R') are used to wrangle
   census data into an appropriate format for usage in the main script. These scripts
   should be run first, taking note of the _data description_ file.
2. R script file named 'data_convert_preperation.R' is required mainly to reduce the size
   of data files detailed in _data description_. When downloaded using the steps provided,
   the data is too large in its raw form. Use this script to load, wrangle, and re-save the data.
3. R script file named 'simulation_analysis.R' is the main script to replicate the study. It can only
   be ran once data has been processed in steps 1 and 2. It generates and saves the visuals. Statistics
   reported in the paper are usually printed to the Console.
