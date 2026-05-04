# Quantifying Uncertainty in the Structure and Duration of Silurian Biogeochemical Events
<!-- [![DOI](https://zenodo.org/badge/375167562.svg)](https://zenodo.org/badge/latestdoi/375167562) -->

This repository contains the code associated with the age model and relevant figures associated with "*Quantifying Uncertainty in the Structure and Duration of Silurian Biogeochemical Events*". 

## File Structure 
The breakdown of the files and R scripts in this repository is below. If you would like to reproduce the age models and associated figures in the manuscript run the scripts in the `./R/` directory *in the order listed* below. Note that the `results` directory is empty in this repository as it is too large to archive on GitHub.  

```
├── data
    └── geochron.csv # U-Pb ages, uncertanties, and stratigraphic positions for all sampled tuffs.
    └── isotopes.csv # carbon and oxygen isotope compostions for the core, including stratigraphic position in depth and the GTS 2020 age. 
├── R # R scripts to to reproduce figures and calculations.
    └── 01_age_modeling.R # age depth modeling code
    └── 02_calculate_sed_rate.R # calculates sedimentaiton rate between top and bottom ages
    └── 03_plots.R # age depth model plotting and hiatus duration plotting
    └── 04_isotope_plots.R # plots isotope compositions against the different age models.
    └── 05_calculate_durations.R # calculates durations between stratigraphic points of interest. 
    └── format_model.R # helper function
├── figures # output directory for PDF figures
├── results # output directory for duration results and age model objects.

    ```
