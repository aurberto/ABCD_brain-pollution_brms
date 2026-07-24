# ABCD_brain-pollution_brms

Code accompanying the manuscript **"Air pollution and neighbourhood disadvantage jointly shape functional brain organization in preadolescence"**.

Complementary analyses related to functional brain organization processing (Functional Connectivity Harmonics, FCH, and Leading Eigenvector Dynamics Analysis, LEiDA) are available at: [https://github.com/isawig/ABCD_brain-dynamics_analyses](https://github.com/isawig/ABCD_brain-dynamics_analyses).

## Repository structure

### `0_install_R_on_HPC`

Contains scripts for installing **R** and managing R packages in a High-Performance Computing (HPC) environment.

* `R_installation.sh`: installs R from the terminal.
* `Rpackages_installation.sh`: installs or updates packages in a user-defined library.

### `A_dataset_building`

Contains the scripts used to assemble the final dataset for statistical analyses by integrating:

* FCH and LEiDA metrics,
* air pollution exposure estimates,
* cognitive and behavioural outcomes,
* demographic and developmental variables.

The scripts also generate descriptive visualizations of the included variables. These scripts are intended to be run **locally**.

### `B_linMods_and_modComp`

Contains all Bayesian regression models evaluating the associations between air pollution and functional brain organization.

The `linear_models` folder includes scripts for all combinations of pollutants and brain metrics, together with scripts for model comparison using Leave-One-Out Cross-Validation (LOO-CV) and Bayesian stacking weights. Analyses are designed to be run on an **HPC** using the corresponding `.sh` scripts.

### `C_exp_mediations`

Contains the scripts used for the exploratory mediation analyses.

The `mediation_models` folder includes mediation models for both prenatal and late-childhood exposure windows, together with scripts for aggregating significant mediation results. Analyses are designed to be run on an **HPC** using the corresponding `.sh` scripts.

### `D_results_visualization`

Contains all scripts used to visualize the results of the regression and mediation analyses.

