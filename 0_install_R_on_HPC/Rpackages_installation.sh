export PATH=/scratch/project_ID/softwares/R-4.4.0/R-4.4.0/bin:$PATH
export R_LIBS_USER=/scratch/project_ID/bertoaur/Rlibs/4.4.0
# mkdir -p $R_LIBS_USER

# Specify in brackets packages to install
Rscript -e 'install.packages(c("dplyr","tidyverse","brms","loo","rlang","bayestestR","compositions"), repos="https://cloud.r-project.org")'

