export PATH=/scratch/project_ID/softwares/R-4.4.0/R-4.4.0/bin:$PATH
export R_LIBS_USER=/scratch/project_ID/bertoaur/Rlibs/4.4.0
mkdir -p $R_LIBS_USER

Rscript -e 'install.packages(c("dplyr","tidyverse","brms","loo","rlang","bayestestR","compositions","remotes","posterior","writexl"), repos="https://cloud.r-project.org")'

RHOME=$(R RHOME)
MAKECONF=$RHOME/etc/Makeconf

# Back up first, just in case
cp $MAKECONF ${MAKECONF}.bak

sed -i \
    -e 's/^CXX17 = *$/CXX17 = g++/' \
    -e 's/^CXX17STD = *$/CXX17STD = -std=gnu++17/' \
    -e 's/^CXX17PICFLAGS = *$/CXX17PICFLAGS = -fPIC/' \
    $MAKECONF

grep -A2 "^CXX17" $MAKECONF
Rscript -e 'install.packages(c("rstan"), repos="https://cloud.r-project.org")'
