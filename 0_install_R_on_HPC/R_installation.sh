#!/bin/bash
## Install R from source

export R_VERSION=4.4.0
    
INSTALL_DIR=/scratch/project_ID7/softwares/R-${R_VERSION}
mkdir -p $INSTALL_DIR
cd $INSTALL_DIR

curl -O https://cran.rstudio.com/src/base/R-4/R-${R_VERSION}.tar.gz
tar -xzvf R-${R_VERSION}.tar.gz
cd R-${R_VERSION}

# Configure, compile, and install R
./configure --prefix=$PWD/R/R-4.4.0 --enable-R-shlib --enable-memory-profiling
make
make install

# Try and verify the install 
export PATH=$INSTALL_DIR/R-${R_VERSION}/bin:$PATH
# First you have to load the current version of R

R --version
#The above gives you the answer
