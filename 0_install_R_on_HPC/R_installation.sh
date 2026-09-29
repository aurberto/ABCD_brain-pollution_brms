#!/bin/bash
# Script to install R on HPC from source
export R_VERSION=4.4.0 
SOFTWARES_PATH=/scratch/project_ID/softwares
INSTALL_DIR=${SOFTWARES_PATH}/R-${R_VERSION} 
mkdir -p $INSTALL_DIR

module purge 
module load gcc/15.2.0 openmpi/5.0.10
BUILD_DIR=$TMPDIR/R-build-${R_VERSION} 
mkdir -p $BUILD_DIR 
cd $BUILD_DIR

curl -O https://cran.rstudio.com/src/base/R-4/R-${R_VERSION}.tar.gz 
tar -xzf R-${R_VERSION}.tar.gz 
cd R-${R_VERSION}

./configure --prefix=$INSTALL_DIR/R-${R_VERSION} \ 
            --enable-R-shlib \ 
            --enable-memory-profiling \ 
            --with-readline=no \ 
            --without-ICU
make -j4
make install

export PATH=$INSTALL_DIR/R-${R_VERSION}/bin:$PATH 
R --version
