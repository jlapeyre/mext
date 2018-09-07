#!/bin/bash

# Safer to use bash until we know everything is working again

# NOTE!!
# In order to build lapack with sbcl, maxima must be started with
# something like "maxima -X '--dynamic-space-size 2048'".
#
# This script builds and installs the mext system and packages to your
# ".maxima" directory on linux (and maybe unix) systems.
#
# USAGE:
#
# ./install_mext.sh maxima level
# ./install_mext.sh maxima
# 
# Builds and installs the mext system and packages.
# `level`  a number from 0 through 6. This specifies how many
#          groups of packges to install. `level` = 0 installs only the mext system.
#          `level` = 5 installs many packages, but not lapack. `level` = 6 installs many packages
#          and lapack as well. If `level` = 5 is the default if it is not specified.
# `maxima` the path to the maxima executable.
#
# 
# Some of the packages in the list below, particularly the
# latter ones, may fail to build on some systems.

maxima=$1

if [ $# -eq 0 ]
  then
    echo "No maxima executable supplied, using maxima";
    maxima=maxima
fi

if [ $# -eq 2 ]
  then
    level=$2;
  else
    level=5;
fi

echo Building mext with $maxima
echo Building through level $level;

topdir=`pwd`

cd ./packages/mext_system; $maxima -b ibuild.mac; cd ..

if [ $level -eq 0 ]
  then
  exit;
fi

# We have to put the full pathname, or else maxima
# can't find the files. Typing the command at the
# command line does work.
build_one_package () {
 echo $maxima -b $topdir/packages/$1/ibuild.mac
 $maxima -b  $topdir/packages/$1/ibuild.mac
}

packages1="defmfun1 maxdoc mext_basic"

packages2="
test_defmfun1 runtime aex lists_aex
discrete_aex numerical replacements"

packages3="
alt_eigen to_poly_mext fourier_elim_mext to_poly_mext
tpsolve grobner_mext"

packages4="
pw circuits coma finance implicit bernstein nelder_mead"

packages5="quicklisp store"

packages6="lapack"

for package in $packages1
do
   build_one_package $package
done
if [ $level -eq 1 ]
  then
  exit;
fi

for package in $packages2
do
   build_one_package $package
done
if [ $level -eq 2 ]
  then
  exit;
fi

for package in $packages3
do
   build_one_package $package
done
if [ $level -eq 3 ]
  then
  exit;
fi

for package in $packages4
do
   build_one_package $package
done
if [ $level -eq 4 ]
  then
  exit;
fi

for package in $packages5
do
   build_one_package $package
done
if [ $level -eq 5 ]
  then
  exit;
fi

for package in $packages6
do
   build_one_package $package
done
if [ $level -eq 6 ]
  then
  exit;
fi
