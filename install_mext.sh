#

# This script builds and installs the mext system and
# packages to your .maxima directory on linux (and maybe unix)
# systems. To use it type
# ./install_mext.sh maxima
# where maxima is your maxima executable.
#
# Or:
# ./install_mext.sh maxima level
# where level is 0 through 6, with each level building more packages.
# level 0 builds only mext. If level is not supplied, then
# packages through level 5 are built. 
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
discrete_aex numerical"

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
