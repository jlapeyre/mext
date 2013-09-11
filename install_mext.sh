#

# This script builds and installs the mext system and
# packages to your .maxima directory on linux (and maybe unix)
# systems. To use it type
# ./install_mext.sh maxima
# where maxima is your maxima executable.
#
# Some of the packages in the list below, particularly the
# latter ones, may fail to build on some systems.

maxima=$1

if [ $# -eq 0 ]
  then
    echo "No maxima executable supplied, using maxima";
    maxima=maxima
fi

echo Building mext with $maxima

topdir=`pwd`

cd ./packages/mext_system; $maxima -b ibuild.mac; cd ..

# We have to put the full pathname, or else maxima
# can't find the files. Typing the command at the
# command line does work.
build_one_package () {
 echo $maxima -b $topdir/packages/$1/ibuild.mac
 $maxima -b  $topdir/packages/$1/ibuild.mac
}

packages="
defmfun1 maxdoc mext_defmfun1 runtime aex lists_aex
discrete_aex numerical test_defmfun1
alt_eigen
pw
circuits
coma
finance
implicit
to_poly_mext
fourier_elim_mext
tpsolve
grobner_mext
bernstein
quicklisp
store
"
 
for package in $packages
do
   build_one_package $package
done

