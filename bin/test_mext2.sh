#!/bin/bash
# NOTE!!: This must be bash, not sh

# USAGE:
# From the mext top level directory run
# ./bin/text_mext.sh
#
# Test the mext distribution with several versions of
# maxima/lisp.
#
# * First, a git archive is made.
# * Next a copy of the archive is unpacked into a new
#   directory for each maxima.
# * Next, the distribution is build and tested in parallel
#   for each maxima.
#   This version of the script calls the script
#   install_mext.sh, which builds and installs each
#   package in a separate maxima process.
# * Finally a crude parsing script looks for errors or
#   success messages in the build and test logs.

# unpack and build and test here
mext_test_top="../mext_test_top"
# put the archive of the master branch of the git repo here
tardist="$mext_test_top/mext-test.tar.gz"

mext_install_level="5"

# errors in these
#  maxima-5.31.0-gcl-2.6.7
#  maxima-5.31.0-gcl-2.6.9

# build and test for these
 maximas="
  maxima-5.41.0-sbcl-1.4.6
 "

 # maximas="
 #  maxima-5.31.1-sbcl-1.1.11
 #  maxima-5.31.1-gcl-2.6.8
 #  maxima-5.31.0-ccl-1.9
 #  maxima-5.31.0-clisp-2.49
 #  maxima-5.31.0-cmucl-20d
 #  maxima-5.31.0-ecl-12.12.1
 # "

 
#################################################

set_do_all () {
    build_package_script="buildall1.mac"
    test_package_script="testall1.mac"
}

set_do_all


perform_one_test () {

 thismaxima=$1

 if [ ! -e $mext_test_top ]; then
     mkdir $mext_test_top
 fi

 mext_test_dir=$mext_test_top/$thismaxima

 if [ ! -e $mext_test_dir ]; then
     mkdir $mext_test_dir
 fi

 rm -rf $mext_test_dir/*

 echo tar xzf $tardist -C $mext_test_dir
 tar xzf $tardist -C $mext_test_dir

# if [ ! -e logfiles ]; then
#     mkdir logfiles
# fi

 cd $mext_test_dir/mext

 echo Building mext in `pwd`

# echo Building mext_system for $thismaxima
# cd ./packages/mext_system; $thismaxima -b ibuild.mac &> ../../../$thismaxima.mextsyslog; cd ..

# echo Building mext packages for $thismaxima
# $thismaxima -b ../bin/$build_package_script &> ../../$thismaxima.mextlog

./install_mext.sh $thismaxima $mext_install_level &> ../$thismaxima.mextlog

 echo Testing mext packages for $thismaxima
 $thismaxima -b ./bin/$test_package_script &> ../$thismaxima.testlog

 echo Parsing build log for $thismaxima
 ./bin/parse_testlog.pl build ../$thismaxima.mextsyslog

 echo Parsing package build log for $thismaxima
 ./bin/parse_testlog.pl build ../$thismaxima.mextlog

 echo Parsing test log for $thismaxima
 ./bin/parse_testlog.pl test ../$thismaxima.testlog

}

################################

echo Building tar archive
echo git archive -o $tardist --prefix="mext/" HEAD
git archive -o $tardist --prefix="mext/" HEAD

for maxima in $maximas
do
    echo Building and testing mext for $maxima
    perform_one_test $maxima&
done
