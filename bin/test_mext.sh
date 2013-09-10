#

# Test the mext distribution with several versions of
# maxima/lisp.
#
# * First, a git archive is made.
# * Next a copy of the archive is unpacked into a new
#   directory for each maxima.
# * Next, the distribution is build and tested in parallel
#   for each maxima
# * Finally a crude parsing script looks for errors or
#   success messages in the build and test logs.

# Run this script from the top level like this:
# ./bin/text_mext.sh

# put the archive of the master branch of the git repo here
tardist="../mext-test.tar.gz"
# unpack and build and test here
mext_test_top="../mext_test_top"

# build and test for these
maximas="
 maxima-5.31.0-sbcl-1.1.11 
 maxima-5.31.0-gcl-2.6.7
 maxima-5.31.0-gcl-2.6.8
 maxima-5.31.0-gcl-2.6.9
 maxima-5.31.0-ccl-1.9 
 maxima-5.31.0-clisp-2.49 
 maxima-5.31.0-cmucl-20d 
 maxima-5.31.0-ecl-12.12.1
"
#maximas="smaxima gmaxima"

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

 cd $mext_test_dir/mext
 
 if [ ! -e logfiles ]; then
     mkdir logfiles
 fi

 echo Building mext in `pwd`

 echo Building mext_system for $thismaxima
 cd ./mext_system; $thismaxima -b ibuild.mac &> ../logfiles/$thismaxima.mextsyslog; cd ..

 echo Building mext packages for $thismaxima
 $thismaxima -b $build_package_script &> logfiles/$thismaxima.mextlog

 echo Testing mext packages for $thismaxima
 $thismaxima -b $test_package_script &> logfiles/$thismaxima.testlog

 echo Parsing build log for $thismaxima
 ./parse_testlog.pl build logfiles/$thismaxima.mextlog

 echo Parsing test log for $thismaxima
 ./parse_testlog.pl test logfiles/$thismaxima.testlog

}

################################


################################

git archive -o ../../mext-test.tar.gz --prefix="mext/" HEAD

for maxima in $maximas
do
    echo Building and testing mext for $maxima
    perform_one_test $maxima&
done


#perform_one_test $maxima
