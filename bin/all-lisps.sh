#
# Use bash for now

# Script for testing mext and packages under *nix lisp implementations.

# Build mext and all mext packages for several versions of maxima
# compiled with different lisps. For unix-like OS's.
# An alternative for a single lisp is to use the maxima
# scripts in buildallx.mac

# Names of the executable maxima programs. These are soft
# links in /usr/local/bin to builds of maxima with differenct
# lisp implementations. Allegro worked with an earlier version
# of this software, as well.

set_do_all () {
    build_package_script="buildall1.mac"
    test_package_script="testall1.mac"
}

set_do_essential () {
    build_package_script="build_essential.mac"
    test_package_script="test_essential.mac"
}

set_do_one () {
    build_package_script="buildone.mac"
    test_package_script="testone.mac"
}


#############################
#maximas="smaxima smaxima-28 smaxima-31 gmaxima gmaxima-5.28.0-2.6.7 clmaxima emaxima cmumaxima ccmaxima ccmaxima-30"

maximas="maxima-5.31.0-sbcl-1.1.11 maxima-5.31.0-gcl-2.6.8 maxima-5.31.0-ccl-1.9 maxima-5.31.0-clisp-2.49 maxima-5.31.0-cmucl-20d maxima-5.31.0-ecl-12.12.1"
#maximas="ccmaxima"
#maximas="smaxima gmaxima"
#maximas="maxima-5.31.0-gcl-2.6.7"

#maximas="smaxima smaxima-28 smaxima-31 gmaxima gmaxima-5.28.0-2.6.7 clmaxima emaxima cmumaxima ccmaxima ccmaxima-30"
#maximas="smaxima"
#maximas="gmaxima-5.31-2.6.8"

set_do_all
#set_do_essential

#build_package_script="buildall1.mac"
#build_package_script="build_essential.mac"
#build_package_script="buildone.mac"

#test_package_script="testall1.mac"
#test_package_script="test_essential.mac"
#test_package_script="testone.mac"
#############################


# Build just the mext_system
# Note that if mext system has already been loaded, eg in your startup
# file, this will probably fail.
build_mext () {
    for maxima in $maximas
    do
        echo Building mext_system for $maxima
        cd ./mext_system; $maxima -b ibuild.mac &> ../logfiles/$maxima.mextsyslog; cd ..
    done
}

# build packages packaged with the mext system
# Which packages are built is specified in the
# file buildall1.mac
build_mext_packages () {
    for maxima in $maximas
    do
        echo Building all packages for $maxima
        $maxima -b $build_package_script &> logfiles/$maxima.mextlog
    done
}

# for testing changes to a single package
build_one_mext_package () {
    for maxima in $maximas
    do
        echo Building one package for $maxima
        $maxima -b buildone.mac &> logfiles/$maxima.mextlog
    done
}

test_mext_packages () {
    for maxima in $maximas
    do
      echo Testing packages for $maxima
      $maxima -b $test_package_script &> logfiles/$maxima.testlog
    done
}

parse_test_logs () {
    for maxima in $maximas
    do
      echo Parsing test log for $maxima
      ./parse_testlog.pl test logfiles/$maxima.testlog
    done
}

parse_build_logs () {
    for maxima in $maximas
    do
      echo Parsing build log for $maxima
      ./parse_testlog.pl build logfiles/$maxima.mextlog
    done
}

# print the big document page
print_max_doc () {
 for maxima in $maximas
     do
      echo $maxima -b testdoc.mac 
 done
}

# don't need this
#build_one_mext_package

mkdir "./logfiles"

build_mext
build_mext_packages
parse_build_logs

test_mext_packages
parse_test_logs

