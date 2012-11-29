#
# Use bash for now

# Build mext and all mext packages for several versions of maxima
# compiled with different lisps. For unix-like OS's.
# An alternative for a single lisp is to use the maxima
# scripts in buildallx.mac

# Names of the executable maxima programs. These are soft
# links in /usr/local/bin to builds of maxima with differenct
# lisp implementations. Allegro worked with an earlier version
# of this software, as well.

maximas="smaxima gmaxima clmaxima emaxima cmumaxima ccmaxima"
#maximas="emaxima"

# Build just the mext_system
build_mext () {
    for maxima in $maximas
    do
        echo Building mext_system for $maxima
        cd ./mext_system; $maxima -b ibuild.mac &> ../logfiles/$maxima.mextsyslog; cd ..
    done
}

# build packages packaged with the mext system
# Which packages are built is specified in the
# file buildall.mac
build_mext_packages () {
    for maxima in $maximas
    do
        echo Building all packages for $maxima
        $maxima -b buildall.mac &> logfiles/$maxima.mextlog
    done
}

test_mext_packages () {
    for maxima in $maximas
    do
      echo Testing packages for $maxima
      $maxima -b testall.mac &> logfiles/$maxima.testlog
    done
}

parse_test_logs () {
    for maxima in $maximas
    do
      echo Parsing tests for $maxima
      ./parse_testlog.pl test logfiles/$maxima.testlog
    done
}

parse_build_logs () {
    for maxima in $maximas
    do
      echo Parsing tests for $maxima
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

#build_mext
#build_mext_packages
test_mext_packages
parse_test_logs
