#!/bin/sh

# Build mext and all mext packages for several versions of maxima
# compiled with different lisps. For unix-like OS's.

# Names of the executable maxima programs. These are soft
# links in /usr/local/bin to builds of maxima with differenct
# lisp implementations. Allegro worked with an earlier version
# of this software, as well.
maximas="smaxima gmaxima clmaxima emaxima cmumaxima ccmaxima"
#maximas="smaxima"

# Build just the mext_system
build_mext () {
 for maxima in $maximas
   do
    echo Building mext_system for $maxima
    cd ./mext_system; $maxima -b ibuild.mac; cd ..
 done
}

# build packages packaged with the mext system
# Which packages are built is specified in the
# file buildall.mac
build_mext_packages () {
 for maxima in $maximas
     do
      echo Building all packages for $maxima
      $maxima -b buildall.mac
 done
}

# print the big document page
print_max_doc () {
 for maxima in $maximas
     do
      $maxima -b testdoc.mac
 done
}

build_mext
build_mext_packages
#print_max_doc
