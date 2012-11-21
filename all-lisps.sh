#

# build mext and all mext packages for several versions of maxima
# compiled with different lisps

#maximas="smaxima gmaxima clmaxima emaxima cmumaxima ccmaxima"
maximas="smaxima"
#maximas="cmumaxima"

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

# run rtests in the distribution folders
mext_tests () {
 for maxima in $maximas
     do
      $maxima -b testdoc.mac
 done
}

build_mext
build_mext_packages
#print_max_doc
