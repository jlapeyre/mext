# Lapack for Maxima packaged for mext

## Sbcl

With sbcl, you need to ask for more memory in order to
compile lapack. (probably other ways to solve this.)

smaxima -X '--dynamic-space-size 2048'

## Preparing source for build

Run reorganize.sh to cat all blas code to one file and
all lapack code to one file and move a file or two out
of the way.

## Unused files:

blas-package.lisp
lapack-lisp.system -- only for makeing lisp from fortran
lapack.mac.orig -- original non-mext file for loading lapack
lapack.system -- we instead use lapack-mext.system
load-lapack.lisp -- we use mext instead
