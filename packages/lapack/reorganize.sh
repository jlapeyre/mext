#!/bin/bash

# copy all the blas source files to one source file.
# do the same with lapack source.

cd blas
mv blas-package.lisp ..
cat *.lisp > ../blas_all.lisp

cd ../lapack
if [ ! -e hide ]; then
    mkdir hide/
fi
mv lapack-package.lisp hide/
cat *.lisp > ../lapack_all.lisp
