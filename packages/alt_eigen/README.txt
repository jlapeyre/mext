alt_eigen packaged for mext

alt_eigen is written by Barton Willis. See alt_eigen_code.mac.
Barton Willis is not responsible for this packaging.

If mext is installed, build and install this package with
(%i1) load("./ibuild.mac");
(%i2) mext_test();

The code is split into two files, one of which is compiled.
To install only source, change ':load-only nil' to ':load-only t' everywhere
in alt_eigen.system before building. But, I saw no change in
the number of tests passed with compiled vs.  source code.
rtest runs about 30% faster when code is compiled.

Compiling alt_eigen was disabled for ecl, because, on the
following platform, compiling failed.

 Maxima version: "5.28.0"
 Maxima build date: "2012-11-13 05:50:23"
 Host type: "x86_64-unknown-linux-gnu"
 Lisp implementation type: "ECL"
 Lisp implementation version: "11.1.1"

To run the code in a subsequent maxima session:

(%i1) load(mext);
(%o1)                  /home/jlapeyre/.maxima/mext.lisp
(%i2) mext_require(alt_eigen);
(%o2) /home/user/.maxima/mext/v5.28.0-sbcl-v1.0.57.0.debian/alt_eigen.mac
(%i3) mext_test(alt_eigen);
