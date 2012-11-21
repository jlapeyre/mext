alt_eigen packaged for mext

alt_eigen is written by Barton Willis. See alt_eigen_code.mac.
Barton Willis is not responsible for this packaging.

If mext is installed, build and install this package with
(%i1) load("./ibuild.mac");
(%i2) mext_test();

By default only compiled code is installed. To install only
source, change ':load-only nil' to ':load-only t' in
alt_eigen.system before building. But, I saw no change in
the number of tests passed with compiled vs.  source code.

To run the code in a subsequent maxima session:

(%i1) load(mext);
(%o1)                  /home/jlapeyre/.maxima/mext.lisp
(%i2) mext_require(alt_eigen);
(%o2) /home/user/.maxima/mext/v5.28.0-sbcl-v1.0.57.0.debian/alt_eigen.mac
(%i3) mext_test(alt_eigen);
