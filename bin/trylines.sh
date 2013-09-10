#maximas="maxima-5.31.0-sbcl-1.1.11 maxima-5.31.0-gcl-2.6.9  maxima-5.31.0-gcl-2.6.8  maxima-5.31.0-gcl-2.6.7 \
#         maxima-5.31.0-ccl-1.9 maxima-5.31.0-clisp-2.49 maxima-5.31.0-cmucl-20d maxima-5.31.0-ecl-12.12.1"

maximas="maxima-5.31.0-sbcl-1.1.11 maxima-5.31.0-gcl-2.6.8 maxima-5.31.0-ccl-1.9 maxima-5.31.0-clisp-2.49 maxima-5.31.0-cmucl-20d maxima-5.31.0-ecl-12.12.1"

#code="float(bfloat(1000!)^.01);"
code="(load(mext3), display2d:false, lisp_version);"

for maxima in $maximas
do
  echo $maxima
  $maxima --batch-string="$code"
done
