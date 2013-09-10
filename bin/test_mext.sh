#

maximas="maxima-5.31.0-sbcl-1.1.11 maxima-5.31.0-gcl-2.6.8 maxima-5.31.0-ccl-1.9 maxima-5.31.0-clisp-2.49 maxima-5.31.0-cmucl-20d maxima-5.31.0-ecl-12.12.1"
#maximas="smaxima gmaxima"

################################

git archive -o ../mext-test.tar.gz --prefix="mext/" HEAD

for maxima in $maximas
do
    echo Building and testing mext for $maxima
    ./test_mext_1.sh $maxima&
done
