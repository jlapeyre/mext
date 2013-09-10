#

maxima=$1

tardist="../mext-test.tar.gz"
mext_test_top="../mext_test_top"

set_do_all () {
    build_package_script="buildall1.mac"
    test_package_script="testall1.mac"
}

set_do_all

if [ ! -e $mext_test_top ]; then
  mkdir $mext_test_top
fi

mext_test_dir=$mext_test_top/$maxima

if [ ! -e $mext_test_dir ]; then
  mkdir $mext_test_dir
fi

rm -rf $mext_test_dir/*

echo tar xzf $tardist -C $mext_test_dir
tar xzf $tardist -C $mext_test_dir

cd $mext_test_dir/mext

if [ ! -e logfiles ]; then
  mkdir logfiles
fi

echo Building mext in `pwd`

echo Building mext_system for $maxima
cd ./mext_system; $maxima -b ibuild.mac &> ../logfiles/$maxima.mextsyslog; cd ..

echo Building mext packages for $maxima
$maxima -b $build_package_script &> logfiles/$maxima.mextlog

echo Testing mext packages for $maxima
$maxima -b $test_package_script &> logfiles/$maxima.testlog

echo Parsing build log for $maxima
./parse_testlog.pl build logfiles/$maxima.mextlog

echo Parsing test log for $maxima
./parse_testlog.pl test logfiles/$maxima.testlog
