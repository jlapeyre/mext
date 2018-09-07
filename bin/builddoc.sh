#!/bin/bash

# build latex and pdf from maxdoc documentation

maxima="smaxima"

filename=./thirdparty
texfile=$filename.tex
pdffile=$filename.pdf
auxfile=$filename.aux
hauxfile=$filename.haux
tocfile=$filename.toc
htocfile=$filename.htoc
outfile=$filename.out
logfile=$filename.log

rm $auxfile $tocfile $outfile $logfile
$maxima -b "./builddoc.mac" && pdflatex $texfile && pdflatex $texfile

rm $auxfile $tocfile $outfile $logfile $hauxfile $htocfile
hevea $texfile && hevea $texfile

rm $auxfile $tocfile $outfile $logfile $hauxfile $htocfile
