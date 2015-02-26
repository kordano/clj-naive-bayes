#!/bin/bash
TMPFILE=`tempfile`
wget http://openclassroom.stanford.edu/MainFolder/courses/MachineLearning/exercises/ex6materials/ex6DataEmails.zip -O $TMPFILE
unzip $TMPFILE
rm $TMPFILE
rm README
