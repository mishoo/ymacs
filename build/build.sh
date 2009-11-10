#! /bin/bash

# make sure the "yuicompressor" program exists and is in $PATH for
# minification to work.  That can be a little shell script that calls
# the YUI Compressor, or whatever.

TEMPDIR=`mktemp -d`
YEMPDIR="$TEMPDIR/Ymacs"
mkdir $YEMPDIR;

cd ..
cp -r src $YEMPDIR
cp -r test $YEMPDIR

cd "$YEMPDIR/src/js"
JSFILES=`cat loadfiles.txt`
rm loadfiles.txt

mkdir ../js-minified
cat $JSFILES > ../js-minified/all.js
cd ../js-minified
yuicompressor all.js > ymacs-min.js
rm all.js

cd $TEMPDIR
tar zcf ymacs.tar.gz Ymacs
mv ymacs.tar.gz /tmp
rm -rf $TEMPDIR
