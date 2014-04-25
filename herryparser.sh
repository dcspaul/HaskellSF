#!/bin/sh

SRCFILE=$1
TMPFILE=$2
SFPARSER=/Users/paul/Work/Playground/sfParser/GitHub/sfParser

cp /dev/null $TMPFILE
cp /dev/null $TMPFILE.err

$SFPARSER -json $SRCFILE 2>$TMPFILE.err | grep -v '^(' >$TMPFILE 
STATUS=$?
cat $TMPFILE.err >>$TMPFILE
if grep -q 'Exception' $TMPFILE ; then
	grep Exception <$TMPFILE >$TMPFILE+
	sed 's/.*Exception: //' <$TMPFILE+ >$TMPFILE
	rm $TMPFILE+
fi
rm -r $TMPFILE.err
exit $STATUS
