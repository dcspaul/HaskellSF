#!/bin/sh

SRCFILE=$1
TMPFILE=$2
SFPARSER=/Users/paul/Work/Playground/sfParser/GitHub/sfParser

$SFPARSER -json $SRCFILE 2>$TMPFILE# | grep -v '^(' >$TMPFILE 
STATUS=$?
cat $TMPFILE# >>$TMPFILE
if grep -q 'Exception' $TMPFILE ; then
	grep Exception <$TMPFILE >$TMPFILE#
	sed 's/.*Exception: //' <$TMPFILE# >$TMPFILE
fi
rm -f $TMPFILE#
exit $STATUS
