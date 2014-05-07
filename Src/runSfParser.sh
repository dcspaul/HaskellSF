#!/bin/sh

# run the sfparser SF compiler on the specified source file
# put the JSON output in the specified destination file
# munge any error messages and include them in the output file
# (so that they can be more easily compared with messages from hsf)

SRCFILE=$1
DSTFILE=$2
SFPARSER=$3

test -z "$SRCFILE" && echo "usage: runSfParser source dest [sfParser]" >&2 && exit 2
test -z "$DSTFILE" && echo "usage: runSfParser source dest [sfParser]" >&2 && exit 2
test -z "$SFPARSER" && SFPARSER=sfparser
test ! -x "$SFPARSER" && echo "can't find $SFPARSER" >&2 && exit 2

$SFPARSER -json $SRCFILE 2>$DSTFILE# | grep -v '^(' >$DSTFILE
cat $DSTFILE# >>$DSTFILE || exit 2
if grep -q 'Exception' $DSTFILE ; then
	grep Exception <$DSTFILE >$DSTFILE# || exit 2
	sed 's/.*Exception: //' <$DSTFILE# >$DSTFILE || exit 2
fi
rm -f $DSTFILE# || exit 2
exit 0
