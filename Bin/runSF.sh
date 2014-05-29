#!/bin/sh

# run an external SF compiler on the specified source file
# put the JSON output in the specified destination file
# munge any error messages and include them in the output file
# (so that they can be more easily compared with messages from hsf)

COMPILER=$1
SRCFILE=$2
DSTFILE=$3
CPATH=$4

usage() {
	echo "usage: runSF scala|ocaml source dest path-to-compiler" >&2
	exit 2
}

doScala() {

	$CPATH -json $SRCFILE 2>$DSTFILE# | grep -v '^(' >$DSTFILE
	cat $DSTFILE# >>$DSTFILE || exit 2
	if grep -q 'Exception' $DSTFILE ; then
		grep Exception <$DSTFILE >$DSTFILE# || exit 2
		sed 's/.*Exception: //' <$DSTFILE# >$DSTFILE || exit 2
	fi
	rm -f $DSTFILE# || exit 2
}

doOCaml() {

	$CPATH -json $SRCFILE 2>$DSTFILE# >$DSTFILE
	cat $DSTFILE# >>$DSTFILE || exit 2
	rm -f $DSTFILE# || exit 2
}

doHP() {

	$CPATH -v $SRCFILE 2>&1 \
		|grep -v 'at org.smartfrog' \
		|grep -v 'Parser - SmartFrog' \
		|grep -v '(C) Copyright' \
		|grep -v 'SFHOME undefined' \
		|grep -v '^\s*at org.smartfrog' \
		|grep -v '^\s*$' \
		>$DSTFILE
}

noCompiler() {

	case $COMPILER in
		scala)	TYPE=scala ; VAR=SF_SCALA_COMPILER ;;
		ocaml)	TYPE=ocaml ; VAR=SF_OCAML_COMPILER ;;
		hp)	TYPE=hp ; VAR=SF_HP_COMPILER ;;
	esac
		
	echo "can't find executable \"$1\" for external $TYPE compiler" >&2
	echo "try setting $VAR in options.mk or the environment" >&2
	exit 2
}

test -z "$SRCFILE" && usage
test -z "$DSTFILE" && usage
test -z "$CPATH" && usage
test ! -x "$CPATH" && noCompiler "$CPATH"

# change to the directory containing the source file
# so that #includes get interpreted in the same way
cd `dirname $SRCFILE`

# then use the relative filename
SRCFILE=`basename $SRCFILE`

# check for excluded tests
# this is necessary because soem compilers fail to terminate
# for some inputs (eg. the HP compiler on recursive includes)
if grep "<<<< not $COMPILER >>>>" $SRCFILE >$DSTFILE ; then
	exit 0
fi

# execute the compiler
case $COMPILER in
	scala)	doScala ;;
	ocaml)	doOCaml ;;
	hp)		doHP ;;
	*)		usage ;;
esac

exit 0
