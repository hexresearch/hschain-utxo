#!/bin/sh

die () {
   echo "*** mkconfig: $*" 1>&2
   exit 1
}

[ -f makefile ] || die "can not find 'makefile'"

cat<<EOF
/**************************************************************/
/* Automatically generated by './mkconfig.sh'.  Do note edit! */
/**************************************************************/
EOF

echo "#define YALS_OS \"`uname -srmn`\""
echo "#define YALS_COMPILED \"`date`\""
echo "#define YALS_RELEASED \"Fr 1. Mär 05:17:48 CET 2019\""
echo "#define YALS_VERSION \"03v\""
echo "#define YALS_ID \"b1316746d40f72f147cde3f4ec54fadbe0730427\""
