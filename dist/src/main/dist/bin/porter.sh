#!/bin/bash

# find utf8 locale and set it
LOC=`locale -a | grep -i utf8 | head -n1`
if [ -n "$LOC" ]; then
  export LC_ALL=${LOC}
fi

# find working dir and cd into it
cd `dirname $0`/..

# create classpath param
CPATH=""
SCALALIB=""
for f in lib/*; do
  if [[ ${f} =~ scala-library.* ]]; then
    SCALALIB="`pwd`/${f}"
  else
    CPATH=${CPATH}:"`pwd`/$f"
  fi
done

#check for porter.conf
CONFOPT=
if [ -r etc/porter.conf ]; then
  CONFOPT="-Dconfig.file=etc/porter.conf"
fi

mkdir temp 2> /dev/null

JAVA_OPTS="-server -Xmx512M -Djava.awt.headless=true \
 -Djava.io.tmpdir=`pwd`/temp \
 -Dlogback.configurationFile=`pwd`/etc/logback.xml $CONFOPT"

JCLASSPATH=${SCALALIB}:${CPATH#?}:plugins/*
java ${JAVA_OPTS} -Xbootclasspath/a:${JCLASSPATH} -jar bin/porter.jar

cd -