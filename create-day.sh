#!/bin/bash

PACKAGE="uk/co/hadoopathome/adventofcode19"
TEMPLATE_DIR="templates"

if [ $# -eq 0 ]; then
    echo "Day must be given as an argument"
    exit 1
fi

day="day${1}"

echo "Creating files for ${day}"

cp -r ${TEMPLATE_DIR}/main src/main/scala/${PACKAGE}/${day}
cp -r ${TEMPLATE_DIR}/test src/test/scala/${PACKAGE}/${day}
cp -r ${TEMPLATE_DIR}/resource src/test/resources/${day}
