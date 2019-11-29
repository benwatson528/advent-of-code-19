#!/bin/bash

PACKAGE="uk/co/hadoopathome/adventofcode19"
TEMPLATE_DIR="templates"

if [ $# -eq 0 ]; then
    echo "Day must be given as an argument"
    exit 1
fi

unformatted_day=${1}
printf -v formatted_day "%02d" ${unformatted_day}
day="day${formatted_day}"


echo "Creating files for ${day}"

destination_main="src/main/scala/${PACKAGE}/${day}"
destination_test="src/test/scala/${PACKAGE}/${day}"
destination_resources="src/test/resources/${day}"

cp -r ${TEMPLATE_DIR}/main ${destination_main}
cp -r ${TEMPLATE_DIR}/test ${destination_test}
cp -r ${TEMPLATE_DIR}/resource ${destination_resources}


echo "Updating references inside files"

sed -i -e "s/\${internal_day}/${day}/" ${destination_main}/MainClass.scala
sed -i -e "s/\${internal_day}/${day}/" ${destination_test}/MainClassTest.scala
