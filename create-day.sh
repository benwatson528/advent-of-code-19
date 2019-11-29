#!/bin/bash

PACKAGE="uk/co/hadoopathome/adventofcode19"
TEMPLATE_DIR="templates"

if [ $# -ne 2 ]; then
    echo "Day and class name must be given as arguments"
    exit 1
fi

unformatted_day=${1}
printf -v formatted_day "%02d" ${unformatted_day}
day="day${formatted_day}"

class_name=${2}


echo "Creating files for ${day}"

destination_main="src/main/scala/${PACKAGE}/${day}"
destination_test="src/test/scala/${PACKAGE}/${day}"
destination_resources="src/test/resources/${day}"
main_class_name="${class_name}.scala"
test_class_name="${class_name}Test.scala"

mkdir ${destination_main}
cp ${TEMPLATE_DIR}/MainClass.scala ${destination_main}/${main_class_name}

mkdir ${destination_test}
cp ${TEMPLATE_DIR}/MainClassTest.scala ${destination_test}/${test_class_name}

mkdir ${destination_resources}
cp ${TEMPLATE_DIR}/input.txt ${destination_resources}


echo "Updating references inside files"

sed -i -e "s/\${internal_day}/${day}/" -e "s/\${internal_class_name}/${class_name}/" ${destination_main}/${main_class_name}
sed -i -e "s/\${internal_day}/${day}/" -e "s/\${internal_class_name}/${class_name}/" ${destination_test}/${test_class_name}
