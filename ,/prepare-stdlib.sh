#!/usr/bin/env bash

mkdir ,,
mv community-build/community-projects/stdLib213/build/pack/lib/scala-library.jar ,,/input/ || exit
cd ,,/input || exit
rm -rf scala-library || exit
mkdir scala-library || exit
cd scala-library || exit
unzip ../scala-library.jar || exit
