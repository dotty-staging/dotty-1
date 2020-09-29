#!/usr/bin/env bash

outdir=out/stdLib213

mkdir -p "$outdir"
cp community-build/community-projects/stdLib213/build/pack/lib/scala-library.jar "$outdir" || exit
cd "$outdir" || exit
rm -rf scala-library || exit
mkdir scala-library || exit
cd scala-library || exit
unzip ../scala-library.jar || exit
