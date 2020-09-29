#!/usr/bin/env bash

dir="out/stdLib213/scala-library"
find "$dir" -type f -iname '*.tasty' -print0 | (
    declare -a arr
    while read -d '' -r line
    do
        _1=${line%.tasty}
        _2=${_1#out/stdLib213/scala-library/}
        _3=${_2//\//.}
        # if ! [[ $_3 == "scala.package" ]]
        # then 
	arr+=("$_3")
        # fi
        # echo "$_3"
    done
    # echo "${arr[@]}"
    bin/dotc -classpath "$dir" -from-tasty "${arr[@]}"
)
