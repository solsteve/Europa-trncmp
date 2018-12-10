#!/bin/bash

temp=$(basename $(pwd))

make install

case ${temp} in
    RELEASE)
        PROG=europa/bin/ftest_simple
        ;;
    DEBUG)
        PROG=europa/bin/ftest_simple
        ;;
    Europa*)
        PROG=RELEASE/europa/bin/ftest_simple
        ;;
    *)
        
esac

echo ""

i=0
s="0.0"
while [ $i -lt 10 ]; do
    $PROG > /dev/null 2> /tmp/check
    v=$(cat /tmp/check)
    s=$(echo "$s + $v" | bc)
    i=$(( $i + 1 ))
    echo "${i}: ${v}"
done

AT=$(echo "scale=5 ; ${s} / ${i}.0" | bc)
echo ""
echo "average run time ${AT} seconds"
echo ""

exit 0
