#!/bin/bash
mymake="../../../_build/default/demos/demoInterpret.exe"
rm -f f1 f2 upToDate

make no > f1
$mymake no > f2
echo ----

make no_rule >> f1
$mymake no_rule >> f2
echo ----

make rule_error >> f1
$mymake rule_error >> f2

rm -f upToDate
make upToDate >> f1
make upToDate >> f1
make is_up_to_date >> f1
rm -f upToDate
$mymake upToDate >> f2
$mymake upToDate >> f2
$mymake is_up_to_date >> f2
echo ----

rm -f upToDate
make is_up_to_date >> f1
make is_up_to_date >> f1
make nothing_to_do >> f1
rm -f upToDate
$mymake is_up_to_date >> f2
$mymake is_up_to_date >> f2
$mymake nothing_to_do >> f2
diff f1 f2
