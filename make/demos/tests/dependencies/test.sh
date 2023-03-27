#!/bin/bash
rm -f f1 f2
touch b_file.txt
make a > f1
make b >> f1
make a_file >> f1
make b_file >> f1
mymake="../../../_build/default/demos/demoInterpret.exe" 
$mymake a > f2
$mymake b >> f2
$mymake a_file >> f2
$mymake b_file >> f2
diff f1 f2
