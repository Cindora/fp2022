#!/bin/bash
rm -f f1 f2
make a > f1
make b >> f1
make x >> f1
mymake="../../../_build/default/demos/demoInterpret.exe" 
$mymake a > f2
$mymake b >> f2
$mymake x >> f2
diff f1 f2
