#!/bin/bash
rm -f f1 f2
make > f1
make ab >> f1
make ba >> f1
mymake="../../../_build/default/demos/demoInterpret.exe" 
$mymake > f2
$mymake ab >> f2
$mymake ba >> f2
diff f1 f2
