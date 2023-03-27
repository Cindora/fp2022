#!/bin/bash
mymake="../../../_build/default/demos/demoInterpret.exe"
rm -f f1 f2 t3s all

make > f1
make t3s >> f1
echo - >> f1

touch t3
make t3s >> f1
echo - >> f1

touch t3
make >> f1
echo - >> f1

rm t3s
make >> f1
echo - >> f1

rm all
make >> f1

#-------

rm -f t3s all
$mymake > f2
$mymake t3s >> f2
echo - >> f2

touch t3
$mymake t3s >> f2
echo - >> f2

touch t3
$mymake >> f2
echo - >> f2

rm t3s
$mymake >> f2
echo - >> f2

rm all >> f2
$mymake >> f2

diff f1 f2
