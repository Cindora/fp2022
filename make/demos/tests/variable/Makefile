a=ab
b=ba

all :
	echo $(a)

$b :
	echo $b = $@

$(a) : $b
	echo $a : $^

