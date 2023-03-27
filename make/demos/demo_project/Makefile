CXX=g++
CPPFLAGS=-Wall
al=all

all : a.out

a.out : main.o gcd.o
	echo build $@
	$(CXX) $(CPPFLAGS) -o $@ main.o gcd.o

main.o : main.cpp gcd.cpp gcd.h
	echo build $@
	$(CXX) $(CPPFLAGS) -c -o $@ main.cpp

gcd.o : gcd.h gcd.cpp
	echo build $@
	$(CXX) $(CPPFLAGS) -c -o $@ gcd.cpp

all : hello_world1 hello_world2

hello_world1 hello_world2 : hello_world1.cpp hello_world2.cpp
	echo build $@
	$(CXX) $(CPPFLAGS) -o $@ $@.cpp 

$(al):
	@echo $@: $^ was building $$

clean :
	rm -f *.o a.out hello_world1 hello_world2 some

with_error:
	c

some :
	touch some

nothing : some

no_rule: no
