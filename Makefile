all:
	gcc -g -std=gnu99 -Wall $(pkg-config --cflags --libs rpm) -c rpm_stub.c
	hsc2hs --cflag=-std=gnu99 --lflag=-lrpm --lflag=-lrpmio Rpm.hsc
	cabal build
