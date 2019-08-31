# Portable Prolog System.

MODULES = prolog.o util.o catchint.o

prolog: $(MODULES)
	cc -o prolog $(MODULES) -lp2c

prolog.c: util.h catchint.h libname.h
util.o: cdefs.h
malloc.o: malloc.h

prolog.c: prolog.p
	p2c prolog.p

clean:
	rm prolog.c *.o

dist_clean: clean
	rm prolog

