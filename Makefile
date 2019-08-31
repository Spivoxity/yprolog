# Portable Prolog System.

prolog: prolog.p
	fpc -o$@ $<

clean:
	rm -f prolog prolog.o

