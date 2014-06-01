# A dumb haskell makefile.

all:
	ghc --make fch.hs

clean:
	rm *.hi *.o FCH/*.hi FCH/*.o
