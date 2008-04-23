# My comment

GHC=ghc

GFLAGS=-XMultiParamTypeClasses --make

all: runtictactoe

runtictactoe:
	$(GHC) $(GFLAGS) runtictactoe.hs

runkalah:
	$(GHC) $(GFLAGS) runkalah.hs

clean: clean_binaries
	rm -rf *.hi *.o *~

clean_binaries:
	rm -rf runtictactoe runkalah
