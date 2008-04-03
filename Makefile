# My comment

GHC=ghc

GFLAGS=--make

all: tictactoe

tictactoe:
	$(GHC) $(GFLAGS) tictactoe.hs

clean: clean_binaries
	rm -rf *.hi *.o *~

clean_binaries:
	rm -rf tictactoe
