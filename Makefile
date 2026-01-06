HC = ghc
HFLAGS  = -O2 -Wall -threaded -rtsopts
CC = gcc
CFLAGS = -O3 -march=native -fopenmp -fopenmp-simd -Wall

.PHONY: all
all: deps-haskell deps-lua

# The Magic Pattern Rule
# "To build any file with no extension (%), look for a .hs file (%: %.hs)"
%: %.hs deps-haskell
	$(HC) $(HFLAGS) -o $@ $<

montecarlo_ffi: kernel.o deps-haskell montecarlo_ffi.hs 
	$(HC) $(HFLAGS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

parallel: parallel.c
	$(CC) $(CFLAGS) $< -o $@

clean:
	git clean -Xdf

deps-haskell:
	cabal install --lib random --package-env .
	cabal install --lib parallel --package-env .

deps-lua:
	luarocks install --tree lua_modules ./fun-scm-1.rockspec
