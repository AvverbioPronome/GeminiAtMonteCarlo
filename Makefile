HC = ghc
HFLAGS  = -O2 -Wall -threaded -rtsopts
CC = gcc
CFLAGS = -O3 -march=native -fopenmp -fopenmp-simd -Wall

.PHONY: all clean deps-lua
all: deps-haskell deps-lua

# The Magic Pattern Rule
# "To build any file with no extension (%), look for a .hs file (%: %.hs)"
%: %.hs
	stack install :$@ --local-bin-path .

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%: %.c
	$(CC) $(CFLAGS) $< -o $@

clean:
	git clean -Xdf

deps-lua:
	luarocks install --tree lua_modules ./fun-scm-1.rockspec
