CC=ghc
CFLAGS=-dynamic
execs=main


all: main

small_step: Types.hs SmallStep.hs
	${CC} ${CFLAGS} main.hs

main: small_step main.hs 
	${CC} ${CFLAGS} main.hs

clean:
	rm -f ${execs} *.hi *.o
