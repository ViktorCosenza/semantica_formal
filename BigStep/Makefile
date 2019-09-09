CC=ghc
CFLAGS=-dynamic
execs=main


all: main

bigstep: BigStepSemantic.hs
	${CC} ${CFLAGS} BigStepSemantic.hs

main: bigstep main.hs 
	${CC} ${CFLAGS} main.hs

clean:
	rm -f ${execs} *.hi *.o
