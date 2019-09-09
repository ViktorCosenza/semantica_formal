CC=ghc
CFLAGS=-dynamic
execs=main


all: main

big_step: Types.hs BigStepSemantic.hs
	${CC} ${CFLAGS} main.hs

main: big_step main.hs 
	${CC} ${CFLAGS} main.hs

clean:
	rm -f ${execs} *.hi *.o
