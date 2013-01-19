SOURCE=$(wildcard *.hs)
INC_DIRS=-iFramework

all: $(SOURCE) before main

before:
	@rm -f Main Main.exe

main:
	@ghc --make -O2 Main $(INC_DIRS)

clean:
	@rm -f *.o *.hi Main Main.exe
