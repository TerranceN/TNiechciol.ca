.PHONY: main

SOURCE=$(wildcard *.hs)
INC_DIRS=-iFramework

all: $(SOURCE) main

main:
	@ghc --make -O2 Main -o TMPMain $(INC_DIRS)
	mv TMPMain Main

clean:
	@rm -f *.o *.hi Main Main.exe
