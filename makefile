SOURCE=$(wildcard *.hs)
INC_DIRS=-iHaskell-Web-Framework

WebServer: $(SOURCE) clean main

clean:
	rm -f main main.exe

main:
	ghc --make -O2 main.hs $(INC_DIRS)
