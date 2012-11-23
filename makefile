SOURCE=$(wildcard *.hs)
INC_DIRS=-iHaskell-Web-Framework

WebServer: $(SOURCE) clean main

clean:
	@# Get the haskell web framework if it doesn't already exist
	@if [ ! -d "Haskell-Web-Framework" ]; then git clone git@github.com:TerranceN/Haskell-Web-Framework.git; fi
	@rm -f main main.exe

main:
	@ghc --make -O2 main.hs $(INC_DIRS)
