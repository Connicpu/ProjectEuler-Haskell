@echo off
mkdir bin 2> nul
FOR %%f in (0*.hs) DO ghc -Wall -outputdir obj/%%f/ %%f -o bin/%%f.exe