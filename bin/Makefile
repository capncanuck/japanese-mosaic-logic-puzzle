all: mosaic

mosaic: Main.hs
	ghc -o mosaic $<

cucumber: mosaic
	- cd .. ; cucumber features/mosaic.feature

clean:
	$(RM) mosaic *.o *.hi
