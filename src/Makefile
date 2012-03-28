all:
	make grammar
	make haskell

grammar:
	cd gr; gf -make --output-format=haskell ResEng.gf ResSpa.gf ResGal.gf
	cd gr; gf -make --output-format=haskell Basics.gf BasicsEng.gf BasicsSpa.gf BasicsGal.gf
	cd gr; gf -make --output-format=haskell Time.gf TimeEng.gf TimeSpa.gf TimeGal.gf
	cd gr; gf -make --output-format=haskell Grammar.gf GrammarEng.gf GrammarSpa.gf GrammarGal.gf
	
haskell:
	ghc	-igr Main.hs --make
	strip Main

clean:
	rm -rf *.hi *.o Main gr/*.pgf gr/*.gfo gr/*.hs gr/*.hi gr/*.o

