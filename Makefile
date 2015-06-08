.PHONY: clean

persistent-file: main.hs XMLParser.hs
	ghc -o persistent-file main.hs -i./

clean:
	rm -f *.o *.hi *.dyn_o *.dyn_hi persistent-file