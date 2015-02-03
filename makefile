# Autores: 
# Jesus Parra 10-10534
# Paul Baptista 10-10056

HC = ghc
DC = haddock -h -o docs

Haskinator : Oraculo
	$(HC) Haskinator.hs

Oraculo : 
	$(HC) Oraculo.hs

docs :
	$(DC) *.hs

all : Haskinator Oraculo docs


	
cleandocs :
	rm -r docs

clean :
	rm *.hi *.o Haskinator

cleanall : clean cleandocs

.PHONY : clean cleandocs

