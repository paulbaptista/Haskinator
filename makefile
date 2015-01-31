CC=ghc
Haskinator : Oraculo
	$(CC) Haskinator.hs

Oraculo : 
	$(CC) Oraculo.hs
	
clean :
	rm *.hi *.o Haskinator