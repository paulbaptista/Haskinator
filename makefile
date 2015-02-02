# Autores: 
# Jesus Parra 10-10534
# Paul Baptista 10-10056

CC=ghc
Haskinator : Oraculo
	$(CC) Haskinator.hs

Oraculo : 
	$(CC) Oraculo.hs
	
clean :
	rm *.hi *.o *~ Haskinator