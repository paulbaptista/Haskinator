# README #

Instrucciones de uso:

* Para compilar **Haskinator** ejecute:
>`make`
* Para correr **Haskinator** ejecute:
>`./Haskinator`
* Para borrar **Haskinator** ejecute:
>`make clean`

* Para crear la documentación ejecute:
>`make docs`
* Para borrar la documentación ejecute:
> `make cleandocs`

* Para compilar Haskinator y crear la documentación ejecute:
>`make all`
* Para eliminar la documentación y binarios creados ejecute:
>`make cleanall`


Recomendamos el uso de la versión más reciente de *haskell-platform* o de *ghc*,
para la correcta compilación del programa.

## HASKINATOR ##

>*En las profundidades del bosque de los mil y un monads, en la misteriosa cuna
del gran río Curry, habita entre paredes de piedra el poderoso oráculo
Haskinator. Con su vasto conocimiento, Haskinator es capaz de adivinar los
pensamientos de aquellos que lo visitan, sin más que unas pocas y precisas
preguntas. El misterioso oráculo se ha hecho conocido y afamado por su gran
poder, sorprendiendo y maravillando a cada viajero con la suerte de
presenciarlo.*

>*Sin embargo, Haskinator presiente que sus poderes se desvanecen con el tiempo
y teme defraudar a quienes expectantes desean ser maravillados por sus
predicciones. Con premura comienza a indagar maneras de conservar su preciado
don antes que se le escape por completo. Entonces recuerda la historia de uno de
sus tantos visitantes. El visitante, agradecido y sorprendido con el oráculo, le
había contado sobre un grupo de talentosos estudiantes que aprendían a programar
en Haskell, el lenguaje de dioses y oráculos de eras pasadas.*

>*Usando el poco poder que aún le quedaba, el gran Haskinator se comunica por
medio de la telepatía con los profesores del Laboratorio de Lenguajes en la
Universidad Simón Bolívar, para que  encomienden a sus estudiantes la creación
de un programa que logre simular sus afamadas capacidades de predicción.*

###Opciones del menú:###

Haskinator dispone de las siguientes opciones que se describen a continuación:

1. Crear un oráculo nuevo: Borra el Oráculo actual y crea uno nuevo vacío.

2. Predecir: Comienza el juego, **Haskinator** le hará preguntas al usuario hasta a
una predicción, el usuario indicara si es correcta o no.
			   
3. Persistir: Guardará el oráculo actual en un archivo con el nombre indicado el
usuario.
                
4. Cargar: Cargará un oráculo a partir de un archivo indicado por el usuario.
                 
5. Consultar pregunta crucial: El usuario introducirá dos predicciones y si
pertenecen al oráculo, se imprimirá la crucial que llevaría a  entre predicción
o la otra.
								 
6. Consultar Estadísticas: Mostrara el mínimo, máximo y promedio de preguntas el
Oráculo debe hacer para llegar a una predicción.

***

##Detalles de implementación##

### Para la lectura del archivo:###

Además de verificar la existencia del mismo, **Haskinator** verifica que
efectivamente se esté cargando un *Oraculo*; haciendo uso de la función *readIO*,
que ha de devolver un *IO Oraculo*, si la representación es la correcta, o
lanzará un error si no lo es; para capturar el error usamos la función
*tryIOError* que devuelve un *Either IOError Oraculo*.
Por lo tanto, el flujo de ejecución de la lectura consiste en: pedir al
usuario el nombre del archivo a cargar, intentar hacer la carga a partir de los
datos obtenidos del archivo en cuestión, y entonces verificar si se hizo la
lectura o se produjo un error, procediendo a informarle al usuario de lo
sucedido y continuando así, de modo robusto, con la ejecución adecuada del
programa.

###Ingreso de datos:###

Por favor evite usar comillas dobles ( *" "* ) al ingresar predicciones o
preguntas a **Haskinator**, debido a que al guardar el *Oraculo* en un archivo
la representación del mismo contendré dichas comillas y **Haskinator** no podra
luego cargarlo de nuevo, pues presentará errores a la hora de parsear los
Strings. En su lugar puede usar comillas simples ( *' '* ) o comillas circunflejas
( *« »* ).
Esperamos ofrecerle proximamente ofrecer un fix a este problema.

***

##Autores##

* Jesús Parra (10-10534)

* Paul Baptista (10-10056)

##Contáctenos##

* adolfo@ldc.usb.ve
* paul@ldc.usb.ve

##Acerca de:##
Código fuente de nuestra implementación de **Haskinator**, que satisface los
requerimientos establecidos para la entrega del mismo como primer proyecto de
**Laboratorio de Lenguajes de Programación (*CI3661*)**

** *Versión 1.5.0* **