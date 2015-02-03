Proyecto 1 del Laboratorio de Lenguajes de Programacion

Integrantes:
Jesus Parra 10-10534
Paul Baptista 10-10056

Instrucciones de uso:

Para compilar Haskinator ejecute : 'make'
Para ejecutar Haskinator ejecute : './Haskinator'
Para borrar Haskinator ejecute : 'make clean'

Para crear la documentación ejecute: 'make docs'
Para borrar la documentación ejecute: 'make cleandocs'

Para compilar Haskinator y crear la documentación ejecute: 'make all'
Para borrar la documentación y binarios creados ejecute: 'make cleanall'

Opciones del menu:
Haskinator dispone de las siguientes opciones que se describen a continuacion

1) Crear un oraculo nuevo. : Borra el Oraculo actual y crea uno nuevo vacio

2) Predecir. : Comienza el juego, Haskinator le hara preguntas al usuario hasta
			   llegar a una prediccion, y el usuario indicara si es correcta o 
			   no.
			   
3) Persistir. : Guardara el oraculo actual en un archivo con el nombre indicado
                por el usuario.
                
4) Cargar. :    Cargara un oraculo apartir de un archivo indicado por el usuario

5) Consultar pregunta crucial. : El usuario introducira dos predicciones y si
								 ambas pertenecen al oraculo, se imprimira la
								 pregunta crucial que llevaria a decidir entre 
								 una prediccion o la otra.
								 
6) Consultar Estadísticas.: Mostrara el minimo, maximo y promedio de preguntas
							que el Oraculo debe hacer para llegar a una 
							prediccion.


Detalles de la implementacion:

-Para la lectura del archivo:
   Además de verificar la existencia del mismo, Haskinator verifica que
efectivamente se esté cargando un Oraculo; haciendo uso de la función "readIO",
que ha de devolver un "IO Oraculo", si la representación es la correcta, o
lanzará un error si no lo es; para capturar el error usamos la funcion
"tryIOError" que devuelve un "Either IOError Oraculo".
   Por lo tanto, el flujo de ejecución de la lectura consiste en: pedir al
usuario el nombre del archivo a cargar, intentar hacer la carga a partir de los
datos obtenidos del archivo en cuestión, y entonces verificar si se hizo la
lectura o se produjo un error, procediendo a informarle al usuario de lo
sucedido y continuando así, de modo robusto, con la ejecución adecuada del
programa.

-Ingreso de datos:
    Por favor evite usar comillas dobles ( " ) al ingresar predicciones o
preguntas a Haskinator, debido a que al guardar el "Oraculo" en un archivo
la representación del mismo contendré dichas comillas y Haskinator no podra
luego cargarlo de nuevo, pues presentará errores a la hora de parsear los
Strings. En su lugar puede usar comillas simples (') o comillas circunflejas
( « » ).
    Esperamos ofrecerle proximamente ofrecer un fix a este problema.
