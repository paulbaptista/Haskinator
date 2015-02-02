Proyecto 1 del Laboratorio de Lenguajes de Programacion

Integrantes:
Jesus Parra 10-10534
Paul Baptista 10-10056

Instrucciones de uso:

Para compilar Haskinator ejecute : 'make'
Para ejecutar Haskinator ejecute : './Haskinator'
Para borrar Haskinator ejecute : 'make clean'

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

Para la lectura de archivo: además de verificar la existencia del mismo,
Haskinator verifica que efectivamente se esté cargando un Oraculo; haciendo uso
de la función readIO, que devolverá un IO Oraculo si la representación es la
correcta o lanzará un error si no; para capturar el error usamos la funcion
tryIOError que devuelve un either, de acuerdo a si se obtuvo un oráculo o un
error. Por ultimo se lee un oraculo llamamos a isOraculo, en caso producir
un error ejecutamos isNotOraculo, informándole al usuario de lo sucedido.
