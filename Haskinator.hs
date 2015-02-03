-- Autores: 
-- Jesus Parra 10-10534
-- Paul Baptista 10-10056

import Oraculo
import System.Exit (exitSuccess)
import Data.Maybe
import System.Directory (doesFileExist)
import System.IO.Error (tryIOError)


--  Función principal del programa
main :: IO()
main =
    do
        cls
        menu Nothing ""


--  Nombre del programa.
programName = "\x1b[4;32mHaskinator\x1b[0m"


--  Función IO() que hace clean de la pantalla
cls :: IO()
cls = putStr "\ESC[2J"


--  Función IO() Que imprime un String dado en color amarillo
info :: [Char] -> IO() 
info str =
    putStrLn $ "\n\x1b[1;33m" ++ str ++ "\x1b[0m"


--  Función IO() Que imprime un String dado en color Cyan
info2 :: [Char] -> IO() 
info2 str =
    putStrLn $ "\n\x1b[36m" ++ str ++ "\x1b[0m"


--  Función IO() Que imprime un String dado en color Rojo
info3 :: [Char] -> IO() 
info3 str =
    putStrLn $ "\n\x1b[31m" ++ str ++ "\x1b[0m"


--  Función IO() que limpia la pantalla y luego imprime el String recibido
clsInfo :: [Char] -> IO()
clsInfo str =
    do
        cls
        info str


--  Función IO() que despliega un menu que informa al usuario de las opciones 
-- que tiene disponibles para interactuar con el programa.
--  Recibe como parametros un Oraculo y un String. El primero para usarlo 
-- como tal y el segundo para ser impreso como mensaje informativo.
menu :: (Maybe Oraculo) -> [Char] -> IO()
menu orac str =
    do
        cls
        putStr "\x1b[1;31m"
        putStr $ "\n\nBienvenido a: " ++ programName ++ "\n\n"
            
        putStrLn "Posee las siguientes opciones de interacción:\n"

        putStrLn "1) Crear un oraculo nuevo."
        putStrLn "2) Predecir."
        putStrLn "3) Persistir."
        putStrLn "4) Cargar."
        putStrLn "5) Consultar pregunta crucial."
        putStrLn "6) Consultar Estadísticas."

        info str;
            
        putStrLn "Ingrese el número de la opción que desea ejecutar"
        putStrLn "o presione 'q' para salir\n"

        xs <- getLine
        case xs of 
            "1" -> nuevoOrac
            "2" -> do
                        cls
                        predecir orac
            "3" -> persistir orac
            "4" -> cargar orac
            "5" -> consPreg orac
            "6" -> consEst orac
            "q" -> exitSuccess
            _   -> menu orac "Entrada mal formada."


--  Función que lanza el menú de interacción con un Oráculo "vacio".
nuevoOrac :: IO()
nuevoOrac =
    do
        menu Nothing "Nuevo Oraculo creado, actualmente vacio."


--  Función que interactua con el usuario usando el Oráculo recibido para hacer
-- una predicción.
--  Si el Oráculo recibido no posee la predicción, se pide información al 
-- usuario para agregarla al mismo.
predecir :: Maybe Oraculo -> IO()
predecir Nothing =
    do 
        clsInfo $ "El oráculo está vacío, no puede hacer una predicción.\n"
                ++"Por favor introducea la respuesta que esperabas:"
        pred <- getLine

        let newPred = Just $ crearPrediccion pred

        menu newPred $ "Gracias por ayudar a mejorar a "++programName++"!"

predecir (Just orac) =
    predecir' orac []
    where
        predecir' (Prediccion str) ruta =
            do
                info3 $ prediccion (Prediccion str)
                info "Es correcta?"
                info2 "s/n\n"

                xs <- getLine
	
                case xs of
                    "s" ->
                        menu (Just orac) $ "Gracias por Jugar con "
                                           ++ programName
                    "n" -> 
                        do
                            clsInfo $ "\nPredicción errada.\n"
                                      ++"Por favor introduce la respuesta que "
                                      ++"esperabas:"
                            pred <- getLine

                            clsInfo $ "\nPor favor introduce una pregunta que "
                                    ++"distinga la respuesta que esperabas\n"
                                    ++"de la prediccion obtenida:"
                            strPreg <- getLine



                            let newPred = crearPrediccion pred
                                newPreg = crearPregunta
                                            strPreg
                                            newPred
                                            $ Prediccion str
                                newOrac = Just $ agregarPreg
                                                 newPreg orac
                                                 $ reverse ruta

                            menu newOrac $ "Gracias por ayudar a mejorar a "
                                                            ++programName++"!"
                    _   ->  
                        do
                            clsInfo "Entrada mal formada."
                            predecir' (Prediccion str) ruta


        predecir' (Pregunta preg) ruta =
            do
                info $ pregunta (Pregunta preg)
                info2 "s/n\n"

                x <- getLine
                case x of
                    "s" -> do
                            cls
                            predecir' (positivo (Pregunta preg)) (True:ruta)

                    "n" -> do
                            cls
                            predecir' (negativo (Pregunta preg)) (False:ruta)

                    _   ->  do
                                clsInfo "Entrada mal formada."
                                predecir' (Pregunta preg) ruta


--  Función devuleve el Oraculo que se forma al agregar el segundo Oraculo al
-- primero, en la posición especificada por la lista de Booleanos
agregarPreg :: Oraculo -> Oraculo -> [Bool] -> Oraculo
agregarPreg (Pregunta preg) (Prediccion pred) xs 
    | xs == [] = (Pregunta preg)
    | otherwise = error "Esto no debería suceder."

agregarPreg (Pregunta preg) (Pregunta orig) (act:resto)
    | act =
        crearPregunta
            ( pregunta $ Pregunta orig )
            ( agregarPreg (Pregunta preg)
                (positivo $ Pregunta orig)
                resto )
            ( negativo (Pregunta orig) )

    | otherwise = 
        crearPregunta
            ( pregunta $ Pregunta orig )
            ( positivo $ Pregunta orig )
            ( agregarPreg (Pregunta preg)
                (negativo $ Pregunta orig)
                resto )


--  Guarda el estado actual del Oráculo en un archivo a especificar por el
-- usuario.
persistir :: Maybe Oraculo -> IO()
persistir Nothing = menu Nothing "Oráculo vacio."
persistir (Just orac)  =
    do
        cls
        info $ "Inserte el nombre del archivo en el que se guardará el\n"
                ++"oraculo actual:"

        filename <- getLine
        writeFile filename (show orac)

        menu (Just orac) "Oráculo guardado exitosamente."


--  Carga un Oráculo a partir de un archivo a especificar por el usuario.
cargar :: Maybe Oraculo -> IO()
cargar orac =
    do
        cls
        info $ "Inserte el nombre del archivo del cual "
                ++"se cargará el oráculo:"
        filename <- getLine
	existe <- doesFileExist filename
	if existe then
            do
                let
                    isOraculo x =  menu (Just x) "Oráculo cargado exitosamente."
                    isNotOraculo x = menu orac "Carga fallida."

                str <- readFile filename
                oraculoOrNot <- tryIOError (readIO str :: IO Oraculo)

                either isNotOraculo isOraculo oraculoOrNot
        else
		  menu orac "El archivo no existe o es un directorio."


--  Función que recibe dos posibles predicciones y si existen en el Oráculo
-- devuelve la pregunta más cercana ancestro común de ambas.
consPreg :: Maybe Oraculo -> IO()
consPreg Nothing =
        menu Nothing "Consulta inválida, oráculo vacío."

consPreg (Just orac) =
    do
        cls
        info "Inserte la primera predicción:"
        pred1 <- getLine
        cls
        info "Inserte la segunda predicción:"
        pred2 <- getLine

        compCad orac pred1 pred2

        where
            compCad orac pred1 pred2 =
                let
                    cad1 = obtenerCadena orac pred1
                    cad2 = obtenerCadena orac pred2
                in
                    if isNothing cad1 || isNothing cad2 then
                        menu  (Just orac) "Consulta inválida."
                    else
                        let
                            resto = foldl fun (fromJust cad1) (fromJust cad2)
                                where
                                    fun [x] y = [x]

                                    fun (x:y:xs) z =
                                        if fst y == fst z then
                                            (y:xs)
                                        else
                                            (x:y:xs)
                        in
                            menu (Just orac) $ "La pregunta crucial es: "
                                                ++ fst (head resto)


--  Pregunta que a partir del Oráculo recibido, imprime las estadísticas del
-- mismo
consEst :: Maybe Oraculo -> IO()
consEst Nothing =
        menu Nothing "Consulta inválida, oráculo vacío."

consEst (Just orac) =
        menu (Just orac) $ "(min,max,prom):\t"
                            ++ show (obtenerEstadistica orac)
