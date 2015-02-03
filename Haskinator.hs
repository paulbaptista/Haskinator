-----------------------------------------------------------------------------
-- |
-- Modulo       :   Haskinator.hs
--
-- Autores      :   Adolfo Parra (10-10534),
--                  Paul Baptista (10-10056)
--
-- Licencia     :   Apache License 2.0
--
-- Implementation de Haskinator.
-- 
-----------------------------------------------------------------------------


module Main where


import Oraculo
import System.Exit (exitSuccess)
import Data.Maybe
import System.Directory (doesFileExist)
import System.IO.Error (tryIOError)


-- | Función principal del programa.
--  
-- Hace una llamada al menú principal con 'Nothing' como parámetro.
main :: IO()
main =
    do
        cls
        menu Nothing ""


-- | Imprime subrayado el nombre del programa en color verde.
programName = "\x1b[4;32mHaskinator\x1b[0m"


-- | Función de'IO' que hace clean de la pantalla.
cls :: IO()
cls = putStr "\ESC[2J"


-- | Función de 'IO' que imprime el String recibido en color amarillo.
info ::
    [Char]      -- ^ String a imprimir.
    -> IO() 
info str =
    putStrLn $ "\n\x1b[1;33m" ++ str ++ "\x1b[0m"


-- | Función de 'IO' que imprime el String recibido en color cyan.
info2 ::
    [Char]      -- ^ String a imprimir.
    -> IO() 
info2 str =
    putStrLn $ "\n\x1b[36m" ++ str ++ "\x1b[0m"


-- | Función de 'IO' que imprime el String recibido en color rojo.
info3 ::
    [Char]      -- ^ String a imprimir.
    -> IO() 
info3 str =
    putStrLn $ "\n\x1b[31m" ++ str ++ "\x1b[0m"


-- | Función de IO que limpia la pantalla y luego imprime el String recibido.
clsInfo ::
    [Char]      -- ^ String a imprimir.
    -> IO()
clsInfo str =
    do
        cls
        info str


-- | Función de 'IO' que despliega un menu que informa al usuario de las
-- opciones que tiene disponibles para interactuar con el programa.
--
-- Recibe como parametros un Oraculo y un String. El primero para usarlo como
-- tal y el segundo para ser impreso como mensaje informativo.
menu ::
    (Maybe Oraculo) -- ^ @Maybe 'Oraculo'@, que ha de poseer el 'Oraculo'
                    --  actual.
    -> [Char]       -- ^ Mensaje informativo.
    -> IO()
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


-- | Función que lanza el menú de interacción con un 'Oraculo' "vacio"
-- ('Nothing').
nuevoOrac :: IO()
nuevoOrac =
    do
        menu Nothing "Nuevo Oraculo creado, actualmente vacio."


-- | Función que interactua con el usuario usando el Oráculo recibido para hacer
-- una predicción.
--
--  Si el 'Oraculo' recibido no posee la predicción, se pide información al 
-- usuario para agregarla al mismo.
predecir ::
    Maybe Oraculo   -- ^ @Maybe 'Oraculo'@ del cual se intentará hacer la
                    -- predicción.
    -> IO()
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


-- | Función que devuleve el Oraculo que se forma al agregar un segundo Oraculo
-- al primero, en la posición representada por la lista de Booleanos recibida
-- como segundo argumento.
agregarPreg ::
    Oraculo         -- ^ 'Oraculo' base.
    -> Oraculo      -- ^ 'Oraculo' a agregar.
    -> [Bool]       -- ^ Lista de 'Bool' que representa la posición en la que se
                    -- hará la agregación.
    -> Oraculo      -- ^ 'Oraculo' formado.

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


-- | Función de 'IO' que escribe el estado actual del 'Oraculo' recibido como
-- primer parámetro en el archivo de cuyo nombre recibe como 'String' de la
-- entrada estándar.
persistir ::
    Maybe Oraculo       -- ^ @Maybe 'Oraculo'@ a guardar.
    -> IO()
persistir Nothing = menu Nothing "Oráculo vacio."
persistir (Just orac)  =
    do
        cls
        info $ "Inserte el nombre del archivo en el que se guardará el\n"
                ++"oraculo actual:"

        filename <- getLine
        writeFile filename (show orac)

        menu (Just orac) "Oráculo guardado exitosamente."


-- | Función de 'IO' que escribe el estado actual del 'Oraculo' recibido como
-- primer parámetro en el archivo de cuyo nombre recibe como 'String' de la
-- entrada estándar.
cargar ::
    Maybe Oraculo       -- ^ @Maybe 'Oraculo'@ que ha de contener el 'Oraculo' a
                        -- cargar.
     -> IO()
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


-- | Función que recibe un 'Oraculo' y recibe de la entrada estándar dos
-- 'String' que representan predicciones, si estas existen en el 'Oraculo'
-- devuelve la pregunta común más cercana a ambas.
consPreg ::
    Maybe Oraculo       -- ^ @Maybe 'Oraculo'@ que ha de poseer el 'Oraculo' a
                        -- consultar.
    -> IO()
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


-- | Función que recibe @Maybe Oraculo@ y si este no es Nothing imprime las
-- las estadísticas del mismo, si no notifica la invalidez de la consulta.
consEst :: 
    Maybe Oraculo   -- ^ @Maybe 'Oraculo'@ que ha de contener el 'Oraculo' a
                    -- ^ consultar.
    -> IO()
consEst Nothing =
        menu Nothing "Consulta inválida, oráculo vacío."

consEst (Just orac) =
        menu (Just orac) $ "(min,max,prom):\t"
                            ++ show (obtenerEstadistica orac)