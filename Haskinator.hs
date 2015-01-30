import Oraculo
import System.Exit
import Data.Maybe
import System.IO



main :: IO()
main =
    do
        cls
        menu Nothing ""


menu :: (Maybe Oraculo) -> [Char] -> IO()
menu orac str =
    do
        cls
        putStr "\x1b[1;31m"
        putStr "\n\nBienvenido a: "
        putStr "\x1b[32m Haskinator\x1b[0m\n\n"
            
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

        (x:xs) <- getLine
        if (xs /= []) then
            do
                menu orac "Entrada mal formada."
        else
            case x of 

                '1' -> nuevoOrac
                '2' -> predecir orac
                '3' -> persistir orac
                '4' -> cargar
                '5' -> consPreg orac
                '6' -> consEst orac
                'q' -> exitSuccess
                _   -> menu orac "Entrada mal formada."


cls :: IO()
cls = putStr "\ESC[2J"

info :: [Char] -> IO() 
info str =
    putStrLn $ "\n\x1b[1;33m" ++ str ++ "\x1b[0m"


nuevoOrac :: IO()
nuevoOrac =
    do
        menu Nothing "Nuevo Oraculo creado, actualmente vacio."


predecir :: Maybe Oraculo -> IO()
predecir Nothing =
    menu Nothing $ "El oráculo está vacío, "
                    ++"no puede hacer una predicción."

predecir (Just orac) =
    predecir' orac []
    where
        predecir' (Prediccion str) ruta =
            do
                cls
                info $ prediccion (Prediccion str)
                       ++ "\nEs correcta?"
                putStrLn "Si/No\n"

                xs <- getLine
                case xs of
                    "Si" ->
                        menu (Just orac) $ "Gracias por Jugar con putStr"
                                            ++"\x1b[32m Haskinator\x1b[0m"

                    "No" -> 
                        do
                            cls
                            info $ "\nPredicción errada.\n"
                                    ++"Por favor introduzca la respuesta "
                                    ++"que esperaba:"
                            pred <- getLine
                            cls
                            info $ "\nPor favor introduce una pregunta "
                                    ++"que distinga la respuesta que "
                                    ++"esperabas de la prediccion "
                                    ++"obtenida."
                            preg <- getLine

                            menu    ( Just (agregarPreg
                                            (crearPregunta
                                                preg
                                                (crearPrediccion pred)
                                                (Prediccion str)
                                                )
                                            orac
                                            (reverse ruta)
                                            )
                                    )
                                    ("Gracias por ayudar a mejorar "
                                    ++"Haskinator!")

                    _   ->  
                        do
                            cls
                            info "Entrada mal formada."
                            predecir' (Prediccion str) ruta


        predecir' (Pregunta preg) ruta =
            do
                cls
                info $ pregunta (Pregunta preg)
                putStrLn "Si/No\n"

                x <- getLine
                case x of
                    "Si" -> predecir' (positivo (Pregunta preg)) (True:ruta)

                    "No" -> predecir' (negativo (Pregunta preg)) (False:ruta)

                    _   ->  do
                                cls
                                info "Entrada mal formada."
                                predecir' (Pregunta preg) ruta


agregarPreg (Pregunta preg) (Prediccion pred) xs 
    | xs == [] = (Pregunta preg)
    | otherwise = error "pepe"

agregarPreg (Pregunta preg) (Pregunta orig) (act:resto)
    | act =
        crearPregunta
            ( pregunta (Pregunta orig) )
            ( agregarPreg (Pregunta preg)
                (positivo (Pregunta orig))
                resto )
            ( negativo (Pregunta orig) )

    | otherwise = 
        crearPregunta
            ( pregunta (Pregunta orig) )
            ( positivo (Pregunta orig) )
            ( agregarPreg (Pregunta preg)
                (negativo (Pregunta orig))
                resto )

persistir Nothing = menu Nothing "Oráculo vacio."
persistir (Just orac)  =
    do
        cls
        info $ "Inserte el nombre del archivo el "
                    ++ "que se guardará el oraculo actual:"
        filename <- getLine
        writeFile filename (show orac)
        menu (Just orac) "Oráculo guardado exitosamente."

cargar =
    do
        cls
        info $ "Inserte el nombre del archivo del cual "
                    ++"se cargará el oráculo:"
        filename <- getLine
        str <- readFile filename
        menu (Just (read str)) "Oráculo cargado exitosamente."


consPreg (Just orac) =
    do
        cls
        info "Inserte la primera predicción:"
        pred1 <- getLine
        cls
        info "Inserte la segunda predicción:"
        pred2 <- getLine

        compCad orac pred1 pred2

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
                menu (Just orac) ("La pregunta crucial es: "
                                    ++ fst (head resto) )

consEst Nothing =
        menu Nothing "Consulta inválida, oráculo vacío."

consEst (Just orac) =
        menu (Just orac) ("(minimo,maximo,promedio)\t=\t"
                  ++ show (obtenerEstadistica orac))