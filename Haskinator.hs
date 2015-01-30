import Oraculo
import System.Exit


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
        putStr "\n\nBienvenido a "
        putStr "\x1b[32m Haskinator"
        putStrLn "\x1b[0m\n"
            
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
                '4' -> cargar orac
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
    predecir' orac Nothing Nothing orac
    where
        predecir' (Prediccion str) padre est orig =
            do
                putStrLn (prediccion orac)
                putStrLn "Es correcta?"
                putStrLn "Si/No"

                xs <- getLine
                case xs of
                    "Si" -> menu (Just orig) $ "Excelente!! Gracias por "
                                                ++"Jugar con Haskinator"

                    "No" -> do
                                putStrLn $ "Qué mal!! Predicción errada.\n"
                                        ++"Por favor introduce la respuesta "
                                        ++"que esperabas"
                                pred <- getLine
                                putStrLn $ "\nPor favor introduce una pregunta "
                                        ++"que distinga la respuesta que "
                                        ++"esperabas de la prediccion "
                                        ++"obtenida."
                                preg <- getLine

                                menu    (Just   (crearPregunta  (preg
                                                                (crearPrediccion pred)
                                                                orig )
                                                )
                                        )
                                        ("Gracias por colaborar con "
                                                        ++"Haskinator")

                    _   ->  do
                                info "Entrada mal formada.\n"
                                predecir' orac padre est orig


        predecir' (Pregunta (preg,o1,o2)) padre est orig =
            do
                putStrLn (pregunta orac)
                putStrLn "Si/No"

                x <- getLine
                case x of
                    "Si" -> predecir' o1 (Just orac) (Just True) orig

                    "No" -> predecir' o2 (Just orac) (Just False) orig

                    _   ->  do
                                info "Entrada mal formada.\n"
                                predecir' orac padre orig


persistir orac  =   putStr "\nopt3"

cargar orac     =   putStr "\nopt4"

consPreg orac   =   putStr "\nopt5"

consEst orac    =   putStr "1"