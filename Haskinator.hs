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
                menu orac "Entrada mal formada"
        else
            case x of 

                '1' -> nuevoOrac
                '2' -> predecir orac
                '3' -> persistir orac
                '4' -> cargar orac
                '5' -> consPreg orac
                '6' -> consEst orac
                'q' -> exitSuccess
                _   -> menu orac "Entrada mal formada"


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
                    ++ "no puede hacer una predicción."

predecir (Just orac) =
    predecir' orac
    where
        predecir' ()

    do
        putStrLn (prediccion orac)
        putStrLn "Es correcta?"
        putStrLn "1) Sí"
        putStrLn "2) No"

        (x:xs) <- getLine
        if (xs /= []) then
            do
                info "Entrada mal formada"
        else
            case x of
                '1' -> nuevoOrac
                '2' -> predecir (Just orac)
                _   -> info "Entrada mal formada"


--predecir (Just (Pregunta orac))  =   putStr "\nopt2"

persistir orac  =   putStr "\nopt3"

cargar orac     =   putStr "\nopt4"

consPreg orac   =   putStr "\nopt5"

consEst orac    =   putStr "1"