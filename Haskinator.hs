import Oraculo
import System.Exit

main :: IO()
main =  do
        menu Nothing

menu :: Maybe Oraculo -> IO()
menu orac =  do
        putStr "\ESC[2J\x1b[1;31m"
        putStr "\n\nBienvenido a "
        putStr "\x1b[32m Haskinator"
        putStrLn "\x1b[0m\n\n"
        
        putStrLn "Posee las siguientes opciones de interaccióon:\n"
        putStrLn "1) Crear un oraculo nuevo."
        putStrLn "2) Predecir."
        putStrLn "3) Persistir."
        putStrLn "4) Cargar."
        putStrLn "5) Consultar pregunta crucial."
        putStrLn "6) Consultar Estadísticas."
        putStrLn "\n\nIngrese el número de la opción que desea ejecutar"
        putStrLn "o presione 'q' para salir\n"
        x <- getChar
        case x of   '1' -> opt1
                    '2' -> opt2
                    '3' -> opt3
                    '4' -> opt4
                    '5' -> opt5
                    '6' -> opt6
                    'q' -> exitSuccess
                    _   -> menu orac
        menu orac

opt1 = putStr "\nopt1"
opt2 = putStr "\nopt2"
opt3 = putStr "\nopt3"
opt4 = putStr "\nopt4"
opt5 = putStr "\nopt5"
opt6 = putStr "1"