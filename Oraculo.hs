module Oraculo 
    (
          Oraculo(Prediccion,Pregunta)
        , crearPrediccion
        , crearPregunta
        , prediccion
        , pregunta
        , positivo
        , negativo
        , obtenerCadena
        , obtenerEstadistica
        , q1
    )
    where 

import Data.Maybe

data Oraculo = Prediccion [Char] | Pregunta ([Char],Oraculo,Oraculo)
                                                                deriving Show

p1 = Prediccion "Se llama Leyda"
p2 = Prediccion "Se llama Jose"
p3 = Prediccion "No se quien es"
p4 = Prediccion "Es tu hermano"

q1 = Pregunta ("Es tu mama?",p1,q2)
q2 = Pregunta("Es tu papa?",p2,q3)
q3 = Pregunta ("Es un familiar?",p2,p3)


crearPrediccion :: [Char] -> Oraculo
crearPrediccion str = Prediccion str

crearPregunta :: [Char] -> Oraculo -> Oraculo -> Oraculo
crearPregunta str o1 o2 = Pregunta (str, o1, o2)

prediccion :: Oraculo -> [Char]
prediccion (Prediccion str) = str
prediccion (Pregunta str) = error "pepe"

pregunta :: Oraculo -> [Char]
pregunta (Prediccion o1) = error "pepe"
pregunta (Pregunta  (str,o1,o2)) = str

positivo :: Oraculo -> Oraculo
positivo (Prediccion o1) = error "pepe"
positivo (Pregunta  (str,o1,o2)) = o1

negativo :: Oraculo -> Oraculo
negativo (Prediccion o1) = error "pepe"
negativo (Pregunta  (str,o1,o2)) = o2


obtenerCadena :: Oraculo -> [Char] -> Maybe [([Char],Bool)]
obtenerCadena (Prediccion orac) pred 
    | orac == pred = Just []
    | otherwise = Nothing 

obtenerCadena (Pregunta (preg,o1,o2)) pred =
    aux (obtenerCadena o1 pred) (obtenerCadena o2 pred) preg
        where
        aux cad1 cad2 preg
            | isJust cad1 = Just ((preg,True):fromJust(cad1))
            | (isNothing cad1) && (isJust cad2) =
                Just ((preg,False):fromJust(cad2))
            | otherwise = Nothing

obtenerEstadistica :: Oraculo -> (Int,Int,Float)
obtenerEstadistica o1 = 
    let x = obtenerEstadistica' o1
    in (minimum x, maximum x, (fromIntegral (sum x)) / 
                                                    (fromIntegral (length x)) )
        where
        obtenerEstadistica' (Prediccion orac) = [0]
        obtenerEstadistica' (Pregunta (str,o1,o2)) = 

            foldr (:) (map (+1) (obtenerEstadistica' o1))
                      (map (+1) (obtenerEstadistica' o2))