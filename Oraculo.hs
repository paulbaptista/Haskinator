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
    )
    where 

import Data.Maybe
import System.IO

data Oraculo = Prediccion [Char] | Pregunta ([Char],Oraculo,Oraculo)

instance Show Oraculo where
  show (Prediccion str) =
    "Prediccion: " ++ "\"" ++ str ++ "\""  ++ "\n"
  show (Pregunta (str,o1,o2)) =
    "Pregunta: " ++ "\"" ++ str ++ "\"" ++ "\n" ++ show o1 ++ show o2
 
 
instance Read Oraculo where
  readsPrec _ r = readsOraculo r
  
readsOraculo :: ReadS Oraculo

readsOraculo ('P':'r':'e':'g':'u':'n':'t':'a':':':s) = 
    [ (Pregunta (x,o1,o2),z) |  (x, '\n':t) <- reads s,
								(o1, '\n':u) <- readsOraculo t,
                                (o2, z) <- readsOraculo u       ]

readsOraculo ('P':'r':'e':'d':'i':'c':'c':'i':'o':'n':':':s) =
    [(Prediccion x, t) | (x,t) <- reads s ] 

-- Formas mas "bonita" que no logre hacer funcionar
{-readsOraculo s       =  [(Pregunta (x,o1,o2),z) | (":", t) <- lex s,
												  (x, '\n':u) <- reads t,
													(y,v) <- lex u,
													(o1,w) <- readsOraculo y,
													(a,b) <- lex v,
													(o2, z) <- readsOraculo a] 
										++  [(Prediccion x, t) | (x,t) <- reads s ]-} 

crearPrediccion :: [Char] -> Oraculo
crearPrediccion str = Prediccion str

crearPregunta :: [Char] -> Oraculo -> Oraculo -> Oraculo
crearPregunta str o1 o2 =
    Pregunta (str, o1, o2)

prediccion :: Oraculo -> [Char]
prediccion (Prediccion str) =
    str
prediccion (Pregunta str) =
    error "Este oráculo es una predicción"

pregunta :: Oraculo -> [Char]
pregunta (Prediccion o1) =
    error "Este oráculo es una pregunta."
pregunta (Pregunta  (str,o1,o2)) =
    str

positivo :: Oraculo -> Oraculo
positivo (Prediccion o1) =
    error "Este oráculo es una predicción"
positivo (Pregunta  (str,o1,o2)) =
    o1

negativo :: Oraculo -> Oraculo
negativo (Prediccion o1) = 
    error "Este oráculo es una predicción"
negativo (Pregunta  (str,o1,o2)) = 
    o2

obtenerCadena :: Oraculo -> [Char] -> Maybe [([Char],Bool)]
obtenerCadena (Prediccion orac) pred 
    | orac == pred =
        Just []
    | otherwise = 
        Nothing 

obtenerCadena (Pregunta (preg,o1,o2)) pred =
    aux (obtenerCadena o1 pred) (obtenerCadena o2 pred) preg
        where
            aux cad1 cad2 preg
                | isJust cad1 = 
                    Just ((preg,True):fromJust(cad1))
                | (isNothing cad1) && (isJust cad2) =
                    Just ((preg,False):fromJust(cad2))
                | otherwise = 
                    Nothing

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