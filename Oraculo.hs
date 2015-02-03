-- Autores: 
-- Jesus Parra 10-10534
-- Paul Baptista 10-10056

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
import Control.Applicative ((<|>))
import Control.Monad (liftM)

data Oraculo = Prediccion [Char] | Pregunta ([Char],Oraculo,Oraculo)


-- Instancia de Show de Oraculo
instance Show Oraculo where
  show (Prediccion str) =
    "Prediccion: " ++ "\"" ++ str ++ "\""  ++ "\n"
  show (Pregunta (str,o1,o2)) =
    "Pregunta: " ++ "\"" ++ str ++ "\"" ++ "\n" ++ show o1 ++ show o2
 
 
instance Read Oraculo where
  readsPrec _ r = readsOraculo r
  
  
-- Funcion usada por la instancia de Read de Oraculo
readsOraculo :: ReadS Oraculo
readsOraculo ('P':'r':'e':'g':'u':'n':'t':'a':':':s) = 
    [ (Pregunta (x,o1,o2),z) |  (x, '\n':t) <- reads s,
								(o1, '\n':u) <- readsOraculo t,
                                (o2, z) <- readsOraculo u       ]

readsOraculo ('P':'r':'e':'d':'i':'c':'c':'i':'o':'n':':':s) =
    [(Prediccion x, t) | (x,t) <- reads s ] 

readsOraculo _ = []


-- Funcion que recibe un string y devuelve un Oraculo del tipo Prediccion
crearPrediccion :: [Char] -> Oraculo
crearPrediccion str = Prediccion str

-- Funcion que recibe un string y dos Oraculos del tipo Prediccion y devuelve
-- un Oraculo del tipo Pregunta
crearPregunta :: [Char] -> Oraculo -> Oraculo -> Oraculo
crearPregunta str o1 o2 =
    Pregunta (str, o1, o2)

-- Funcion que recibe un Oraculo del tipo Prediccion y devuelve su prediccion
prediccion :: Oraculo -> [Char]
prediccion (Prediccion str) =
    str
prediccion (Pregunta str) =
    error "Este oráculo es una predicción"

-- Funcion que recibe un Oraculo del tipo Pregunta y devuelve su pregunta
pregunta :: Oraculo -> [Char]
pregunta (Prediccion o1) =
    error "Este oráculo es una pregunta."
pregunta (Pregunta  (str,o1,o2)) =
    str

-- Funcion que recibe un Oraculo del tipo Pregunta y devuelve el Oraculo de la
-- respuesta positiva
positivo :: Oraculo -> Oraculo
positivo (Prediccion o1) =
    error "Este oráculo es una predicción"
positivo (Pregunta  (str,o1,o2)) =
    o1

-- Funcion que recibe un Oraculo del tipo Pregunta y devuelve el Oraculo de la
-- respuesta negativa
negativo :: Oraculo -> Oraculo
negativo (Prediccion o1) = 
    error "Este oráculo es una predicción"
negativo (Pregunta  (str,o1,o2)) = 
    o2

    
-- Funcion que recibe un Oraculo y una pregunta y en caso de que la pregunta
-- pertenezca al Oraculo devuelve de una lista de tuplas que corresponde a las
-- preguntas que se deben hacer y su respuesta para llegar a la pregunta dada
obtenerCadena :: Oraculo -> [Char] -> Maybe [([Char],Bool)]
obtenerCadena (Prediccion orac) pred 
    | orac == pred =
        Just []
    | otherwise = 
        Nothing 

obtenerCadena (Pregunta (preg,o1,o2)) pred =
     (liftM ((preg,True):) (obtenerCadena o1 pred))
     <|>
     (liftM ((preg,False):)) (obtenerCadena o2 pred)

     
-- Funcion que dado un Oraculo devuelve datos estadisticos del mismo
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