-----------------------------------------------------------------------------
-- |
-- Modulo       :   Oraculo.hs
--
-- Autores      :   Adolfo Parra (10-10534),
--                  Paul Baptista (10-10056)
--
-- Licencia     :   Apache License 2.0
--
-- Implementation para Haskinator.
-- 
-----------------------------------------------------------------------------


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


-- | Tipo de datos 'Oraculo', usada para representar el conocimiento de
-- Haskinator.
data Oraculo =
    -- | 'Prediccion': representa una predicción de Haskinator.
    Prediccion [Char]
    -- | 'Pregunta': representa una pregunta de Haskinator.
    | Pregunta ([Char],Oraculo,Oraculo)


-- | Instancia de 'Show' para el tipo 'Oraculo'.
-- Toma un argumento de tipo 'Oraculo' y devuelve su representación como
-- 'String'.
instance Show Oraculo where
    show orac = show' orac 0
        where
            show' (Prediccion str) n =
                replicate (4*n) ' ' ++
                "Prediccion: " ++ "\"" ++ str ++ "\""  ++ "\n"
            show' (Pregunta (str,o1,o2)) n =
                replicate (4*n) ' ' ++
                "Pregunta: " ++ "\"" ++ str ++ "\"" ++ "\n" ++
                show' o1 (n+1) ++ show' o2 (n+1)
 

-- | Instancia de 'Read' para el tipo 'Oraculo'.
-- Desde 'IO', lee un 'String' y haciendo uso de 'reads' genera un @IO Oraculo@.
instance Read Oraculo where
  readsPrec _ r = readsOraculo r
  

-- | Funcion usada por la instancia de Read de Oraculo, tranaja en IO.
--
-- De los que recibe como String devuelve un elemento de tipo ReadS, que posee
-- una lista de posibles interpretaciones @(Oraculo,String)@.
readsOraculo :: ReadS Oraculo
readsOraculo ('P':'r':'e':'g':'u':'n':'t':'a':':':s) = 
    [ (Pregunta (x,o1,o2),z) |  (x, t) <- reads s,
								(o1, u) <- readsOraculo t,
                                (o2, z) <- readsOraculo u       ]

readsOraculo ('P':'r':'e':'d':'i':'c':'c':'i':'o':'n':':':s) =
    [(Prediccion x, t) | (x,t) <- reads s ] 

readsOraculo ('\n':ls) = readsOraculo ls

readsOraculo (' ':ls) = readsOraculo ls

readsOraculo _ = []


-- | La función 'crearPrediccion' contruye un 'Oraculo' de tipo 'Prediccion' a
-- partir de un 'String'.
crearPrediccion ::
    [Char]      -- ^ String a partir del cual se construye la 'Prediccion'.
    -> Oraculo  -- ^ 'Prediccion' retornada.
crearPrediccion str = Prediccion str


-- | La funcion 'crearPregunta' recibe un string y dos oráculos del tipo
-- 'Prediccion', para construir un 'Oraculo' de tipo 'Pregunta'.
crearPregunta ::
    [Char]      -- ^ 'String' que contiene el texto de la 'Pregunta'.
    -> Oraculo  -- ^ 'Oraculo' que representa el estado de Haskinator, en caso
                --      de una respuesta positiva a la 'Pregunta' retornada.
    -> Oraculo  -- ^ 'Oraculo' que representa el estado de Haskinator, en caso
                --      de una respuesta negativa a la 'Pregunta' retornada.
    -> Oraculo  -- ^ 'Pregunta' retornada.
crearPregunta str o1 o2 =
    Pregunta (str, o1, o2)


-- | La función 'prediccion' recibe un 'Oraculo' y en caso de que sea este una
-- 'Prediccion', devuelve el texto de la misma; arroja un error en caso
-- contrario.
prediccion ::
    Oraculo         -- ^ 'Oraculo' del que hacer la extracción.   
    -> [Char]       -- ^ 'String' extraído.
prediccion (Prediccion str) =
    str
prediccion (Pregunta str) =
    error "Este oráculo es una predicción"


-- | La función 'pregunta' recibe un 'Oraculo' y en caso de que sea este una
-- 'Pregunta', devuelve el texto de la misma.
--
-- Arroja un error en caso contrario.
pregunta ::
    Oraculo         -- ^ 'Oraculo' del que hacer la extracción.
    -> [Char]       -- ^ 'String' extraído.
pregunta (Prediccion o1) =
    error "Este oráculo es una pregunta."
pregunta (Pregunta  (str,o1,o2)) =
    str


-- | La función 'positivo' recibe un 'Oraculo' y en caso de que sea este una
-- 'Pregunta', devuelve el 'Oraculo' correspondiente al estado generado por una
-- respuesta positiva.
-- 
-- Arroja un error en caso de que el 'Oraculo' recibido no sea de tipo
-- 'Pregunta'.
positivo ::
    Oraculo         -- ^ 'Oraculo' del que hacer la extracción.
    -> Oraculo      -- ^ 'Oraculo' extraído.
positivo (Prediccion o1) =
    error "Este oráculo es una predicción"
positivo (Pregunta  (str,o1,o2)) =
    o1


-- | La función 'positivo' recibe un 'Oraculo' y en caso de que sea este una
-- 'Pregunta', devuelve el 'Oraculo' correspondiente al estado generado por una
-- respuesta negativa.
--
-- Arroja un error en caso de que el 'Oraculo' recibido no
-- sea de tipo 'Pregunta'.
negativo ::
    Oraculo         -- ^ 'Oraculo' del que hacer la extracción.
    -> Oraculo      -- ^ 'Oraculo' extraído.
negativo (Prediccion o1) = 
    error "Este oráculo es una predicción"
negativo (Pregunta  (str,o1,o2)) = 
    o2

    
-- | La función 'obtenerCadena' que recibe un 'Oraculo' y el texto de una
-- predicción.
--
-- En caso de que la 'Prediccion' correspondiente esté presente en el 'Oraculo'
-- recibido, retorna un  @Maybe [([Char],Bool)]@.
--
-- Que representa las preguntas que deben hacerse y el estado de las respuestas
-- que han de recibirse para alcanzar la 'Prediccion' en cuestión.
--
-- En caso contrario retorna Nothing.  
obtenerCadena ::
    Oraculo                     -- ^ 'Oraculo' en el que se ha de consultar.
    -> [Char]                   -- ^ 'String' a consultar.
    -> Maybe [([Char],Bool)]    -- ^ Resultado de la consulta.

obtenerCadena (Prediccion orac) pred 
    | orac == pred =
        Just []
    | otherwise = 
        Nothing 

obtenerCadena (Pregunta (preg,o1,o2)) pred =
    (liftM ((preg,True):) (obtenerCadena o1 pred))
    <|>
    (liftM ((preg,False):)) (obtenerCadena o2 pred)

     
-- | Funcion 'obtenerEstadistica', recibe un 'Oraculo' y retorna los datos
-- estadisticos del mismo en una tupla @(Int,Int,Float)@, que contiene el
-- mínimo, máximo y promedio de preguntas que el oráculo necesita hacer para
-- llegar a alguna predicción, respectivamente.
obtenerEstadistica ::
    Oraculo             -- ^ 'Oraculo' a consultar.
    -> (Int,Int,Float)  -- ^ Estadísticas obtenidas, (mínimo,máximó,promedio).
obtenerEstadistica o1 = 
    let x = obtenerEstadistica' o1
    in (minimum x, maximum x, (fromIntegral (sum x)) / 
                                                    (fromIntegral (length x)) )
        where
        obtenerEstadistica' (Prediccion orac) = [0]
        obtenerEstadistica' (Pregunta (str,o1,o2)) = 
            (++)    (map (+1) (obtenerEstadistica' o1))
                    (map (+1) (obtenerEstadistica' o2))