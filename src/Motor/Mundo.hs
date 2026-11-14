-- Modulo que se encarga de leer y validar los archivos de mundo

module Motor.Mundo
  ( leerMundo
  , parsearClaveValor
  , LineaConNumero
  , BloqueLineas
  ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Motor.Tipos

-- Una linea del archivo con su numero de linea original (1-based)
type LineaConNumero = (Int, String)

-- Un bloque de lineas delimitado por lineas con ---
type BloqueLineas = [LineaConNumero]

-- Elimino espacios en blanco al inicio y al final
recortar :: String -> String
recortar = dropWhileEnd isSpace . dropWhile isSpace

-- Filtro las lineas que no aportan (vacias y comentarios con #)
-- pero preservando el numero de linea original para reportar errores
limpiarLineas :: String -> [LineaConNumero]
limpiarLineas contenido =
  [ (num, limpia)
  | (num, linea) <- zip [1..] (lines contenido)
  , let limpia = recortar linea
  , case limpia of
      ""      -> False
      ('#':_) -> False
      _       -> True
  ]

-- Separo las lineas utiles en bloques usando '---' como separador
separarBloques :: [LineaConNumero] -> [BloqueLineas]
separarBloques [] = []
separarBloques lns =
  let (bloque, resto) = break (\(_, s) -> s == "---") lns
  in case resto of
       [] -> [bloque | not (null bloque)]
       (_:mas) ->
         (if null bloque then id else (bloque :)) (separarBloques mas)

-- Parsea una linea con formato CLAVE: valor
parsearClaveValor :: LineaConNumero -> Either ErrorMundo (String, String)
parsearClaveValor (num, linea) =
  let (clave, resto) = break (== ':') linea
  in case resto of
       (':':val) ->
         let c = recortar clave
             v = recortar val
         in if null c
              then Left (ErrorSintaxis num "falta la clave antes de los dos puntos")
              else Right (c, v)
       _ ->
         Left (ErrorSintaxis num "la linea no tiene el formato CLAVE: valor")

-- Punto de entrada puro para parsear y validar un mundo
-- Por ahora valida que el archivo no este vacio y tenga bloques separados
leerMundo :: String -> Either [ErrorMundo] Mundo
leerMundo contenido =
  let utiles = limpiarLineas contenido
      bloques = separarBloques utiles
  in case bloques of
       [] ->
         Left [ErrorValidacion "El archivo de mundo esta vacio o solo contiene comentarios"]
       (cabecera:demasBloques) ->
         -- Mas adelante aqui parseamos la cabecera, objetos, salas y corremos V1-V10
         Left [ErrorValidacion ("Estructura preliminar: detectada cabecera con "
                                ++ show (length cabecera) ++ " lineas y "
                                ++ show (length demasBloques) ++ " bloques adicionales.")]
