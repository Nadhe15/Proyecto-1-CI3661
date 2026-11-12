-- | Intérprete puro de comandos escritos por el jugador (Parte 2 - Nadheska García).
module Motor.Comandos
  ( leerComando
  ) where

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, intercalate)
import Motor.Tipos (Comando (..), Direccion (..), IdObjeto (..))

-- | Elimina espacios en blanco al inicio y al final de una cadena.
recortar :: String -> String
recortar = dropWhileEnd isSpace . dropWhile isSpace

-- | Reconoce una dirección completa o su abreviatura de una letra.
leerDireccion :: String -> Maybe Direccion
leerDireccion s = case map toLower (recortar s) of
  "norte"  -> Just Norte
  "n"      -> Just Norte
  "sur"    -> Just Sur
  "s"      -> Just Sur
  "este"   -> Just Este
  "e"      -> Just Este
  "oeste"  -> Just Oeste
  "o"      -> Just Oeste
  "arriba" -> Just Arriba
  "abajo"  -> Just Abajo
  _        -> Nothing

-- | Normaliza el nombre de un objeto escrito por el jugador.
-- Acepta el identificador exacto (p. ej. @memoria_usb@), mayúsculas o
-- palabras separadas con un artículo opcional (p. ej. @la memoria usb@).
leerObjeto :: String -> Maybe IdObjeto
leerObjeto s =
  case quitarArticulo (words (map toLower s)) of
    [] -> Nothing
    ws -> Just (IdObjeto (intercalate "_" ws))
  where
    articulos = ["el", "la", "los", "las", "un", "una"]
    quitarArticulo (w : ws) | w `elem` articulos = ws
    quitarArticulo ws = ws

-- | Analiza una línea de entrada del jugador y la convierte en un 'Comando'.
-- Es una función totalmente pura y libre de funciones parciales.
leerComando :: String -> Maybe Comando
leerComando entrada =
  case recortar entrada of
    "" -> Nothing
    limpia ->
      let (verboCrudo, restoCrudo) = break isSpace limpia
          verbo = map toLower verboCrudo
          resto = recortar restoCrudo
      in case verbo of
           "ir"         -> Ir <$> leerDireccion resto
           "mirar"      -> sinArgs Mirar resto
           "tomar"      -> Tomar <$> leerObjeto resto
           "coger"      -> Tomar <$> leerObjeto resto
           "soltar"     -> Soltar <$> leerObjeto resto
           "examinar"   -> Examinar <$> leerObjeto resto
           "inventario" -> sinArgs Inventario resto
           "inv"        -> sinArgs Inventario resto
           "guardar"    -> conArchivo Guardar resto
           "cargar"     -> conArchivo Cargar resto
           "ayuda"      -> sinArgs Ayuda resto
           "salir"      -> sinArgs Salir resto
           _            -> case leerDireccion limpia of
                             Just dir -> Just (Ir dir)
                             Nothing  -> Nothing
  where
    sinArgs cmd "" = Just cmd
    sinArgs _   _  = Nothing

    conArchivo _ ""   = Nothing
    conArchivo f path = Just (f path)
