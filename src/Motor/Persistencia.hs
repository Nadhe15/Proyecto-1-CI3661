-- Modulo para leer archivos y manejar el guardado de partidas

module Motor.Persistencia
  ( cargarMundoArchivo
  , guardarPartida
  , cargarPartida
  ) where

import Control.Exception (IOException, catch, displayException, evaluate)
import qualified Data.Map.Strict as Map
import System.IO (IOMode (ReadMode, WriteMode), hGetContents, hPutStr, hSetEncoding, utf8, withFile)
import Text.Read (readMaybe)
import Motor.Mundo (leerMundo)
import Motor.Nucleo (cumpleMeta)
import Motor.Tipos

-- Leo el archivo del mundo y se lo paso a leerMundo
-- Si ocurre algun fallo de lectura atrapo el error
cargarMundoArchivo :: FilePath -> IO (Either [ErrorMundo] Mundo)
cargarMundoArchivo ruta =
  (do contenido <- leerArchivoUtf8 ruta
      pure (leerMundo contenido))
    `catch` manejarErrorLectura
  where
    manejarErrorLectura :: IOException -> IO (Either [ErrorMundo] Mundo)
    manejarErrorLectura e =
      pure (Left [ErrorValidacion ("No se pudo leer el archivo '" ++ ruta ++ "': " ++ displayException e)])

-- Guardo los datos necesarios de la partida actual en el archivo
guardarPartida :: FilePath -> Estado -> IO (Either String ())
guardarPartida ruta st =
  let partida = PartidaGuardada
        { pgTitulo     = mundoTitulo (estMundo st)
        , pgSalaActual = estSalaActual st
        , pgInventario = estInventario st
        , pgSalasObjs  = estSalasObjs st
        }
  in (escribirArchivoUtf8 ruta (show partida) >> pure (Right ()))
       `catch` manejarErrorEscritura
  where
    manejarErrorEscritura :: IOException -> IO (Either String ())
    manejarErrorEscritura e =
      pure (Left ("No se pudo guardar la partida en '" ++ ruta ++ "': " ++ displayException e))

-- Cargo la partida desde el archivo y verifico que sirva para este mundo
cargarPartida :: FilePath -> Mundo -> IO (Either String Estado)
cargarPartida ruta mundo =
  (do contenido <- leerArchivoUtf8 ruta
      pure (reconstruirEstado mundo contenido))
    `catch` manejarErrorLectura
  where
    manejarErrorLectura :: IOException -> IO (Either String Estado)
    manejarErrorLectura e =
      pure (Left ("No se pudo cargar la partida '" ++ ruta ++ "': " ++ displayException e))

-- Reviso el contenido leido y reconstruyo el estado si todo coincide
reconstruirEstado :: Mundo -> String -> Either String Estado
reconstruirEstado mundo contenido =
  case readMaybe contenido :: Maybe PartidaGuardada of
    Nothing ->
      Left "El archivo de partida está corrupto o no tiene un formato válido."
    Just pg
      | pgTitulo pg /= mundoTitulo mundo ->
          Left ("La partida pertenece a otro mundo ('" ++ pgTitulo pg ++ "').")
      | not (Map.member (pgSalaActual pg) (mundoSalas mundo)) ->
          Left "La partida guardada es incompatible: la sala actual no existe en este mundo."
      | not (all (`Map.member` mundoObjetos mundo) (pgInventario pg)) ->
          Left "La partida guardada es incompatible: contiene objetos inexistentes en este mundo."
      | not (salasYObjetosValidos pg) ->
          Left "La partida guardada es incompatible: sus salas u objetos no coinciden con este mundo."
      | excedeCapacidad (pgInventario pg) ->
          Left "La partida guardada es incompatible: el inventario excede la capacidad del mundo."
      | otherwise ->
          let st = Estado
                { estMundo      = mundo
                , estSalaActual = pgSalaActual pg
                , estInventario = pgInventario pg
                , estSalasObjs  = pgSalasObjs pg
                , estTerminado  = False
                , estSalir      = False
                }
          in Right (st { estTerminado = cumpleMeta st })
  where
    salasYObjetosValidos pg =
      all (\(sid, oids) -> Map.member sid (mundoSalas mundo)
                        && all (`Map.member` mundoObjetos mundo) oids)
          (Map.toList (pgSalasObjs pg))
    excedeCapacidad inv =
      case mundoCapacidad mundo of
        Nothing  -> False
        Just cap -> pesoInventario mundo inv > cap

-- Leo el archivo forzando codificacion utf8
leerArchivoUtf8 :: FilePath -> IO String
leerArchivoUtf8 ruta =
  withFile ruta ReadMode $ \h -> do
    hSetEncoding h utf8
    contenido <- hGetContents h
    _ <- evaluate (length contenido)
    pure contenido

-- Escribo el archivo en utf8
escribirArchivoUtf8 :: FilePath -> String -> IO ()
escribirArchivoUtf8 ruta contenido =
  withFile ruta WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStr h contenido
