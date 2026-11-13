-- Punto de entrada del juego

module Main (main) where

import Data.Char (isSpace)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.IO (hFlush, hSetEncoding, isEOF, stderr, stdout, stdin, hPutStrLn, utf8)
import Motor.Comandos (leerComando)
import Motor.Nucleo (describirSalaActual, ejecutar, estadoInicial)
import Motor.Persistencia (cargarMundoArchivo, cargarPartida, guardarPartida)
import Motor.Tipos

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  case args of
    ["-validar", ruta]  -> validarYMostrar ruta
    ["--validar", ruta] -> validarYMostrar ruta
    [ruta]              -> iniciarJuego ruta
    _                   -> do
      hPutStrLn stderr "Uso: aventura [-validar|--validar] <archivo_mundo>"
      exitWith (ExitFailure 1)

-- Solo valido el mundo y muestro si esta correcto o los errores
validarYMostrar :: FilePath -> IO ()
validarYMostrar ruta = do
  res <- cargarMundoArchivo ruta
  case res of
    Left errs -> do
      mapM_ (putStrLn . formatearError) errs
      exitWith (ExitFailure 1)
    Right _ -> do
      putStrLn "Mundo válido."
      exitSuccess

-- Cargo el mundo e inicio el bucle del juego si no hay errores
iniciarJuego :: FilePath -> IO ()
iniciarJuego ruta = do
  res <- cargarMundoArchivo ruta
  case res of
    Left errs -> do
      mapM_ (putStrLn . formatearError) errs
      exitWith (ExitFailure 1)
    Right mundo -> do
      putStrLn ("=== " ++ mundoTitulo mundo ++ " ===")
      let st0 = estadoInicial mundo
      mapM_ putStrLn (describirSalaActual st0)
      if estTerminado st0
        then do
          putStrLn "*** ¡Has ganado! ***"
          exitSuccess
        else bucleJuego st0

-- Bucle principal donde leo comandos y actualizo el estado
bucleJuego :: Estado -> IO ()
bucleJuego st
  | estTerminado st = exitSuccess
  | estSalir st     = exitSuccess
  | otherwise       = do
      putStr "> "
      hFlush stdout
      fin <- isEOF
      if fin
        then do
          putStrLn ""
          exitSuccess
        else do
          linea <- getLine
          if all isSpace linea
            then bucleJuego st
            else case leerComando linea of
              Nothing -> do
                putStrLn "No entiendo ese comando. Escribe 'ayuda' para ver los comandos."
                bucleJuego st
              Just (Guardar archivo) -> do
                res <- guardarPartida archivo st
                case res of
                  Left err -> putStrLn err
                  Right () -> putStrLn ("Partida guardada en '" ++ archivo ++ "'.")
                bucleJuego st
              Just (Cargar archivo) -> do
                res <- cargarPartida archivo (estMundo st)
                case res of
                  Left err -> do
                    putStrLn err
                    bucleJuego st
                  Right stCargado -> do
                    putStrLn ("Partida cargada desde '" ++ archivo ++ "'.")
                    mapM_ putStrLn (describirSalaActual stCargado)
                    if estTerminado stCargado
                      then do
                        putStrLn "*** ¡Has ganado! ***"
                        exitSuccess
                      else bucleJuego stCargado
              Just cmd -> do
                let (msgs, st') = ejecutar cmd st
                mapM_ putStrLn msgs
                bucleJuego st'
