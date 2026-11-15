-- Suite de pruebas unitarias para el motor de aventuras

module Main (main) where

import qualified Data.Map.Strict as Map
import System.Exit (exitFailure, exitSuccess)
import System.IO (IOMode (ReadMode), hGetContents, hSetEncoding, stdout, utf8, withFile)
import Motor.Mundo (leerMundo)
import Motor.Reconocedor (identificador, parsearMeta, parsearSalida, parsearTodo)
import Motor.Tipos

-- Lee un archivo en UTF-8 para las pruebas de mundos desde disco
leerArchivo :: FilePath -> IO String
leerArchivo ruta =
  withFile ruta ReadMode $ \h -> do
    hSetEncoding h utf8
    contenido <- hGetContents h
    length contenido `seq` pure contenido

-- Ejecuta una prueba booleana y reporta si paso o fallo
ejecutarPrueba :: String -> Bool -> IO Bool
ejecutarPrueba nombre ok = do
  if ok
    then putStrLn ("[OK]    " ++ nombre) >> pure True
    else putStrLn ("[FALLO] " ++ nombre) >> pure False

-- Verifica que leerMundo devuelva la cantidad exacta de errores esperada
probarMundoInvalido :: String -> FilePath -> Int -> IO Bool
probarMundoInvalido nombre ruta esperados = do
  contenido <- leerArchivo ruta
  case leerMundo contenido of
    Right _ -> do
      putStrLn ("[FALLO] " ++ nombre ++ ": se esperaba error pero fue valido")
      pure False
    Left errs ->
      let n = length errs
          ok = n == esperados
      in do
        if ok
          then putStrLn ("[OK]    " ++ nombre ++ " (" ++ show n ++ " errores)")
          else putStrLn ("[FALLO] " ++ nombre ++ ": esperaba " ++ show esperados ++ " errores y dio " ++ show n)
        pure ok

mundoValidoEjemplo :: String
mundoValidoEjemplo = unlines
  [ "MUNDO: Prueba Base"
  , "INICIO: entrada"
  , "META: llevar llave_maestra a salida"
  , "CAPACIDAD: 4"
  , "---"
  , "OBJETO: llave_maestra"
  , "DESC: Abre la puerta final."
  , "PESO: 2"
  , "---"
  , "OBJETO: nota"
  , "DESC: Un papel arrugado."
  , "---"
  , "SALA: entrada"
  , "DESC: Inicio del recorrido."
  , "SALIDA: norte -> pasillo"
  , "OBJETO: nota"
  , "---"
  , "SALA: pasillo"
  , "DESC: Pasillo estrecho."
  , "SALIDA: sur -> entrada"
  , "SALIDA: este -> salida requiere llave_maestra"
  , "OBJETO: llave_maestra"
  , "---"
  , "SALA: salida"
  , "DESC: Meta del juego."
  , "SALIDA: oeste -> pasillo"
  ]

mundoConTildes :: String
mundoConTildes = unlines
  [ "MUNDO: Mundo con tildes"
  , "INICIO: cañón_básico"
  , "META: llegar a pingüino_final"
  , "---"
  , "SALA: cañón_básico"
  , "DESC: Sala con tildes y ene."
  , "SALIDA: norte -> pingüino_final"
  , "---"
  , "SALA: pingüino_final"
  , "DESC: Llegada."
  ]

mundoObjetoRepetidoMismaSala :: String
mundoObjetoRepetidoMismaSala = unlines
  [ "MUNDO: Objeto repetido en sala"
  , "INICIO: sala_a"
  , "META: llegar a sala_a"
  , "---"
  , "OBJETO: moneda"
  , "DESC: Moneda de oro."
  , "---"
  , "SALA: sala_a"
  , "DESC: Un cuarto."
  , "OBJETO: moneda"
  , "OBJETO: moneda"
  ]

mundoPesoInvalido :: String
mundoPesoInvalido = unlines
  [ "MUNDO: Peso cero"
  , "INICIO: sala_a"
  , "META: llegar a sala_a"
  , "CAPACIDAD: 3"
  , "---"
  , "OBJETO: pluma"
  , "DESC: Muy liviana."
  , "PESO: 0"
  , "---"
  , "SALA: sala_a"
  , "DESC: Un cuarto."
  , "OBJETO: pluma"
  ]

main :: IO ()
main = do
  hSetEncoding stdout utf8
  p1 <- ejecutarPrueba "leerMundo acepta mundo valido con capacidad y pesos" $
    case leerMundo mundoValidoEjemplo of
      Right m ->
        mundoTitulo m == "Prueba Base"
          && mundoCapacidad m == Just 4
          && pesoObjeto m (IdObjeto "llave_maestra") == 2
          && pesoObjeto m (IdObjeto "nota") == 1
          && Map.size (mundoSalas m) == 3
      Left _ -> False

  p2 <- ejecutarPrueba "leerMundo acepta identificadores con tildes, ñ y ü" $
    case leerMundo mundoConTildes of
      Right m -> mundoInicio m == IdSala "cañón_básico"
      Left _  -> False

  p3 <- ejecutarPrueba "leerMundo rechaza archivo vacio o solo comentarios" $
    case leerMundo "# solo comentario\n   \n" of
      Left [_] -> True
      _        -> False

  p4 <- probarMundoInvalido "mundos/invalidos/sintaxis_invalida.txt" "mundos/invalidos/sintaxis_invalida.txt" 5
  p5 <- probarMundoInvalido "mundos/invalidos/referencias_invalidas.txt" "mundos/invalidos/referencias_invalidas.txt" 7
  p6 <- probarMundoInvalido "mundos/invalidos/peso_y_alcance.txt" "mundos/invalidos/peso_y_alcance.txt" 5

  p7 <- ejecutarPrueba "leerMundo detecta objeto repetido en la misma sala (V7)" $
    case leerMundo mundoObjetoRepetidoMismaSala of
      Left [_] -> True
      _        -> False

  p8 <- ejecutarPrueba "leerMundo rechaza PESO: 0 en objeto" $
    case leerMundo mundoPesoInvalido of
      Left [_] -> True
      _        -> False

  p9 <- ejecutarPrueba "parsearSalida reconoce salida con y sin requisito" $
    parsearTodo parsearSalida "oeste -> biblioteca requiere carnet"
      == Just (Oeste, IdSala "biblioteca", Just (IdObjeto "carnet"))
      && parsearTodo parsearSalida "sur -> patio"
      == Just (Sur, IdSala "patio", Nothing)
      && parsearTodo parsearSalida "diagonal -> patio" == Nothing

  p10 <- ejecutarPrueba "parsearMeta e identificador reconocen formatos validos" $
    parsearTodo parsearMeta "llevar libro a biblioteca"
      == Just (LlevarA (IdObjeto "libro") (IdSala "biblioteca"))
      && parsearTodo parsearMeta "llegar a plaza"
      == Just (LlegarA (IdSala "plaza"))
      && parsearTodo identificador "SalaMayuscula" == Nothing

  p11 <- ejecutarPrueba "leerMundo no duplica error de campo faltante cuando META tiene mal formato" $
    let malMeta = unlines
          [ "MUNDO: Meta mal escrita"
          , "INICIO: sala_a"
          , "META: llegar sala_a"
          , "---"
          , "SALA: sala_a"
          , "DESC: Cuarto."
          ]
    in case leerMundo malMeta of
         Left [_] -> True
         _        -> False

  p12 <- ejecutarPrueba "parsearMeta exige espacios entre palabras clave" $
    parsearTodo parsearMeta "llegar asala" == Nothing
      && parsearTodo parsearMeta "llegarasala" == Nothing
      && parsearTodo parsearMeta "llevar libro abiblioteca" == Nothing

  p13 <- ejecutarPrueba "leerMundo soporta mismo ID para sala y objeto, ':' en DESC y bloques mixtos" $
    let mundoMixto = unlines
          [ "   # Comentario con espacios delante"
          , "MUNDO: Prueba: Casos borde"
          , "INICIO: cocina"
          , "META: llevar cocina a cocina"
          , "  ---  "
          , "SALA: cocina"
          , "DESC: Hora: 12:00. Una cocina normal."
          , "OBJETO: cocina"
          , "---"
          , "OBJETO: cocina"
          , "DESC: Una cocina en miniatura de juguete."
          ]
    in case leerMundo mundoMixto of
         Right m ->
           mundoInicio m == IdSala "cocina"
             && Map.member (IdSala "cocina") (mundoSalas m)
             && Map.member (IdObjeto "cocina") (mundoObjetos m)
         Left _ -> False

  let resultados = [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13]
      pasaron = length (filter id resultados)
      total = length resultados
  putStrLn ("Resumen: " ++ show pasaron ++ "/" ++ show total ++ " pruebas pasaron.")
  if all id resultados
    then exitSuccess
    else exitFailure
