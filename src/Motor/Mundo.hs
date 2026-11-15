-- Modulo que se encarga de leer y validar los archivos de mundo

module Motor.Mundo
  ( leerMundo
  , parsearClaveValor
  , LineaConNumero
  , BloqueLineas
  ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as Map
import Motor.Reconocedor
  ( enteroPositivo
  , identificador
  , parsearMeta
  , parsearSalida
  , parsearTodo
  )
import Motor.Tipos

-- Una linea del archivo con su numero de linea original (1-based)
type LineaConNumero = (Int, String)

-- Un bloque de lineas delimitado por lineas con ---
type BloqueLineas = [LineaConNumero]

-- Tipos intermedios crudos para conservar duplicados antes de validar V2-V10
data CabeceraCruda = CabeceraCruda
  { ccTitulo    :: String
  , ccInicio    :: IdSala
  , ccMeta      :: Meta
  , ccCapacidad :: Maybe Int
  }

data SalaCruda = SalaCruda
  { scId      :: IdSala
  , scDesc    :: String
  , scSalidas :: [(Direccion, IdSala, Maybe IdObjeto)]
  , scObjetos :: [IdObjeto]
  }

data MundoCrudo = MundoCrudo CabeceraCruda [Objeto] [SalaCruda]

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

-- Extrae la linea de inicio de un bloque para reportar campos faltantes
lineaInicioBloque :: BloqueLineas -> Int
lineaInicioBloque []          = 1
lineaInicioBloque ((n, _):_)  = n

-- Parsea el primer bloque del archivo (cabecera del mundo)
parsearCabecera :: BloqueLineas -> Either [ErrorMundo] CabeceraCruda
parsearCabecera lns =
  let numInicio = lineaInicioBloque lns
      (errs, mTitulo, mInicio, mMeta, mCap) =
        foldl paso ([], Nothing, Nothing, Nothing, Nothing) lns
      faltantes =
        [ ErrorSintaxis numInicio "falta el campo obligatorio MUNDO en la cabecera"
        | Nothing <- [mTitulo]
        ] ++
        [ ErrorSintaxis numInicio "falta el campo obligatorio INICIO en la cabecera"
        | Nothing <- [mInicio]
        ] ++
        [ ErrorSintaxis numInicio "falta el campo obligatorio META en la cabecera"
        | Nothing <- [mMeta]
        ]
      todosErrs = errs ++ faltantes
  in case (todosErrs, mTitulo, mInicio, mMeta) of
       ([], Just t, Just ini, Just meta) ->
         Right (CabeceraCruda t ini meta mCap)
       _ ->
         Left todosErrs
  where
    paso (es, mt, mi, mm, mc) ln@(num, _) =
      case parsearClaveValor ln of
        Left e -> (es ++ [e], mt, mi, mm, mc)
        Right (clave, val) -> case clave of
          "MUNDO" ->
            case mt of
              Just _ -> (es ++ [ErrorSintaxis num "clave MUNDO repetida en la cabecera"], mt, mi, mm, mc)
              Nothing ->
                if null val
                  then (es ++ [ErrorSintaxis num "el titulo MUNDO no puede estar vacio"], mt, mi, mm, mc)
                  else (es, Just val, mi, mm, mc)
          "INICIO" ->
            case mi of
              Just _ -> (es ++ [ErrorSintaxis num "clave INICIO repetida en la cabecera"], mt, mi, mm, mc)
              Nothing ->
                case parsearTodo identificador val of
                  Just sid -> (es, mt, Just (IdSala sid), mm, mc)
                  Nothing  -> (es ++ [ErrorSintaxis num ("identificador invalido en INICIO: '" ++ val ++ "'")], mt, mi, mm, mc)
          "META" ->
            case mm of
              Just _ -> (es ++ [ErrorSintaxis num "clave META repetida en la cabecera"], mt, mi, mm, mc)
              Nothing ->
                case parsearTodo parsearMeta val of
                  Just meta -> (es, mt, mi, Just meta, mc)
                  Nothing   -> (es ++ [ErrorSintaxis num ("formato invalido en META: '" ++ val ++ "'")], mt, mi, mm, mc)
          "CAPACIDAD" ->
            case mc of
              Just _ -> (es ++ [ErrorSintaxis num "clave CAPACIDAD repetida en la cabecera"], mt, mi, mm, mc)
              Nothing ->
                case parsearTodo enteroPositivo val of
                  Just cap -> (es, mt, mi, mm, Just cap)
                  Nothing  -> (es ++ [ErrorSintaxis num ("CAPACIDAD debe ser un entero positivo: '" ++ val ++ "'")], mt, mi, mm, mc)
          otra ->
            (es ++ [ErrorSintaxis num ("clave desconocida en cabecera: '" ++ otra ++ "'")], mt, mi, mm, mc)

-- Decide si un bloque es de OBJETO o de SALA segun su primera linea
parsearBloque :: BloqueLineas -> Either [ErrorMundo] (Either Objeto SalaCruda)
parsearBloque [] = Left [ErrorSintaxis 1 "bloque vacio"]
parsearBloque (primera@(num, _):resto) =
  case parsearClaveValor primera of
    Left e -> Left [e]
    Right ("OBJETO", val) ->
      case parsearTodo identificador val of
        Just oid ->
          case parsearBloqueObjeto num (IdObjeto oid) resto of
            Left es -> Left es
            Right o -> Right (Left o)
        Nothing ->
          Left [ErrorSintaxis num ("identificador de objeto invalido: '" ++ val ++ "'")]
    Right ("SALA", val) ->
      case parsearTodo identificador val of
        Just sid ->
          case parsearBloqueSala num (IdSala sid) resto of
            Left es -> Left es
            Right s -> Right (Right s)
        Nothing ->
          Left [ErrorSintaxis num ("identificador de sala invalido: '" ++ val ++ "'")]
    Right (clave, _) ->
      Left [ErrorSintaxis num ("un bloque debe empezar por OBJETO o SALA, no por '" ++ clave ++ "'")]

-- Parsea las lineas internas de un bloque OBJETO
parsearBloqueObjeto :: Int -> IdObjeto -> [LineaConNumero] -> Either [ErrorMundo] Objeto
parsearBloqueObjeto numInicio oid lns =
  let (errs, mDesc, mPeso) = foldl paso ([], Nothing, Nothing) lns
      faltante =
        [ ErrorSintaxis numInicio ("falta DESC en el objeto '" ++ unIdObjeto oid ++ "'")
        | Nothing <- [mDesc]
        ]
      todosErrs = errs ++ faltante
  in case (todosErrs, mDesc) of
       ([], Just desc) ->
         let peso = maybe 1 id mPeso
         in Right (Objeto oid desc peso)
       _ ->
         Left todosErrs
  where
    paso (es, md, mp) ln@(num, _) =
      case parsearClaveValor ln of
        Left e -> (es ++ [e], md, mp)
        Right (clave, val) -> case clave of
          "DESC" ->
            case md of
              Just _ -> (es ++ [ErrorSintaxis num "clave DESC repetida en objeto"], md, mp)
              Nothing ->
                if null val
                  then (es ++ [ErrorSintaxis num "la descripcion DESC no puede estar vacia"], md, mp)
                  else (es, Just val, mp)
          "PESO" ->
            case mp of
              Just _ -> (es ++ [ErrorSintaxis num "clave PESO repetida en objeto"], md, mp)
              Nothing ->
                case parsearTodo enteroPositivo val of
                  Just p  -> (es, md, Just p)
                  Nothing -> (es ++ [ErrorSintaxis num ("PESO debe ser un entero positivo: '" ++ val ++ "'")], md, mp)
          "OBJETO" ->
            (es ++ [ErrorSintaxis num "clave OBJETO repetida dentro del bloque"], md, mp)
          otra ->
            (es ++ [ErrorSintaxis num ("clave desconocida en objeto: '" ++ otra ++ "'")], md, mp)

-- Parsea las lineas internas de un bloque SALA
parsearBloqueSala :: Int -> IdSala -> [LineaConNumero] -> Either [ErrorMundo] SalaCruda
parsearBloqueSala numInicio sid lns =
  let (errs, mDesc, salidasRev, objetosRev) = foldl paso ([], Nothing, [], []) lns
      faltante =
        [ ErrorSintaxis numInicio ("falta DESC en la sala '" ++ unIdSala sid ++ "'")
        | Nothing <- [mDesc]
        ]
      todosErrs = errs ++ faltante
  in case (todosErrs, mDesc) of
       ([], Just desc) ->
         Right (SalaCruda sid desc (reverse salidasRev) (reverse objetosRev))
       _ ->
         Left todosErrs
  where
    paso (es, md, ss, os) ln@(num, _) =
      case parsearClaveValor ln of
        Left e -> (es ++ [e], md, ss, os)
        Right (clave, val) -> case clave of
          "DESC" ->
            case md of
              Just _ -> (es ++ [ErrorSintaxis num "clave DESC repetida en sala"], md, ss, os)
              Nothing ->
                if null val
                  then (es ++ [ErrorSintaxis num "la descripcion DESC no puede estar vacia"], md, ss, os)
                  else (es, Just val, ss, os)
          "SALIDA" ->
            case parsearTodo parsearSalida val of
              Just sal -> (es, md, sal : ss, os)
              Nothing  -> (es ++ [ErrorSintaxis num ("salida mal formada: '" ++ val ++ "'")], md, ss, os)
          "OBJETO" ->
            case parsearTodo identificador val of
              Just oid -> (es, md, ss, IdObjeto oid : os)
              Nothing  -> (es ++ [ErrorSintaxis num ("identificador de objeto invalido en sala: '" ++ val ++ "'")], md, ss, os)
          "SALA" ->
            (es ++ [ErrorSintaxis num "clave SALA repetida dentro del bloque"], md, ss, os)
          otra ->
            (es ++ [ErrorSintaxis num ("clave desconocida en sala: '" ++ otra ++ "'")], md, ss, os)

-- Parsea todos los bloques del mundo acumulando todos los errores de sintaxis V1
parsearMundoCrudo :: [BloqueLineas] -> Either [ErrorMundo] MundoCrudo
parsearMundoCrudo [] =
  Left [ErrorSintaxis 1 "el archivo de mundo esta vacio o solo contiene comentarios"]
parsearMundoCrudo (cab:resto) =
  let resCab = parsearCabecera cab
      resBloques = map parsearBloque resto
      errsCab = either id (const []) resCab
      errsBloques = concat [ es | Left es <- resBloques ]
      todosErrs = errsCab ++ errsBloques
  in case (todosErrs, resCab) of
       ([], Right c) ->
         let objs = [ o | Right (Left o)  <- resBloques ]
             sals = [ s | Right (Right s) <- resBloques ]
         in Right (MundoCrudo c objs sals)
       _ ->
         Left todosErrs

-- Convierte un MundoCrudo en el Mundo final (las validaciones V2-V10 se anadiran en el siguiente paso)
construirMundo :: MundoCrudo -> Mundo
construirMundo (MundoCrudo cab objs sals) =
  Mundo
    { mundoTitulo    = ccTitulo cab
    , mundoInicio    = ccInicio cab
    , mundoMeta      = ccMeta cab
    , mundoCapacidad = ccCapacidad cab
    , mundoSalas     = Map.fromList [ (scId s, convertirSala s) | s <- sals ]
    , mundoObjetos   = Map.fromList [ (objId o, o) | o <- objs ]
    }
  where
    convertirSala s =
      Sala
        { salaId      = scId s
        , salaDesc    = scDesc s
        , salaSalidas = Map.fromList [ (dir, Salida dest req) | (dir, dest, req) <- scSalidas s ]
        , salaObjetos = scObjetos s
        }

-- Punto de entrada puro para parsear y validar un mundo
leerMundo :: String -> Either [ErrorMundo] Mundo
leerMundo contenido =
  let utiles = limpiarLineas contenido
      bloques = separarBloques utiles
  in case parsearMundoCrudo bloques of
       Left errs   -> Left errs
       Right crudo -> Right (construirMundo crudo)
