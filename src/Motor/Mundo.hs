-- Modulo que se encarga de leer y validar los archivos de mundo

module Motor.Mundo
  ( leerMundo
  , parsearClaveValor
  , LineaConNumero
  , BloqueLineas
  ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
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

-- Distingue entre campo ausente, presente pero invalido y presente valido
-- para no reportar "falta campo" cuando el campo si estaba pero mal formado
data EstadoCampo a
  = Ausente
  | PresenteInvalido
  | Presente a

esAusente :: EstadoCampo a -> Bool
esAusente Ausente = True
esAusente _       = False

-- Parsea el primer bloque del archivo (cabecera del mundo)
parsearCabecera :: BloqueLineas -> Either [ErrorMundo] CabeceraCruda
parsearCabecera lns =
  let numInicio = lineaInicioBloque lns
      (errs, eTitulo, eInicio, eMeta, eCap) =
        foldl paso ([], Ausente, Ausente, Ausente, Ausente) lns
      faltantes =
        [ ErrorSintaxis numInicio "falta el campo obligatorio MUNDO en la cabecera"
        | esAusente eTitulo
        ] ++
        [ ErrorSintaxis numInicio "falta el campo obligatorio INICIO en la cabecera"
        | esAusente eInicio
        ] ++
        [ ErrorSintaxis numInicio "falta el campo obligatorio META en la cabecera"
        | esAusente eMeta
        ]
      todosErrs = errs ++ faltantes
      mCap = case eCap of
               Presente c -> Just c
               _          -> Nothing
  in case (todosErrs, eTitulo, eInicio, eMeta) of
       ([], Presente t, Presente ini, Presente meta) ->
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
              Ausente ->
                if null val
                  then (es ++ [ErrorSintaxis num "el titulo MUNDO no puede estar vacio"], PresenteInvalido, mi, mm, mc)
                  else (es, Presente val, mi, mm, mc)
              _ -> (es ++ [ErrorSintaxis num "clave MUNDO repetida en la cabecera"], mt, mi, mm, mc)
          "INICIO" ->
            case mi of
              Ausente ->
                case parsearTodo identificador val of
                  Just sid -> (es, mt, Presente (IdSala sid), mm, mc)
                  Nothing  -> (es ++ [ErrorSintaxis num ("identificador invalido en INICIO: '" ++ val ++ "'")], mt, PresenteInvalido, mm, mc)
              _ -> (es ++ [ErrorSintaxis num "clave INICIO repetida en la cabecera"], mt, mi, mm, mc)
          "META" ->
            case mm of
              Ausente ->
                case parsearTodo parsearMeta val of
                  Just meta -> (es, mt, mi, Presente meta, mc)
                  Nothing   -> (es ++ [ErrorSintaxis num ("formato invalido en META: '" ++ val ++ "'")], mt, mi, PresenteInvalido, mc)
              _ -> (es ++ [ErrorSintaxis num "clave META repetida en la cabecera"], mt, mi, mm, mc)
          "CAPACIDAD" ->
            case mc of
              Ausente ->
                case parsearTodo enteroPositivo val of
                  Just cap -> (es, mt, mi, mm, Presente cap)
                  Nothing  -> (es ++ [ErrorSintaxis num ("CAPACIDAD debe ser un entero positivo: '" ++ val ++ "'")], mt, mi, mm, PresenteInvalido)
              _ -> (es ++ [ErrorSintaxis num "clave CAPACIDAD repetida en la cabecera"], mt, mi, mm, mc)
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
  let (errs, eDesc, ePeso) = foldl paso ([], Ausente, Ausente) lns
      faltante =
        [ ErrorSintaxis numInicio ("falta DESC en el objeto '" ++ unIdObjeto oid ++ "'")
        | esAusente eDesc
        ]
      todosErrs = errs ++ faltante
      peso = case ePeso of
               Presente p -> p
               _          -> 1
  in case (todosErrs, eDesc) of
       ([], Presente desc) -> Right (Objeto oid desc peso)
       _                   -> Left todosErrs
  where
    paso (es, md, mp) ln@(num, _) =
      case parsearClaveValor ln of
        Left e -> (es ++ [e], md, mp)
        Right (clave, val) -> case clave of
          "DESC" ->
            case md of
              Ausente ->
                if null val
                  then (es ++ [ErrorSintaxis num "la descripcion DESC no puede estar vacia"], PresenteInvalido, mp)
                  else (es, Presente val, mp)
              _ -> (es ++ [ErrorSintaxis num "clave DESC repetida en objeto"], md, mp)
          "PESO" ->
            case mp of
              Ausente ->
                case parsearTodo enteroPositivo val of
                  Just p  -> (es, md, Presente p)
                  Nothing -> (es ++ [ErrorSintaxis num ("PESO debe ser un entero positivo: '" ++ val ++ "'")], md, PresenteInvalido)
              _ -> (es ++ [ErrorSintaxis num "clave PESO repetida en objeto"], md, mp)
          "OBJETO" ->
            (es ++ [ErrorSintaxis num "clave OBJETO repetida dentro del bloque"], md, mp)
          otra ->
            (es ++ [ErrorSintaxis num ("clave desconocida en objeto: '" ++ otra ++ "'")], md, mp)

-- Parsea las lineas internas de un bloque SALA
parsearBloqueSala :: Int -> IdSala -> [LineaConNumero] -> Either [ErrorMundo] SalaCruda
parsearBloqueSala numInicio sid lns =
  let (errs, eDesc, salidasRev, objetosRev) = foldl paso ([], Ausente, [], []) lns
      faltante =
        [ ErrorSintaxis numInicio ("falta DESC en la sala '" ++ unIdSala sid ++ "'")
        | esAusente eDesc
        ]
      todosErrs = errs ++ faltante
  in case (todosErrs, eDesc) of
       ([], Presente desc) ->
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
              Ausente ->
                if null val
                  then (es ++ [ErrorSintaxis num "la descripcion DESC no puede estar vacia"], PresenteInvalido, ss, os)
                  else (es, Presente val, ss, os)
              _ -> (es ++ [ErrorSintaxis num "clave DESC repetida en sala"], md, ss, os)
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

-- Devuelve los elementos repetidos de una lista conservando el orden de aparicion
duplicados :: Ord a => [a] -> [a]
duplicados = reverse . (\(ds, _, _) -> ds) . foldl paso ([], Set.empty, Set.empty)
  where
    paso (dups, vistos, reportados) x
      | Set.member x vistos =
          if Set.member x reportados
            then (dups, vistos, reportados)
            else (x : dups, vistos, Set.insert x reportados)
      | otherwise = (dups, Set.insert x vistos, reportados)

-- Recorre el grafo de salas desde una sala inicial ignorando requisitos
salasAlcanzables :: IdSala -> Map.Map IdSala [IdSala] -> Set IdSala
salasAlcanzables inicio ady = bfs [inicio] (Set.singleton inicio)
  where
    bfs [] visitados = visitados
    bfs (u:cola) visitados =
      let vecinos = Map.findWithDefault [] u ady
          nuevos  = filter (`Set.notMember` visitados) vecinos
          visitados' = foldl (flip Set.insert) visitados nuevos
      in bfs (cola ++ nuevos) visitados'

-- Ejecuta todas las validaciones semanticas V2-V10 y las de Variante 1 (Peso)
validarMundo :: MundoCrudo -> [ErrorMundo]
validarMundo (MundoCrudo cab objs sals) =
  errV2 ++ errV3 ++ errV4 ++ errV5 ++ errV6 ++ errV7 ++ errV8 ++ errV9 ++ errV10 ++ errPeso
  where
    idsSalas = map scId sals
    setSalas = Set.fromList idsSalas
    idsObjs  = map objId objs
    setObjs  = Set.fromList idsObjs
    mapaObjs = Map.fromList [ (objId o, o) | o <- objs ]

    -- V2: identificadores de salas u objetos duplicados
    errV2 =
      [ ErrorValidacion ("identificador de sala duplicado: '" ++ unIdSala sid ++ "'")
      | sid <- duplicados idsSalas
      ] ++
      [ ErrorValidacion ("identificador de objeto duplicado: '" ++ unIdObjeto oid ++ "'")
      | oid <- duplicados idsObjs
      ]

    -- V3: salidas hacia salas que no existen
    errV3 =
      [ ErrorValidacion ("la sala '" ++ unIdSala (scId s) ++ "' tiene una salida hacia una sala inexistente: '" ++ unIdSala dest ++ "'")
      | s <- sals
      , (_, dest, _) <- scSalidas s
      , Set.notMember dest setSalas
      ]

    -- V4: salidas que requieren objetos que no existen
    errV4 =
      [ ErrorValidacion ("la sala '" ++ unIdSala (scId s) ++ "' tiene una salida que requiere un objeto inexistente: '" ++ unIdObjeto req ++ "'")
      | s <- sals
      , (_, _, Just req) <- scSalidas s
      , Set.notMember req setObjs
      ]

    -- V5: salas con dos o mas salidas en la misma direccion
    errV5 =
      [ ErrorValidacion ("la sala '" ++ unIdSala (scId s) ++ "' tiene salidas duplicadas en direccion '" ++ mostrarDireccion dir ++ "'")
      | s <- sals
      , dir <- duplicados [ d | (d, _, _) <- scSalidas s ]
      ]

    -- V6: salas que contienen objetos no definidos
    errV6 =
      [ ErrorValidacion ("la sala '" ++ unIdSala (scId s) ++ "' contiene un objeto no definido: '" ++ unIdObjeto oid ++ "'")
      | s <- sals
      , oid <- scObjetos s
      , Set.notMember oid setObjs
      ]

    -- V7: objetos colocados en mas de una sala o repetidos en la misma
    colocaciones = [ (oid, scId s) | s <- sals, oid <- scObjetos s ]
    conteoPorObj = Map.fromListWith (+) [ (oid, 1 :: Int) | (oid, _) <- colocaciones ]
    errV7 =
      [ ErrorValidacion ("el objeto '" ++ unIdObjeto oid ++ "' esta colocado mas de una vez en el mundo")
      | (oid, n) <- Map.toList conteoPorObj
      , n > 1
      ]

    -- V8: INICIO o META con salas u objetos inexistentes
    errV8 =
      [ ErrorValidacion ("la sala de INICIO no existe: '" ++ unIdSala (ccInicio cab) ++ "'")
      | Set.notMember (ccInicio cab) setSalas
      ] ++
      case ccMeta cab of
        LlegarA sid ->
          [ ErrorValidacion ("la sala de la META no existe: '" ++ unIdSala sid ++ "'")
          | Set.notMember sid setSalas
          ]
        LlevarA oid sid ->
          [ ErrorValidacion ("el objeto de la META no existe: '" ++ unIdObjeto oid ++ "'")
          | Set.notMember oid setObjs
          ] ++
          [ ErrorValidacion ("la sala de la META no existe: '" ++ unIdSala sid ++ "'")
          | Set.notMember sid setSalas
          ]

    -- V9: salas no alcanzables desde INICIO (ignorando requisitos)
    adyacencias = Map.fromListWith (++)
      [ (scId s, [ dest | (_, dest, _) <- scSalidas s, Set.member dest setSalas ])
      | s <- sals
      ]
    alcanzables = salasAlcanzables (ccInicio cab) adyacencias
    errV9 =
      if Set.notMember (ccInicio cab) setSalas
        then []
        else [ ErrorValidacion ("la sala '" ++ unIdSala sid ++ "' no es alcanzable desde INICIO")
             | sid <- Set.toList setSalas
             , Set.notMember sid alcanzables
             ]

    -- V10: objetos definidos necesarios (meta o requisito) que no estan colocados en ninguna sala
    colocados = Set.fromList (map fst colocaciones)
    reqsSalida = [ req | s <- sals, (_, _, Just req) <- scSalidas s ]
    objsMeta = case ccMeta cab of
                 LlegarA _     -> []
                 LlevarA oid _ -> [oid]
    necesarios = Set.toList (Set.fromList (objsMeta ++ reqsSalida))
    errV10 =
      [ ErrorValidacion ("el objeto necesario '" ++ unIdObjeto oid ++ "' no esta colocado en ninguna sala")
      | oid <- necesarios
      , Set.member oid setObjs
      , Set.notMember oid colocados
      ]

    -- Validaciones adicionales de Variante 1 (Peso)
    errPeso = case ccCapacidad cab of
      Nothing  -> []
      Just cap -> errPesoIndividual cap ++ errPesoMetaReqs cap

    errPesoIndividual cap =
      [ ErrorValidacion ("el objeto '" ++ unIdObjeto (objId o) ++ "' pesa " ++ show (objPeso o)
                         ++ " y supera la capacidad maxima (" ++ show cap ++ ")")
      | o <- objs
      , objPeso o > cap
      ]

    errPesoMetaReqs cap = case ccMeta cab of
      LlegarA _ -> []
      LlevarA oidMeta sidMeta ->
        let yaEnMeta = any (\s -> scId s == sidMeta && oidMeta `elem` scObjetos s) sals
            entradas = [ mReq | s <- sals, (_, dest, mReq) <- scSalidas s, dest == sidMeta ]
            pesoM    = maybe 1 objPeso (Map.lookup oidMeta mapaObjs)
            reqsValid = [ req | Just req <- entradas, req /= oidMeta, Set.member req setObjs ]
            cabeConAlguno = any (\r -> pesoM + maybe 1 objPeso (Map.lookup r mapaObjs) <= cap) reqsValid
        in [ ErrorValidacion ("el objeto de la meta '" ++ unIdObjeto oidMeta
                              ++ "' no puede llevarse junto con ningun requisito de entrada a '"
                              ++ unIdSala sidMeta ++ "' sin superar la capacidad (" ++ show cap ++ ")")
           | not yaEnMeta
           , Set.member oidMeta setObjs
           , not (null entradas)
           , all isJust entradas
           , not (null reqsValid)
           , not cabeConAlguno
           ]

-- Convierte un MundoCrudo ya validado en el Mundo final
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
       Right crudo ->
         case validarMundo crudo of
           []   -> Right (construirMundo crudo)
           errs -> Left errs
