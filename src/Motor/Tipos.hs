-- | Tipos base del dominio del motor de aventuras (Paso 0 + Parte 2).
-- Incluye los tipos del mundo acordados entre ambos integrantes y los
-- tipos de estado, comandos y guardado correspondientes a la Parte 2
-- (Nadheska García, 19-10054) con soporte para la Variante 1 (Peso).
module Motor.Tipos
  ( IdSala (..)
  , IdObjeto (..)
  , Direccion (..)
  , mostrarDireccion
  , Objeto (..)
  , Salida (..)
  , Sala (..)
  , Meta (..)
  , Mundo (..)
  , ErrorMundo (..)
  , formatearError
  , Comando (..)
  , Estado (..)
  , PartidaGuardada (..)
  , pesoObjeto
  , pesoInventario
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Identificador tipado de una sala (espacio de nombres separado).
newtype IdSala = IdSala { unIdSala :: String }
  deriving (Show, Read, Eq, Ord)

-- | Identificador tipado de un objeto (espacio de nombres separado).
newtype IdObjeto = IdObjeto { unIdObjeto :: String }
  deriving (Show, Read, Eq, Ord)

-- | Direcciones válidas de movimiento entre salas.
data Direccion
  = Norte
  | Sur
  | Este
  | Oeste
  | Arriba
  | Abajo
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Representación textual en minúsculas de una dirección.
mostrarDireccion :: Direccion -> String
mostrarDireccion Norte  = "norte"
mostrarDireccion Sur    = "sur"
mostrarDireccion Este   = "este"
mostrarDireccion Oeste  = "oeste"
mostrarDireccion Arriba = "arriba"
mostrarDireccion Abajo  = "abajo"

-- | Un objeto del mundo, con identificador, descripción y peso (>= 1).
data Objeto = Objeto
  { objId   :: IdObjeto
  , objDesc :: String
  , objPeso :: Int
  } deriving (Show, Read, Eq)

-- | Una salida desde una sala hacia otra, opcionalmente bloqueada por un objeto.
data Salida = Salida
  { salDestino   :: IdSala
  , salRequisito :: Maybe IdObjeto
  } deriving (Show, Read, Eq)

-- | Una sala del mundo con descripción, salidas y objetos iniciales.
data Sala = Sala
  { salaId      :: IdSala
  , salaDesc    :: String
  , salaSalidas :: Map Direccion Salida
  , salaObjetos :: [IdObjeto]
  } deriving (Show, Read, Eq)

-- | Condición de victoria del mundo.
data Meta
  = LlegarA IdSala
  | LlevarA IdObjeto IdSala
  deriving (Show, Read, Eq)

-- | Definición completa e inmutable de un mundo válido.
-- 'mundoCapacidad' es 'Nothing' si el archivo no declara @CAPACIDAD:@
-- (compatibilidad con mundos sin peso explícito) o @Just n@ con @n >= 1@.
data Mundo = Mundo
  { mundoTitulo    :: String
  , mundoInicio    :: IdSala
  , mundoMeta      :: Meta
  , mundoCapacidad :: Maybe Int
  , mundoSalas     :: Map IdSala Sala
  , mundoObjetos   :: Map IdObjeto Objeto
  } deriving (Show, Read, Eq)

-- | Errores de análisis sintáctico o validación semántica de un mundo.
data ErrorMundo
  = ErrorSintaxis Int String
  | ErrorValidacion String
  deriving (Show, Read, Eq)

-- | Formatea un 'ErrorMundo' garantizando el prefijo obligatorio @Error: @.
formatearError :: ErrorMundo -> String
formatearError (ErrorSintaxis linea msg) =
  "Error: línea " ++ show linea ++ ": " ++ msg
formatearError (ErrorValidacion msg) =
  "Error: " ++ msg

-- | Comandos aceptados por el intérprete del motor.
data Comando
  = Ir Direccion
  | Mirar
  | Tomar IdObjeto
  | Soltar IdObjeto
  | Examinar IdObjeto
  | Inventario
  | Guardar FilePath
  | Cargar FilePath
  | Ayuda
  | Salir
  deriving (Show, Read, Eq)

-- | Estado completo de una partida en curso.
-- Mantiene una referencia al 'Mundo' inmutable y separa lo que cambia
-- durante el juego: sala actual, inventario y objetos presentes en cada sala.
data Estado = Estado
  { estMundo      :: Mundo
  , estSalaActual :: IdSala
  , estInventario :: [IdObjeto]
  , estSalasObjs  :: Map IdSala [IdObjeto]
  , estTerminado  :: Bool
  , estSalir      :: Bool
  } deriving (Show, Read, Eq)

-- | Instantánea serializable para guardar y cargar partidas.
-- Guarda únicamente lo que cambia junto al título del mundo para verificar
-- compatibilidad al cargar.
data PartidaGuardada = PartidaGuardada
  { pgTitulo     :: String
  , pgSalaActual :: IdSala
  , pgInventario :: [IdObjeto]
  , pgSalasObjs  :: Map IdSala [IdObjeto]
  } deriving (Show, Read, Eq)

-- | Obtiene el peso de un objeto registrado en el mundo (1 por defecto si no figura).
pesoObjeto :: Mundo -> IdObjeto -> Int
pesoObjeto mundo oid =
  maybe 1 objPeso (Map.lookup oid (mundoObjetos mundo))

-- | Calcula el peso total de una lista de objetos en el inventario.
pesoInventario :: Mundo -> [IdObjeto] -> Int
pesoInventario mundo = sum . map (pesoObjeto mundo)
