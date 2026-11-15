-- Modulo de combinadores de reconocimiento sobre ReadS
-- para analizar las lineas y componentes del archivo de mundo

module Motor.Reconocedor
  ( caracter
  , satisface
  , cadena
  , (<|>)
  , muchos
  , alMenosUno
  , espacios
  , alfanumerico
  , letra
  , digito
  , identificador
  , enteroPositivo
  , parsearDireccion
  , parsearSalida
  , parsearMeta
  , parsearTodo
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isSpace)
import Motor.Tipos (Direccion (..), IdObjeto (..), IdSala (..), Meta (..))

-- Reconoce un caracter que cumpla un predicado
satisface :: (Char -> Bool) -> ReadS Char
satisface p (c:cs) | p c = [(c, cs)]
satisface _ _            = []

-- Reconoce un caracter especifico
caracter :: Char -> ReadS Char
caracter c = satisface (== c)

-- Reconoce una secuencia exacta de caracteres
cadena :: String -> ReadS String
cadena ""     s = [("", s)]
cadena (c:cs) s =
  [ (c:cs, r2)
  | (_, r1) <- caracter c s
  , (_, r2) <- cadena cs r1
  ]

-- Alternancia de combinadores: prueba ambas opciones
infixr 3 <|>
(<|>) :: ReadS a -> ReadS a -> ReadS a
(p <|> q) s = p s ++ q s

-- Cero o mas repeticiones
muchos :: ReadS a -> ReadS [a]
muchos p = alMenosUno p <|> ninguno
  where
    ninguno s = [([], s)]

-- Una o mas repeticiones
alMenosUno :: ReadS a -> ReadS [a]
alMenosUno p s =
  [ (x:xs, r2)
  | (x, r1)  <- p s
  , (xs, r2) <- muchos p r1
  ]

-- Cero o mas espacios en blanco
espacios :: ReadS String
espacios = muchos (satisface isSpace)

-- Un caracter alfanumerico estandar
alfanumerico :: ReadS Char
alfanumerico = satisface isAlphaNum

-- Una letra estandar
letra :: ReadS Char
letra = satisface isAlpha

-- Un digito numerico (0-9)
digito :: ReadS Char
digito = satisface isDigit

-- Caracter permitido en identificadores de salas u objetos:
-- letras minusculas (incluyendo vocales con tilde, ñ y ü), digitos y guion bajo
esCharIdentificador :: Char -> Bool
esCharIdentificador c =
  (isAlpha c && isLower c) || isDigit c || c == '_' || c `elem` "áéíóúñü"

-- Parsea un identificador segun las reglas del enunciado (sin espacios)
identificador :: ReadS String
identificador = alMenosUno (satisface esCharIdentificador)

-- Parsea un numero entero estrictamente positivo (> 0)
-- sin usar la funcion parcial 'read'
enteroPositivo :: ReadS Int
enteroPositivo s =
  [ (val, r1)
  | (ds, r1) <- alMenosUno digito s
  , let val = foldl (\acc d -> acc * 10 + (fromEnum d - fromEnum '0')) 0 ds
  , val > 0
  ]

-- Parsea una direccion de movimiento valida en espanol
parsearDireccion :: ReadS Direccion
parsearDireccion =
  dirNorte <|> dirSur <|> dirEste <|> dirOeste <|> dirArriba <|> dirAbajo
  where
    dirNorte  s = [ (Norte,  r) | ("norte",  r) <- cadena "norte" s ]
    dirSur    s = [ (Sur,    r) | ("sur",    r) <- cadena "sur" s ]
    dirEste   s = [ (Este,   r) | ("este",   r) <- cadena "este" s ]
    dirOeste  s = [ (Oeste,  r) | ("oeste",  r) <- cadena "oeste" s ]
    dirArriba s = [ (Arriba, r) | ("arriba", r) <- cadena "arriba" s ]
    dirAbajo  s = [ (Abajo,  r) | ("abajo",  r) <- cadena "abajo" s ]

-- Parsea una declaracion de salida
-- Formas: "<dir> -> <sala>" o "<dir> -> <sala> requiere <obj>"
parsearSalida :: ReadS (Direccion, IdSala, Maybe IdObjeto)
parsearSalida = conReq <|> sinReq
  where
    conReq s1 =
      [ ((dir, IdSala dest, Just (IdObjeto req)), r7)
      | (dir, r1)        <- parsearDireccion s1
      , (_, r2)          <- espacios r1
      , ("->", r3)       <- cadena "->" r2
      , (_, r4)          <- espacios r3
      , (dest, r5)       <- identificador r4
      , (_, r6)          <- alMenosUno (satisface isSpace) r5
      , ("requiere", r7') <- cadena "requiere" r6
      , (_, r8)          <- alMenosUno (satisface isSpace) r7'
      , (req, r7)        <- identificador r8
      ]
    sinReq s1 =
      [ ((dir, IdSala dest, Nothing), r5)
      | (dir, r1)  <- parsearDireccion s1
      , (_, r2)    <- espacios r1
      , ("->", r3) <- cadena "->" r2
      , (_, r4)    <- espacios r3
      , (dest, r5) <- identificador r4
      ]

-- Parsea la condicion de meta del mundo
-- Formas: "llegar a <sala>" o "llevar <objeto> a <sala>"
parsearMeta :: ReadS Meta
parsearMeta = metaLlegar <|> metaLlevar
  where
    esp1 = alMenosUno (satisface isSpace)
    metaLlegar s1 =
      [ (LlegarA (IdSala sala), r4)
      | ("llegar", r1) <- cadena "llegar" s1
      , (_, r2)        <- esp1 r1
      , ("a", r3)      <- cadena "a" r2
      , (_, r4')       <- esp1 r3
      , (sala, r4)     <- identificador r4'
      ]
    metaLlevar s1 =
      [ (LlevarA (IdObjeto obj) (IdSala sala), r6)
      | ("llevar", r1) <- cadena "llevar" s1
      , (_, r2)        <- esp1 r1
      , (obj, r3)      <- identificador r2
      , (_, r4)        <- esp1 r3
      , ("a", r5)      <- cadena "a" r4
      , (_, r6')       <- esp1 r5
      , (sala, r6)     <- identificador r6'
      ]

-- Ejecuta un reconocedor exigiendo que se consuma toda la entrada
-- (ignorando espacios finales) y resuelve la ambiguedad tomando la lectura completa
parsearTodo :: ReadS a -> String -> Maybe a
parsearTodo p s =
  case [ x | (x, r) <- p s, all isSpace r ] of
    (x:_) -> Just x
    []    -> Nothing
