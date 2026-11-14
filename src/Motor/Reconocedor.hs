-- | Combinadores de reconocimiento construidos sobre ReadS
-- para el análisis del archivo de mundo.
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
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

-- | Reconoce un carácter que cumple el predicado dado.
satisface :: (Char -> Bool) -> ReadS Char
satisface p (c:cs) | p c = [(c, cs)]
satisface _ _            = []

-- | Reconoce un carácter específico.
caracter :: Char -> ReadS Char
caracter c = satisface (== c)

-- | Reconoce una cadena exacta de caracteres.
cadena :: String -> ReadS String
cadena ""     s = [("", s)]
cadena (c:cs) s =
  [ (c:cs, r2)
  | (_, r1) <- caracter c s
  , (_, r2) <- cadena cs r1
  ]

-- | Alternancia: prueba ambas lecturas posibles.
infixr 3 <|>
(<|>) :: ReadS a -> ReadS a -> ReadS a
(p <|> q) s = p s ++ q s

-- | Cero o más repeticiones de un reconocedor.
muchos :: ReadS a -> ReadS [a]
muchos p = alMenosUno p <|> ninguno
  where
    ninguno s = [([], s)]

-- | Una o más repeticiones de un reconocedor.
alMenosUno :: ReadS a -> ReadS [a]
alMenosUno p s =
  [ (x:xs, r2)
  | (x, r1)  <- p s
  , (xs, r2) <- muchos p r1
  ]

-- | Reconoce cero o más espacios en blanco.
espacios :: ReadS String
espacios = muchos (satisface isSpace)

-- | Reconoce un carácter alfanumérico.
alfanumerico :: ReadS Char
alfanumerico = satisface isAlphaNum

-- | Reconoce una letra.
letra :: ReadS Char
letra = satisface isAlpha

-- | Reconoce un dígito numérico.
digito :: ReadS Char
digito = satisface isDigit
