# Decisiones de Arquitectura

## Samuel Sanabria (20-10591)

### D1. Representación del mundo, identificadores e invariantes

**Contexto:**  
Al modelar el mundo del juego había que decidir dos cosas: si los identificadores de salas y objetos serían simples `String` o tipos propios, y si `Mundo` guardaría las salas, salidas y objetos en listas de pares o en `Data.Map.Strict.Map`. Además, durante la lectura del archivo pueden aparecer salas repetidas (V2) o salidas repetidas en la misma dirección (V5), cosas que un `Map` sobreescribiría en silencio si se construyera demasiado pronto.

**Opciones:**  
1. Usar `String` planos en todo el proyecto y listas de asociación en `Mundo`. Es sencillo de escribir, pero permite pasar por error el nombre de un objeto donde va una sala y obliga a hacer búsquedas lineales en cada comando.
2. Usar `newtype IdSala = IdSala String` y `newtype IdObjeto = IdObjeto String` junto con `Map` en `Mundo` (para salas, salidas por `Direccion` y objetos), manteniendo estructuras intermedias con listas (`MundoCrudo` y `SalaCruda` en `Motor.Mundo`) mientras se parsea antes de validar V2–V10.

**Decisión:**  
Elegimos la segunda opción: `IdSala` e `IdObjeto` como `newtype` y `Map` en `Motor.Tipos` (`mundoSalas :: Map IdSala Sala`, `salaSalidas :: Map Direccion Salida`, `mundoObjetos :: Map IdObjeto Objeto`), usando `MundoCrudo` y `SalaCruda` con listas en `Motor.Mundo` hasta pasar `validarMundo`.

**Consecuencias:**  
Con esto ganamos que el compilador impide confundir una sala con un objeto en las firmas (por ejemplo en `LlevarA IdObjeto IdSala`), las búsquedas en `Motor.Nucleo` son rápidas y `Map Direccion Salida` garantiza por tipo que no hay dos salidas en la misma dirección en un mundo ya validado, además de que `MundoCrudo` evita perder duplicados antes de comprobar V2 y V5. A cambio, perdimos algo de simplicidad por tener un paso intermedio extra al transformar `MundoCrudo` en `Mundo`, y siguen quedando invariantes que el tipo por sí solo no puede asegurar (como que el destino de una `Salida` exista en `mundoSalas` o que `objPeso` sea positivo), los cuales dependen de pasar por `leerMundo`. Si el proyecto creciera más, haríamos un constructor inteligente privado para `Mundo` de modo que ningún módulo externo pueda fabricar uno sin validar.

---

### D2. Reporte acumulado de errores frente a `Either` con `do`

**Contexto:**  
La función `leerMundo :: String -> Either [ErrorMundo] Mundo` debe detectar todos los errores del archivo (sintaxis V1 y validaciones V2–V10 más Peso) y devolverlos juntos, como los 9 errores de `mundos/invalido_semantica.txt`. Si se encadenan validaciones sobre `Either` con notación `do`, la mónada corta la ejecución en el primer `Left` y solo reporta el primer fallo.

**Opciones:**  
1. Encadenar validaciones con `Either` en un bloque `do`. Queda corto, pero se detiene en el primer error y no cumple el requisito de reportarlos todos juntos.
2. Acumular listas `[ErrorMundo]` con `foldl` en el parseo de cada bloque (`parsearCabecera`, `parsearBloqueObjeto`, `parsearBloqueSala`), juntar los fallos de todos los bloques en `parsearMundoCrudo`, y luego concatenar las listas de errores de cada regla en `validarMundo` (`errV2 ++ errV3 ++ ... ++ errPeso`), usando `EstadoCampo a` (`Ausente | PresenteInvalido | Presente a`) para no emitir un falso error de "campo faltante" cuando la clave sí estaba escrita pero mal formada.

**Decisión:**  
Elegimos la segunda opción en `Motor.Mundo`: separar la fase sintáctica V1 (acumulando errores por línea y por bloque) de la fase semántica V2–V10 + Peso (que corre si no hubo errores V1 y concatena todas las listas de `ErrorValidacion`).

**Consecuencias:**  
Ganamos que cualquier archivo roto devuelve todos sus errores en una sola ejecución (como verificamos en `test/Main.hs` con los 5, 7 y 5 errores de `mundos/invalidos/`), y distinguir `Ausente` de `PresenteInvalido` evita mensajes duplicados confusos cuando `META`, `INICIO` o `DESC` tienen errores de sintaxis. Perdimos un poco de concisión en `parsearCabecera` y en los bloques porque hay que ir pasando tuplas con la lista acumulada de errores a mano en lugar de usar un `do`. Para mejorarlo después, se podría definir un tipo aplicativo propio de validación para componer campos obligatorios sin repetir el `foldl` en cabecera, objeto y sala.

---

### D3. Diseño del reconocedor sobre `ReadS`, ambigüedad y número de línea

**Contexto:**  
El enunciado prohíbe `parsec`, `megaparsec`, `ReadP` y expresiones regulares, pidiendo combinadores propios sobre `ReadS` en `Motor.Reconocedor`. Como `ReadS a` es `String -> [(a, String)]`, combinadores como `muchos` o `identificador` producen varias lecturas parciales (por ejemplo `"plaza"` genera `("plaza","")`, `("plaz","a")`, etc.), y además los errores V1 exigen indicar el número exacto de línea del archivo original aunque se ignoren comentarios `#` y líneas en blanco.

**Opciones:**  
1. Envolver `ReadS` en un `newtype Parser a` con instancias `Functor`, `Applicative` y `Monad`, pasando el archivo entero y contando saltos de línea `\n` en el estado.
2. Usar `ReadS a` directamente con combinadores ligeros (`satisface`, `caracter`, `cadena`, `<|>`, `muchos`, `alMenosUno`) y listas por comprensión; resolver la ambigüedad en un único punto (`parsearTodo`) exigiendo consumir toda la entrada salvo espacios finales; y conservar el número de línea numerando primero con `zip [1..] (lines contenido)` en `limpiarLineas` (`LineaConNumero = (Int, String)`) antes de filtrar comentarios y líneas vacías.

**Decisión:**  
Elegimos la segunda opción: `ReadS` directo en `Motor.Reconocedor` con `parsearTodo` para eliminar ambigüedades, y `LineaConNumero` en `Motor.Mundo` para no perder nunca la línea original.

**Consecuencias:**  
Ganamos un reconocedor muy directo y fácil de depurar sin escribir instancias extra, `parsearTodo` descarta automáticamente las lecturas parciales como `("bibliotec", "a")` en `parsearSalida` al exigir `all isSpace r`, y cualquier error conserva exactamente la línea real del archivo aunque haya comentarios arriba. Perdimos la posibilidad de expresar la estructura completa de bloques (`---` y `CLAVE: valor`) como un único combinador `ReadS Mundo`, ya que esa coordinación quedó del lado de `Motor.Mundo`. Si tuviéramos que parsear bloques anidados o gramáticas más complejas, ahí sí convendría envolver `ReadS` en un `newtype` con `Applicative` y rastreo de posición `(linea, columna)`.
