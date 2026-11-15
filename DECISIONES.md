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

---

## Nadheska García (19-10054)

### D4. Qué guarda `Estado`, si se copia el mundo y dónde quedan los objetos soltados

**Contexto:**  
La firma obligatoria `ejecutar :: Comando -> Estado -> ([String], Estado)` en `Motor.Nucleo` recibe únicamente el comando y el estado actual, sin recibir `Mundo` por separado. Sin embargo, para ejecutar `ir`, `tomar`, `examinar` o `mirar`, el núcleo necesita consultar las descripciones, salidas, pesos, capacidad y la meta del mundo, mientras que la posición de los objetos cambia cuando el jugador los recoge o los suelta.

**Opciones:**  
1. Modificar directamente el campo `salaObjetos` dentro de `mundoSalas` en `estMundo` cada vez que el jugador toma o suelta un objeto.
2. Mantener `estMundo :: Mundo` como referencia inmutable de solo lectura en `Estado` y separar lo que cambia durante la partida en `estSalaActual :: IdSala`, `estInventario :: [IdObjeto]`, `estSalasObjs :: Map IdSala [IdObjeto]`, `estTerminado :: Bool` y `estSalir :: Bool`.

**Decisión:**  
Elegimos la segunda opción. En `estadoInicial`, `estSalasObjs` se inicializa con `fmap salaObjetos (mundoSalas mundo)` y `estMundo` no se toca nunca durante la partida. Cuando el jugador ejecuta `soltar oid`, `ejecutarSoltar` quita `oid` de `estInventario` con `delete` y lo añade a la lista de `estSalasObjs` asociada a `estSalaActual`.

**Consecuencias:**  
Ganamos una separación limpia entre la definición estática del mundo y el estado dinámico de la partida, lo cual simplifica también guardar y cargar partidas en `PartidaGuardada` sin volcar todo `Mundo` al archivo. Los objetos soltados quedan en la sala donde el jugador los dejó y aparecen al volver a ella o ejecutar `mirar`. A cambio, existe una pequeña duplicación inicial entre las listas de objetos de `mundoSalas` y `estSalasObjs`, aunque su costo en memoria es mínimo.

---

### D5. Decisiones de la Variante 1 (Peso): exceso de capacidad y compatibilidad sin `CAPACIDAD`

**Contexto:**  
En la Variante 1 (Peso), la cabecera puede declarar `CAPACIDAD: n` y cada objeto `PESO: n` (1 por defecto). Había que decidir qué pasa cuando el jugador intenta tomar un objeto que supera la capacidad disponible, cómo informarle la carga actual y cómo mantener compatibilidad con archivos que no declaran `CAPACIDAD:` como `mundos/entrega.txt`.

**Opciones:**  
1. Imponer una capacidad fija por defecto cuando `CAPACIDAD:` no aparece en la cabecera, o bien representarla como `mundoCapacidad :: Maybe Int` donde `Nothing` significa capacidad ilimitada.
2. Al superar la capacidad, impedir recoger el objeto dejándolo en la sala, frente a permitir recogerlo pero bloquear el movimiento hasta soltar peso.

**Decisión:**  
Representamos la capacidad como `mundoCapacidad :: Maybe Int` en `Motor.Tipos`: `Nothing` desactiva el límite de peso (garantizando compatibilidad con `mundos/entrega.txt`) y `Just cap` aplica el límite. En `ejecutarTomar` (`Motor.Nucleo`), si `pesoActual + pesoObj > cap`, el comando falla con un mensaje que muestra el peso del objeto y la relación `nuevoPeso/cap`, dejando el objeto en la sala y el inventario intacto. Además, `mirar`, `tomar`, `soltar`, `examinar` e `inventario` muestran el peso de cada objeto y la carga actual.

**Consecuencias:**  
Ganamos compatibilidad inmediata con mundos con y sin `CAPACIDAD:` sin inventar constantes arbitrarias, y el jugador tiene siempre visible la información de peso para resolver acertijos como el de `mundos/mundo.txt` (donde debe soltar `llave_maestra` de peso 2 en la bóveda para poder cargar `nucleo_cuantico` de peso 4 junto con `tarjeta_acceso` de peso 1). Como contrapartida, si un autor define `PESO:` en los objetos pero olvida poner `CAPACIDAD:` en la cabecera, no habrá restricción de carga en tiempo de juego.

---

### D6. Formas de comandos, formato de guardado y manejo de entrada redirigida / EOF

**Contexto:**  
Para completar la experiencia de juego había que definir tres aspectos prácticos: qué formas acepta `leerComando` en `Motor.Comandos`, qué información guarda `Motor.Persistencia` en disco y cómo lee `app/Main.hs` los comandos para funcionar tanto en consola interactiva como con entrada redirigida (`< guion.txt`).

**Opciones:**  
1. En `leerComando`, exigir solo `ir <dirección>` e identificadores exactos con guion bajo, frente a aceptar atajos directos (`norte`, `n`, `sur`, `s`, `este`, `e`, `oeste`, `o`, `arriba`, `abajo`), sinónimos (`coger`, `inv`), mayúsculas y artículos opcionales (`el`, `la`, `los`, `las`, `un`, `una`).
2. En `Motor.Persistencia`, usar `Show`/`Read` sobre todo `Estado`, frente a definir `PartidaGuardada` con solo los datos dinámicos y el título del mundo.
3. En `app/Main.hs`, leer toda la entrada con `getContents` y `lines`, frente a un bucle línea a línea con `hFlush stdout`, `isEOF` y `getLine`.

**Decisión:**  
En `Motor.Comandos` aceptamos atajos de dirección directa (necesarios para `guiones/entrega.txt`), sinónimos y eliminación de artículos iniciales uniendo palabras con `_`, pero conservando intactas las mayúsculas en rutas de `guardar`/`cargar`. En `Motor.Persistencia` serializamos solo `PartidaGuardada` y usamos `readMaybe` validando que el título, las salas, los objetos y el peso coincidan con el mundo cargado. En `app/Main.hs` usamos `hFlush stdout`, `isEOF` y `getLine`.

**Consecuencias:**  
Ganamos una interfaz cómoda para el jugador, archivos de guardado pequeños que detectan si la partida es de otro mundo o está corrupta sin caer nunca, y un bucle principal que muestra el indicador `> ` a tiempo en modo interactivo y termina limpiamente con código `0` al llegar a EOF en `< guion.txt`. Perdimos la posibilidad de encadenar el bucle como una función pura `String -> String` con `interact`, ya que los comandos `guardar` y `cargar` requieren realizar efectos de E/S entre líneas.
