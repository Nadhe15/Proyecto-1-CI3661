# Declaración de Uso de IA

## Parte 1 — Samuel Sanabria (20-10591)

**Herramientas utilizadas y propósito:**  
Se utilizó un asistente de IA integrado en el editor (Antigravity) como apoyo para estructurar los combinadores sobre `ReadS` en `Motor.Reconocedor`, organizar las validaciones V1–V10 y de la variante Peso en `Motor.Mundo`, redactar casos de prueba en `test/Main.hs` y revisar la redacción de `DECISIONES.md`.

**Ejemplos concretos de sugerencias corregidas:**  
1. **Campos obligatorios duplicados (`Maybe` frente a `EstadoCampo`):** En la primera versión de `parsearCabecera`, `parsearBloqueObjeto` y `parsearBloqueSala` en `src/Motor/Mundo.hs`, la sugerencia inicial modelaba los campos obligatorios (`MUNDO`, `INICIO`, `META` y `DESC`) como `Maybe a`. Al probar con archivos inválidos (por ejemplo `META: llegar cuarto`), vimos que cuando una clave sí estaba presente pero con formato incorrecto, el acumulador quedaba en `Nothing` y al cerrar el bloque el motor emitía un segundo error falso indicando que el campo faltaba. Corregimos ese diseño sustituyendo `Maybe a` por `EstadoCampo a` (`Ausente | PresenteInvalido | Presente a`) para distinguir cuándo una clave realmente no existe y cuándo existe pero está mal escrita, y agregamos su prueba en `test/Main.hs` (commit `7cd880f`).
2. **Espacios obligatorios en `parsearMeta` y `parsearSalida`:** En una revisión de casos borde detectamos que usar el combinador `espacios` (cero o más espacios) entre palabras clave como `llegar` y `a` permitía aceptar entradas mal formadas como `llegarasala` o `llegar asala`. Se corrigió exigiendo `alMenosUno (satisface isSpace)` entre palabras clave y se añadió la prueba correspondiente en `test/Main.hs`.

---

## Parte 2 — Nadheska García (19-10054)

**Herramientas utilizadas y propósito:**  
Se utilizó el asistente de IA en el editor (Antigravity) como apoyo para definir los tipos de estado y comandos en `Motor.Tipos`, implementar el parser de comandos en `Motor.Comandos`, la lógica pura y restricción de peso en `Motor.Nucleo`, la persistencia segura en `Motor.Persistencia`, el bucle CLI en `app/Main.hs`, el mundo propio `mundos/mundo.txt` con sus pruebas y la redacción de los ADRs D4–D6.

**Ejemplos concretos de sugerencias rechazadas o corregidas:**  
1. **Actualización de objetos en la sala al ejecutar `Tomar`:** En una versión preliminar de `ejecutarTomar` en `src/Motor/Nucleo.hs`, al recoger un objeto se añadía a `estInventario` pero se omitía eliminarlo de la lista de objetos de la sala actual en `estSalasObjs`, permitiendo duplicar el objeto si se volvía a tomar. Se corrigió actualizando `estSalasObjs` con `Map.insert sid (delete oid objsSala) (estSalasObjs st)` (commit `5ec8304`).
2. **Serialización de `PartidaGuardada` con `readMaybe` frente a `Estado` completo con `read`:** Se descartó guardar el `Estado` entero (que incluiría todo el `Mundo`) y leerlo con `Prelude.read` parcial. En su lugar se creó `PartidaGuardada` con solo los datos dinámicos y se usó `Text.Read.readMaybe` junto con validaciones explícitas de título, salas, objetos y capacidad en `src/Motor/Persistencia.hs`, evitando que archivos corruptos o de otro mundo hagan caer el programa.
