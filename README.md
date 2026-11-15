# Proyecto 1 — Motor de Aventuras de Texto en Haskell (CI-3661)

## Integrantes
- **Samuel Sanabria** — Carnet: `20-10591`
- **Nadheska García** — Carnet: `19-10054`

## Variante Asignada — Variante 1: Peso
Cálculo según la Sección 7 del enunciado (suma de carnets sin guion módulo 4):
- `2010591 + 1910054 = 3920645`
- `3920645 mod 4 = 1` → **Variante 1: Peso**

La cabecera puede definir `CAPACIDAD: n` y cada objeto puede definir `PESO: n` (enteros positivos `>= 1`; si no se especifica peso, vale `1`). El jugador no puede cargar más peso que `CAPACIDAD`. Si el mundo no especifica `CAPACIDAD:`, se permite cargar objetos sin límite de peso para mantener compatibilidad con mundos estándar como `mundos/entrega.txt`.

## Estructura del Proyecto
- `src/Motor/Tipos.hs`: tipos del dominio (`Mundo`, `Sala`, `Objeto`, `Salida`, `Direccion`, `Meta`, `ErrorMundo`, `Comando`, `Estado`, `PartidaGuardada`).
- `src/Motor/Reconocedor.hs`: combinadores propios de reconocimiento construidos sobre `ReadS`.
- `src/Motor/Mundo.hs`: función pura `leerMundo :: String -> Either [ErrorMundo] Mundo` y validaciones V1–V10 más Peso.
- `src/Motor/Comandos.hs`: función pura `leerComando :: String -> Maybe Comando`.
- `src/Motor/Nucleo.hs`: funciones puras `estadoInicial :: Mundo -> Estado` y `ejecutar :: Comando -> Estado -> ([String], Estado)`.
- `src/Motor/Persistencia.hs`: lectura de archivos de mundo y guardado/carga segura de partidas.
- `app/Main.hs`: ejecutable `aventura` (modo interactivo, entrada redirigida y bandera `-validar` / `--validar`).
- `mundos/` y `guiones/`:
  - `mundos/mundo.txt` y `guiones/mundo.txt`: mundo propio y su guion ganador.
  - `mundos/entrega.txt` y `guiones/entrega.txt`: mundo y guion de ejemplo del material.
  - `mundos/invalidos/`: mundos inválidos de prueba (`sintaxis_invalida.txt`, `referencias_invalidas.txt`, `peso_y_alcance.txt`).
- `test/Main.hs`: suite de pruebas automáticas (23 casos).
- `DECISIONES.md` e `IA.md`: registros de decisiones de diseño (ADRs D1–D6) y declaración de uso de IA.

## Cómo Compilar, Correr y Probar

### Con `cabal` (o `stack`)
```bash
cabal build
cabal test
cabal run aventura -- -validar mundos/mundo.txt
cabal run aventura -- mundos/entrega.txt < guiones/entrega.txt
cabal run aventura -- mundos/mundo.txt < guiones/mundo.txt
```

### Con `Setup.hs` / `ghc` directamente (si no está instalado `cabal-install`)
```bash
runghc Setup.hs configure --enable-tests
runghc Setup.hs build
runghc Setup.hs test
./dist/build/aventura/aventura -validar mundos/mundo.txt
./dist/build/aventura/aventura mundos/entrega.txt < guiones/entrega.txt
./dist/build/aventura/aventura mundos/mundo.txt < guiones/mundo.txt
```
