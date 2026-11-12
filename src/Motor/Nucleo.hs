-- | Núcleo del motor de aventuras y lógica de la Variante 1: Peso

module Motor.Nucleo
  ( estadoInicial
  , ejecutar
  , cumpleMeta
  , describirSalaActual
  ) where

import Data.List (delete, intercalate)
import qualified Data.Map.Strict as Map
import Motor.Tipos

-- | Construye el estado inicial a partir de un 'Mundo' validado.
estadoInicial :: Mundo -> Estado
estadoInicial mundo =
  let st = Estado
        { estMundo      = mundo
        , estSalaActual = mundoInicio mundo
        , estInventario = []
        , estSalasObjs  = fmap salaObjetos (mundoSalas mundo)
        , estTerminado  = False
        , estSalir      = False
        }
  in st { estTerminado = cumpleMeta st }

-- | Determina si el estado actual satisface la meta del mundo.
cumpleMeta :: Estado -> Bool
cumpleMeta st = case mundoMeta (estMundo st) of
  LlegarA salaMeta ->
    estSalaActual st == salaMeta
  LlevarA objMeta salaMeta ->
    estSalaActual st == salaMeta && objMeta `elem` estInventario st

-- | Genera la descripción de la sala actual.
describirSalaActual :: Estado -> [String]
describirSalaActual st =
  let m = estMundo st
      sid = estSalaActual st
  in case Map.lookup sid (mundoSalas m) of
       Nothing ->
         ["Error interno: la sala '" ++ unIdSala sid ++ "' no existe en el mundo."]
       Just sala ->
         let objs = Map.findWithDefault [] sid (estSalasObjs st)
             salidas = Map.toList (salaSalidas sala)
             lineaObjs =
               if null objs
                 then []
                 else ["Objetos aquí: " ++ intercalate ", " (map (mostrarObjConPeso m) objs)]
             lineaSalidas =
               if null salidas
                 then ["Salidas: ninguna."]
                 else ["Salidas: " ++ intercalate ", " (map mostrarSalida salidas)]
         in [salaDesc sala] ++ lineaObjs ++ lineaSalidas
  where
    mostrarObjConPeso m oid =
      unIdObjeto oid ++ " (peso " ++ show (pesoObjeto m oid) ++ ")"
    mostrarSalida (dir, sal) =
      mostrarDireccion dir ++
        maybe "" (\req -> " (requiere " ++ unIdObjeto req ++ ")") (salRequisito sal)

-- | Ejecuta un comando de manera pura, devolviendo los mensajes para el
-- jugador y el nuevo estado.
ejecutar :: Comando -> Estado -> ([String], Estado)
ejecutar cmd st = case cmd of
  Ir dir       -> ejecutarIr dir st
  Mirar        -> (describirSalaActual st, st)
  Tomar oid    -> ejecutarTomar oid st
  Soltar oid   -> ejecutarSoltar oid st
  Examinar oid -> ejecutarExaminar oid st
  Inventario   -> (mostrarInventario st, st)
  Ayuda        -> (textoAyuda, st)
  Salir        -> (["¡Hasta luego!"], st { estSalir = True })
  Guardar _    -> (["El comando 'guardar' requiere E/S desde el bucle principal."], st)
  Cargar _     -> (["El comando 'cargar' requiere E/S desde el bucle principal."], st)

ejecutarIr :: Direccion -> Estado -> ([String], Estado)
ejecutarIr dir st =
  let m = estMundo st
      sid = estSalaActual st
  in case Map.lookup sid (mundoSalas m) of
       Nothing ->
         (["Error interno: sala actual inválida."], st)
       Just sala ->
         case Map.lookup dir (salaSalidas sala) of
           Nothing ->
             (["No hay salida hacia " ++ mostrarDireccion dir ++ "."], st)
           Just salida ->
             case salRequisito salida of
               Just req | req `notElem` estInventario st ->
                 (["La salida hacia " ++ mostrarDireccion dir
                   ++ " está bloqueada. Necesitas: " ++ unIdObjeto req ++ "."], st)
               _ ->
                 let stMov = st { estSalaActual = salDestino salida }
                     gano = cumpleMeta stMov
                     stFinal = stMov { estTerminado = gano }
                     desc = describirSalaActual stFinal
                     msgs = if gano then desc ++ ["*** ¡Has ganado! ***"] else desc
                 in (msgs, stFinal)

ejecutarTomar :: IdObjeto -> Estado -> ([String], Estado)
ejecutarTomar oid st =
  let m = estMundo st
      sid = estSalaActual st
      objsSala = Map.findWithDefault [] sid (estSalasObjs st)
  in if oid `notElem` objsSala
       then if oid `elem` estInventario st
              then (["Ya llevas " ++ unIdObjeto oid ++ " en tu inventario."], st)
              else (["No ves '" ++ unIdObjeto oid ++ "' en esta sala."], st)
       else
         let pObj = pesoObjeto m oid
             pAct = pesoInventario m (estInventario st)
             pNuevo = pAct + pObj
         in case mundoCapacidad m of
              Just cap | pNuevo > cap ->
                (["No puedes cargar " ++ unIdObjeto oid
                  ++ " (peso " ++ show pObj ++ "). Excederías tu capacidad ("
                  ++ show pNuevo ++ "/" ++ show cap ++ ")."], st)
              capMaybe ->
                let invNuevo = estInventario st ++ [oid]
                    salasNuevas = Map.insert sid (delete oid objsSala) (estSalasObjs st)
                    stTom = st { estInventario = invNuevo, estSalasObjs = salasNuevas }
                    gano = cumpleMeta stTom
                    stFinal = stTom { estTerminado = gano }
                    msgCarga = formatearCarga pNuevo capMaybe
                    msgBase = "Tomas " ++ unIdObjeto oid
                              ++ " (peso " ++ show pObj ++ "). " ++ msgCarga
                    msgs = if gano then [msgBase, "*** ¡Has ganado! ***"] else [msgBase]
                in (msgs, stFinal)

ejecutarSoltar :: IdObjeto -> Estado -> ([String], Estado)
ejecutarSoltar oid st =
  let m = estMundo st
      sid = estSalaActual st
      inv = estInventario st
  in if oid `notElem` inv
       then (["No llevas '" ++ unIdObjeto oid ++ "' en tu inventario."], st)
       else
         let invNuevo = delete oid inv
             objsSala = Map.findWithDefault [] sid (estSalasObjs st)
             salasNuevas = Map.insert sid (objsSala ++ [oid]) (estSalasObjs st)
             stFinal = st { estInventario = invNuevo, estSalasObjs = salasNuevas }
             pNuevo = pesoInventario m invNuevo
             msgCarga = formatearCarga pNuevo (mundoCapacidad m)
         in (["Sueltas " ++ unIdObjeto oid ++ ". " ++ msgCarga], stFinal)

ejecutarExaminar :: IdObjeto -> Estado -> ([String], Estado)
ejecutarExaminar oid st =
  let m = estMundo st
      sid = estSalaActual st
      objsSala = Map.findWithDefault [] sid (estSalasObjs st)
      presente = oid `elem` estInventario st || oid `elem` objsSala
  in if not presente
       then (["No ves '" ++ unIdObjeto oid ++ "' aquí ni en tu inventario."], st)
       else case Map.lookup oid (mundoObjetos m) of
              Nothing ->
                (["No hay información sobre '" ++ unIdObjeto oid ++ "'."], st)
              Just obj ->
                ([objDesc obj ++ " (peso: " ++ show (objPeso obj) ++ ")"], st)

mostrarInventario :: Estado -> [String]
mostrarInventario st =
  let m = estMundo st
      inv = estInventario st
      pTotal = pesoInventario m inv
      encabezado = case mundoCapacidad m of
        Just cap -> "Inventario (carga: " ++ show pTotal ++ "/" ++ show cap ++ "):"
        Nothing  -> "Inventario (peso total: " ++ show pTotal ++ "):"
  in if null inv
       then [encabezado, "  (vacío)"]
       else encabezado : map (\oid -> "  - " ++ unIdObjeto oid
                                      ++ " (peso " ++ show (pesoObjeto m oid) ++ ")") inv

formatearCarga :: Int -> Maybe Int -> String
formatearCarga peso (Just cap) = "Carga actual: " ++ show peso ++ "/" ++ show cap ++ "."
formatearCarga peso Nothing    = "Peso total actual: " ++ show peso ++ "."

textoAyuda :: [String]
textoAyuda =
  [ "Comandos disponibles:"
  , "  ir <dirección>   - Moverse (norte, sur, este, oeste, arriba, abajo)"
  , "  <dirección>      - Atajo directo (norte/n, sur/s, este/e, oeste/o, arriba, abajo)"
  , "  mirar            - Describir la sala actual, sus objetos y salidas"
  , "  tomar <objeto>   - Recoger un objeto (sinónimo: coger)"
  , "  soltar <objeto>  - Dejar un objeto en la sala actual"
  , "  examinar <objeto>- Ver la descripción y peso de un objeto"
  , "  inventario       - Listar tus objetos y peso cargado (sinónimo: inv)"
  , "  guardar <archivo>- Guardar la partida actual"
  , "  cargar <archivo> - Cargar una partida guardada"
  , "  ayuda            - Mostrar esta ayuda"
  , "  salir            - Terminar el juego"
  ]
