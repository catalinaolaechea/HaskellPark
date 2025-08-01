
data Atraccion = Atraccion{
    nombre :: String,
    alturaMinimaRequerida :: Float,
    duracion :: Int,
    opiniones :: [String],
    mantenimiento :: Bool,
    reparaciones :: [Reparacion]
}

type Reparacion = (Int, Trabajo)
type Trabajo = Atraccion -> Atraccion 

trabajoPendiente :: Reparacion -> Trabajo
trabajoPendiente = snd

tiempoReparacion :: Reparacion -> Int 
tiempoReparacion = fst

mapDuracion :: (Int -> Int) -> Atraccion -> Atraccion
mapDuracion funcion atraccion = atraccion{
    duracion = funcion.duracion $ atraccion
}

mapReparaciones :: ([Reparacion] -> [Reparacion]) -> Atraccion -> Atraccion
mapReparaciones funcion atraccion = atraccion{
    reparaciones = funcion.reparaciones $ atraccion
}

mapMantenimiento :: (Bool -> Bool) -> Atraccion -> Atraccion
mapMantenimiento funcion atraccion = atraccion{
    mantenimiento = funcion.mantenimiento $ atraccion
}

mapAlturaMinimaRequerida :: (Float -> Float) -> Atraccion -> Atraccion
mapAlturaMinimaRequerida funcion atraccion = atraccion{
    alturaMinimaRequerida = funcion.alturaMinimaRequerida $ atraccion
}

mapOpiniones :: ([String] -> [String]) -> Atraccion -> Atraccion
mapOpiniones funcion atraccion = atraccion{
    opiniones = funcion.opiniones $ atraccion
}

finalizarTrabajo :: Trabajo 
finalizarTrabajo unaAtraccion 
    |(not . null .reparaciones)unaAtraccion = mapReparaciones sacarUltimo unaAtraccion
    |otherwise = mapMantenimiento(const False).mapReparaciones sacarUltimo $ unaAtraccion

sacarUltimo :: [a] -> [a]
sacarUltimo [] = []
sacarUltimo [_] = []
sacarUltimo (x:xs) = x : sacarUltimo xs

ajustarTornillos :: Int -> Trabajo 
ajustarTornillos tornillos = finalizarTrabajo.mapDuracion(min 10.(+ tornillos))

engrasar :: Int -> Trabajo
engrasar gramosDeGrasa = finalizarTrabajo.mapAlturaMinimaRequerida(+(fromIntegral gramosDeGrasa * 0.1)).mapOpiniones("para valientes" :)

mantenimientoelectronico :: Trabajo 
mantenimientoelectronico = finalizarTrabajo.mapOpiniones (take 2)

mantenimientoBasico :: Trabajo
mantenimientoBasico = engrasar 10.ajustarTornillos 8

meDaMiedito :: Atraccion -> Bool
meDaMiedito atraccion= any(\(tiempo,_)-> tiempo > 4)(reparaciones atraccion)

acaCerramos :: Atraccion -> Bool
acaCerramos = (>7).sum.map tiempoReparacion.reparaciones 

type Parque = [Atraccion]

disneyNoEsistis :: Parque -> Bool
disneyNoEsistis  = all(not.null.reparaciones).filter((>5).length.nombre)

queTanBuenaEs :: Atraccion -> Int
queTanBuenaEs unaAtraccion 
    |((> 10).duracion) unaAtraccion = 100
    |((< 3).length.reparaciones) unaAtraccion = ((* 10).length.nombre)unaAtraccion + ((* 2).length.opiniones) unaAtraccion
    |otherwise = ((* 10).ceiling.alturaMinimaRequerida)unaAtraccion 

listaDeTrabajosPendiente :: Atraccion -> [Trabajo]
listaDeTrabajosPendiente = map trabajoPendiente.reparaciones  

tieneReparacionesPeolas :: Atraccion -> Bool
tieneReparacionesPeolas atraccion = evaluar (listaDeTrabajosPendiente atraccion) atraccion

evaluar :: [Trabajo] -> Atraccion -> Bool
evaluar [] _ = False
evaluar [unTrabajo] atraccion =  True
evaluar (unTrabajo : otroTrabajo : pendientes) atraccion 
    |queTanBuenaEs (unTrabajo atraccion) < queTanBuenaEs (otroTrabajo atraccion) = evaluar (otroTrabajo : pendientes) (unTrabajo atraccion)
    |otherwise = False

mannyALaObra :: Atraccion -> Atraccion
mannyALaObra atraccion = mapReparaciones(const []).realizarTrabajosPendientes (listaDeTrabajosPendiente atraccion) $ atraccion

realizarTrabajosPendientes :: [Trabajo] -> Atraccion -> Atraccion
realizarTrabajosPendientes trabajos atraccion = foldl (\atraccion trabajo -> trabajo atraccion) atraccion trabajos

-- parte teorica:
--si la lista de trabajos fuera infinita en realizarTrabajosPendientes no se obtendría un resultado por consola, pues se tildaría evaluando la atracción por cada trabajo pendiente.
--idem para tieneReparacionesPeolas pero en este caso continuaría la recursividad de manera infinita
