module Lib () where

import Text.Show.Functions ()

type Poder = Personaje -> Personaje


data Personaje = UnPersonaje {
    nombre :: String,
    poder :: [Poder],
    superPoder :: Poder,
    superPoderActivo :: Bool,
    vida :: Int
    } deriving Show


espina :: Personaje
espina = UnPersonaje{
        nombre = "Spike",
        poder = [bolaEspinosa],
        superPoder = granadaDeEspinas(5),
        superPoderActivo = True,
        vida = 4800
    }

pamela :: Personaje
pamela = UnPersonaje{
    nombre = "Pam",
    poder = [lluviaTuercasSanadoras,lluviaTuercasDañinas],
    superPoder = torretaCurativa,
    superPoderActivo = False,
    vida = 9600
}

-- FUNCIONES AUXILIARES
cambiarVida :: (Int -> Int) -> Personaje -> Personaje
cambiarVida fn unPersonaje = unPersonaje {vida = ((fn.vida) unPersonaje)}

agregarANombre :: (String -> String) -> Personaje -> Personaje
agregarANombre fn unPersonaje = unPersonaje {nombre = nombre ++ fn}

cambiarSuper :: (Bool -> Bool) -> Personaje -> Personaje
cambiarSuper fn unPersonaje = unPersonaje {superPoderActivo = fn}

--PODERES Y SUPERPODERES

--bolaEspinosa: le quita 1000 puntos de vida a quien sea su contrincante (¡no debe quedar un número negativo!).

bolaEspinosa :: Poder
bolaEspinosa unPersonaje = (cambiarVida(subtract 1000)) unPersonaje

{-
pueden ser sanadoras o dañinas. Las primeras le suman 800 puntos de vida a su colega y 
las segundas le disminuyen a la mitad la vida de quien sea su contrincante. 
En cualquier otro caso, no le pasa nada al personaje.
-}

lluviaTuercasDañinas :: Poder
lluviaTuercasDañinas unPersonaje  = (cambiarVida(subtract (div (vida unPersonaje) 2)) unPersonaje)

lluviaTuercasSanadoras :: Poder
lluviaTuercasSanadoras unPersonaje = (cambiarVida(+800) unPersonaje)

{-granadaDeEspinas: el daño va a depender del radio de explosión de la misma. 
Si es mayor a 3, le agregara a su nombre “Espina estuvo aquí”. 
Si además su contrincante tiene menos de 800 vida, desactiva su súper y lo deja con 0 de vida. 
En otro caso, se usa una bola de espinas.
-}

granadaDeEspinas :: (Int -> Int) -> Poder
granadaDeEspinas RadioExplosion unPersonaje 
    |RadioExplosion > 3 && vida UnPersonaje < 800 = (agregarANombre("Espina estuvo aqui").cambiarSuper(False).cambiarVida(subtract (vida unPersonaje))) unPersonaje
    |otherwise = bolaEspinosa unPersonaje
{-
torretaCurativa: le activa el súper a su aliado y lo deja con el doble de su salud inicial.
-}

torretaCurativa :: Poder
torretaCurativa unPersonaje = (cambiarSuper(True).cambiarVida(*2)) unPersonaje

--REPORTES

{-atacar con el poder especial: si el personaje tiene el súper poder activo, 
entonces va a atacar a su contrincante con el súper y con el básico. Si no, no hará nada.-}

atacarConPoderEspecial :: Personaje -> Poder
atacarConPoderEspecial unContrincante unPersonaje
    |superPoderActivo unPersonaje = ((superPoder unPersonaje).(poder unPersonaje) unContrincante)
    |otherwise = unContrincante

{-
saber quiénes están en las últimas: 
es decir, el nombre de aquellos brawlers que tienen menos de 800 puntos de vida
-}

personajesQueEstanEnLasUltimas :: Personaje -> Bool
personajesQueEstanEnLasUltimas unPersonaje = (vida unPersonaje) < 800

estanEnLasUltimas :: [Personaje] -> [String]
estanEnLasUltimas personajes = nombresDePersonajesQueEstanLasUltimas personajes

nombresDePersonajesQueEstanLasUltimas :: [Personajes] -> [String]
nombresDePersonajesQueEstanLasUltimas personajes = (map (nombre) . filter (personajesQueEstanEnLasUltimas)) Personajes