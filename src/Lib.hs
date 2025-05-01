module Lib () where

import Text.Show.Functions ()

data Personaje = UnPersonaje {
    nombre :: String,
    poderBasico :: String, 
    superPoder :: String,
    superPoderActivo :: Bool,
    vida :: Float
    } deriving (Show,Eq)


espina :: Personaje
espina = UnPersonaje{
        nombre = "Espina",
        poderBasico = "Bola de Espinas",
        superPoder = "Granada de Espinas",
        superPoderActivo = True,
        vida = 4800
    }

pamela :: Personaje
pamela = UnPersonaje{
    nombre = "Pamela",
    poderBasico = "Lluevia de tuercas sanadoras",
    superPoder = "Torreta Curativa",
    superPoderActivo = False,
    vida = 9600
}

modificarVida :: Float -> Personaje -> Personaje --recibe la nueva vida del personaje y el personaje para poder modificar. Devuelve el personaje con su vida modificada--
modificarVida nuevaVida (UnPersonaje nombre poderBasico superPoder superPoderActivo _) = UnPersonaje nombre poderBasico superPoder superPoderActivo nuevaVida

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa personaje = modificarVida (vida personaje - 1000) personaje

{-lluviaDeTuercas: pueden ser sanadoras o dañinas. 
Las primeras le suman 800 puntos de vida a su colega y 
las segundas le disminuyen a la mitad la vida de quien sea su contrincante. 
En cualquier otro caso, no le pasa nada al personaje.
-}

lluviaDeTuercas :: Personaje -> Bool -> Personaje
lluviaDeTuercas personaje esColega
    |esColega == True = modificarVida (vida personaje + 800) personaje
    |esColega == False = modificarVida ((vida personaje) /2) personaje

{-lluviaDeTuercasDañinas :: Personaje -> Personaje
lluviaDeTuercasDañinas personaje = modificarVida ((vida personaje) /2) personaje

lluviaDeTuercasSanadoras :: Personaje -> Personaje
lluviaDeTuercasSanadoras personaje = modificarVida (vida personaje + 800) personaje-}

{-granadaDeEspinas: el daño va a depender del radio de explosión de la misma. 
Si es mayor a 3, le agregara a su nombre “Espina estuvo aquí”. 
Si además su contrincante tiene menos de 800 vida, desactiva su súper y lo deja con 0 de vida. 
En otro caso, se usa una bola de espinas.
-}
modificarSuperYVida :: Float -> Bool -> Personaje -> Personaje
modificarSuperYVida nuevaVida ulti (UnPersonaje nombre poderBasico superPoder _ _) = UnPersonaje nombre poderBasico superPoder ulti nuevaVida

modificarNombre :: Personaje -> Personaje
modificarNombre (UnPersonaje nombre poderBasico superPoder superPoderActivo vida) = UnPersonaje (nombre ++ ", Espina estuvo aqui") poderBasico superPoder superPoderActivo vida

granadaDeEspinas :: Float -> Personaje -> Personaje
granadaDeEspinas radioExplosion personaje
    | radioExplosion > 3 = modificarNombre personaje
    | vida personaje < 800 = (modificarSuperYVida 0 False personaje)
    | otherwise = bolaEspinosa personaje


