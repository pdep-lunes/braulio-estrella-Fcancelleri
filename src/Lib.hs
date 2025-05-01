module Lib () where

import Text.Show.Functions ()

data Personaje = UnPersonaje {
    nombre :: String,
    poderBasico :: String, 
    superPoder :: String,
    superPoderActivo :: Bool,
    vida :: Int
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

modificarVida :: Int -> Personaje -> Personaje --recibe la nueva vida del personaje y el personaje para poder modificar. Devuelve el personaje con su vida modificada--
modificarVida nuevaVida (UnPersonaje nombre poderBasico superPoder superPoderActivo _) = UnPersonaje nombre poderBasico superPoder superPoderActivo nuevaVida

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa personaje = modificarVida (vida personaje - 1000) personaje

