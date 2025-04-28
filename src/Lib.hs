module Lib () where

import Text.Show.Functions ()

data Personaje = UnPersonaje {
    nombre :: String,
    poderBasico :: String, 
    superPoder :: String,
    superPoderActivo :: Bool,
    cantidadVida :: Int
    } deriving Show


espina :: Personaje
espina = UnPersonaje{
        nombre = "Espina",
        poderBasico = "Bola de Espinas",
        superPoder = "Granada de Espinas",
        superPoderActivo = True,
        cantidadVida = 4800

    }

pamela :: Personaje
pamela = UnPersonaje{
    nombre = "Pamela",
    poderBasico = "Lluevia de tuercas sanadoras",
    superPoder = "Torreta Curativa",
    superPoderActivo = False,
    cantidadVida = 9600
}

 