module Lib () where

import Text.Show.Functions ()
restarMilDeVida :: Personaje -> Int
restarMilDeVida unPersonaje = cantidadDeVida unPersonaje - 1000

data Personaje = UnPersonaje {
   nombre :: String,
   poderBasico :: String,
   superPoder :: String,
   tieneSuperPoderActivo :: Bool,
   cantidadDeVida :: Int,
}

espina :: Personaje 
espina = UnPersonaje "Espina" "bola de espinas" "granada de espinas" True 4800 

pamela :: Personaje
pamela = UnPersonaje "Pamela" "lluvia de tuercas" "torreta curativa" False 9600 

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa unPersonaje = UnPersonaje {cantidadDeVida = restarMilDeVida(unPersonaje)}
