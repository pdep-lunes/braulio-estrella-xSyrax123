data Personaje = UnPersonaje {
   nombre :: String,
   poderBasico :: String,
   superPoder :: String,
   tieneSuperPoderActivo :: Bool,
   cantidadDeVida :: Int
} deriving (Show)

espina :: Personaje 
espina = UnPersonaje "Espina" "bola de espinas" "granada de espinas" True 4800 

pamela :: Personaje
pamela = UnPersonaje "Pamela" "lluvia de tuercas" "torreta curativa" False 9600

milDeDanio :: Personaje -> Int
milDeDanio unPersonaje = cantidadDeVida unPersonaje - 1000

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa unPersonaje = unPersonaje {cantidadDeVida = milDeDanio(unPersonaje)}

-----------------------------------------------------------------------------------------

curar800DeVida :: Personaje -> Int
curar800DeVida unPersonaje = cantidadDeVida unPersonaje + 800

mitadDeVida :: Personaje -> Int
mitadDeVida unPersonaje = (cantidadDeVida unPersonaje) `div` 2

lluviaDeTuercas :: String -> Personaje -> Personaje -> Personaje
lluviaDeTuercas efectoDeTuercas atacante enemigo 
  | efectoDeTuercas == "sanadoras" = atacante {cantidadDeVida = curar800DeVida atacante} 
  | efectoDeTuercas == "dañinas" = enemigo {cantidadDeVida = mitadDeVida enemigo}
  | otherwise = enemigo

-----------------------------------------------------------------------------------------

nuevoNombre :: Personaje -> String
nuevoNombre unPersonaje = nombre unPersonaje ++ "Espina estuvo aqui"

granadaDeEspinas :: Int -> Personaje -> Personaje
granadaDeEspinas radioDeExplosion enemigo
  | radioDeExplosion > 3 && cantidadDeVida enemigo < 800 = enemigo {nombre = nuevoNombre enemigo, tieneSuperPoderActivo = False, cantidadDeVida = 0}
  | radioDeExplosion > 3 = enemigo {nombre = nuevoNombre enemigo}
  | otherwise = bolaEspinosa(enemigo)  

-----------------------------------------------------------------------------------------


