data Personaje = UnPersonaje {
   nombre :: String,
   poderBasico :: String,
   superPoder :: String,
   tieneSuperPoderActivo :: Bool,
   cantidadDeVida :: Int
} deriving (Show)

espina :: Personaje 
espina = UnPersonaje "Espina" "Bola de espinas" "Granada de espinas" True 4800 

pamela :: Personaje
pamela = UnPersonaje "Pamela" "Lluvia de tuercas" "Torreta curativa" False 9600

personajes :: [Personaje]
personajes = [espina, pamela]

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
lluviaDeTuercas efectoDeTuercas enemigo atacante
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

curarDobleDeVida :: Personaje -> Int
curarDobleDeVida unPersonaje = cantidadDeVida unPersonaje * 2

torretaCurativa :: Personaje -> Personaje
torretaCurativa aliado = aliado {cantidadDeVida = curarDobleDeVida(aliado), tieneSuperPoderActivo = True}

-----------------------------------------------------------------------------------------

atacarConElPoderEspecial :: Personaje -> Personaje -> Personaje
atacarConElPoderEspecial atacante enemigo 
  | tieneSuperPoderActivo atacante == True && nombre atacante == "Espina" = (bolaEspinosa . granadaDeEspinas 4) enemigo
  | tieneSuperPoderActivo atacante == True && nombre atacante == "Pamela" = (lluviaDeTuercas "dañinas" enemigo . torretaCurativa) atacante
  | otherwise = enemigo

estanEnLasUltimas :: [Personaje] -> [String]
estanEnLasUltimas unosPersonajes = (map nombre . filter tienePocaVida) unosPersonajes

tienePocaVida :: Personaje -> Bool
tienePocaVida personaje = cantidadDeVida personaje < 800
