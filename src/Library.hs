module Library where
import PdePreludat
import GHC.Float (plusDouble)

--Modelado del enunciado
type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Rima = Palabra -> Palabra -> Bool
type Patron = Estrofa -> Bool
type Conjugacion = Verso -> Verso -> Bool
type Potencia = Number
type EstadoPublico = String
type Puntaje = Number
type Jurado = Estrofa -> PuestaEscena -> Puntaje
type Artista = String --Solamente interesa el nombre

esVocal :: Char -> Bool
esVocal = flip elem "aeiou"

tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"

--Defino y modelo la Puesta Base
data PuestaEscena = UnaPuestaEscena{
    potencia :: Potencia,
    publico :: EstadoPublico
} deriving Show

puestaBase :: PuestaEscena
puestaBase = UnaPuestaEscena 1 "Tranquilo"

--Funciones para delegar
recorrerVerso :: Verso -> Number -> Palabra --Se usa en obtenerUltimaPalabra
recorrerVerso [] _= []
recorrerVerso _ 0 = []
recorrerVerso (x:xs) cont 
    |x/= ' ' = x:recorrerVerso xs cont
    |otherwise = recorrerVerso xs (cont - 1)

mismaLetra :: Rima --Se usa en rimanAsonante
mismaLetra palabraV1 palabraV2 = (last.init) palabraV1 == (last.init) palabraV2

sonLaMismaPalabra :: Rima
sonLaMismaPalabra palabraV1 palabraV2 = palabraV1 == palabraV2

mismasTresLetras :: Rima
mismasTresLetras palabraV1 palabraV2 = take 3 (reverse palabraV1) == take 3 (reverse palabraV2)

sonVocales :: Rima --Se usa en rimanAsonante
sonVocales palabraV1 palabraV2 = esVocal ((last.init) palabraV2) && esVocal ((last.init) palabraV2) || tieneTilde ((last.init) palabraV1) && tieneTilde ((last.init) palabraV2)

rimanAsonante :: Rima --Se usa en riman
rimanAsonante palabraV1 palabraV2
    |sonVocales palabraV1 palabraV2 = mismaLetra palabraV1 palabraV2
    |otherwise = False

rimanConsonante :: Rima --Se usa en riman
rimanConsonante = mismasTresLetras 

riman :: Rima --Se usa en palabrasRiman
riman palabraV1 palabraV2 
    |palabraV1 == palabraV2 = False
    |rimanAsonante palabraV1 palabraV2 || rimanConsonante palabraV1 palabraV2 = True
    |otherwise = False

esSimple :: Number -> Number -> Patron --Se usa en simple
esSimple p1 p2 estrofa = cumplen obtenerUltimaPalabra palabrasRiman ((!!) estrofa p1) ((!!) estrofa p2)

mismaPalabraAnafora :: [Palabra] -> Bool
mismaPalabraAnafora (x:xs) = all (==x) xs

esAnafora :: Patron --Se usa en anafora
esAnafora estrofa = mismaPalabraAnafora (map (head.words) estrofa)

esCombinaDos :: Patron -> Patron -> Patron --Se usa en combinaDos
esCombinaDos patron1 patron2 estrofa = patron1 estrofa && patron2 estrofa

palabraEsdrujula :: Palabra -> Bool --Se usa en sonEsdrujulas
palabraEsdrujula palabra = any tieneTilde (take (length palabra - 4) palabra)

sonEsdrujula :: [Palabra] -> Bool --Se usa en esEsdrujulas
sonEsdrujula = all palabraEsdrujula 

esEsdrujula :: Patron --Se usa en esdrujulas
esEsdrujula estrofa = sonEsdrujula (map (last.words) estrofa)

esCadena :: Conjugacion -> Estrofa -> [Bool] --Se usa en cadena
esCadena _ [] = []
esCadena conjugacion (x:xs) 
    |length (x:xs) > 1 = conjugacion x (head xs):esCadena conjugacion xs
    |otherwise = []

--Funciones Rimas
obtenerUltimaPalabra :: Verso -> Palabra
obtenerUltimaPalabra verso = reverse(recorrerVerso (reverse verso) 1)

palabrasRiman :: Rima
palabrasRiman = riman 

--Funciones Conjugaciones
conjugacionPorRimas :: Conjugacion
conjugacionPorRimas verso1 verso2 = riman (obtenerUltimaPalabra verso1) (obtenerUltimaPalabra verso2)

conjugacionHaciendoAnadiplosis :: Conjugacion
conjugacionHaciendoAnadiplosis verso1 verso2 = (last.words) verso1 == (head.words) verso2

--Funciones Patrones
simple :: Number -> Number -> Patron
simple = esSimple 

anafora :: Patron
anafora = esAnafora 

esdrujulas :: Patron
esdrujulas = esEsdrujula

combinaDos :: Patron -> Patron -> Patron
combinaDos = esCombinaDos 

cadena :: Conjugacion -> Patron
cadena conjugacion estrofa = all (== True) (esCadena conjugacion estrofa)

patronCombinado :: String -> Patron
patronCombinado "aabb" estrofa = simple 0 1 estrofa && simple 2 3 estrofa
patronCombinado "abab" estrofa = simple 0 2 estrofa && simple 1 3 estrofa
patronCombinado "abba" estrofa = simple 0 3 estrofa && simple 1 2 estrofa
patronCombinado "hardcore" estrofa = esdrujulas estrofa && cadena conjugacionPorRimas estrofa

--Funciones Puestas en Escena
gritar :: PuestaEscena
gritar = puestaBase {potencia = potencia puestaBase*1.5}

exaltarPublico :: Bool -> PuestaEscena -> PuestaEscena
exaltarPublico seExalto puestaEscena
    |seExalto = puestaEscena {publico = "Exaltado"}
    |otherwise = puestaEscena

responderAcote :: Bool -> PuestaEscena
responderAcote fueEfectivo = exaltarPublico fueEfectivo (puestaBase {potencia = potencia puestaBase*1.2})

tirarTecnica :: Bool -> PuestaEscena
tirarTecnica pudo = exaltarPublico pudo (puestaBase {potencia = potencia puestaBase*1.1})

hacerPuesta :: PuestaEscena -> PuestaEscena
hacerPuesta puestaEscena = puestaEscena

--Tuplas
tuplaAABB :: Bool -> (Bool, String)
tuplaAABB bool = (bool, "aabb")
tuplaHardcore :: Bool -> (Bool, String)
tuplaHardcore bool = (bool, "hardcore")
tuplaPublico :: Bool -> (Bool, String)
tuplaPublico bool = (bool, "Exaltacion")
tuplaPotencia :: Bool -> (Bool, String)
tuplaPotencia bool = (bool, "Potencia")

--Funciones Jurados
puntaje :: (Bool, String) -> Puntaje
puntaje (condicion, tipo)
    |tipo == "aabb" && condicion = 0.5
    |tipo == "hardcore" && condicion = 1
    |tipo == "Exaltacion" && condicion = 1
    |tipo == "Potencia" && condicion = 2
    |otherwise = 0

calcularPuntaje :: Bool -> Bool -> EstadoPublico -> Potencia -> Puntaje
calcularPuntaje aabb hardcore estadoDelPublico nivelPotencia 
    |sum (map puntaje [tuplaAABB aabb, tuplaHardcore hardcore, tuplaPublico (estadoDelPublico == "Exaltado"), tuplaPotencia (nivelPotencia >= 1.5)]) > 3 = 3
    |otherwise = sum (map puntaje [tuplaAABB aabb, tuplaHardcore hardcore, tuplaPublico (estadoDelPublico == "Exaltado"), tuplaPotencia (nivelPotencia >= 1.5)])

alToke :: Jurado
alToke estrofa puestaEscena = calcularPuntaje (patronCombinado "aabb" estrofa) (patronCombinado "hardcore" estrofa) (publico puestaEscena) (potencia puestaEscena)

--Funcion del enunciado
cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)

estrofaInfinita :: Estrofa
estrofaInfinita = repeat "esdrújula páción"

puestaPrueba :: PuestaEscena
puestaPrueba = UnaPuestaEscena 1.4 "Exaltado"



