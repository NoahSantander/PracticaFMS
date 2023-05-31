module Library where
import PdePreludat

--Modelado del enunciado
type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Rima = Palabra -> Palabra -> Bool
type Patron = Estrofa -> Bool
type Conjugacion = Verso -> Verso -> Bool
type Artista = String --Solamente interesa el nombre

esVocal :: Char -> Bool
esVocal = flip elem "aeiou"

tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"

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

esCombinaDos :: Patron -> Patron -> Patron
esCombinaDos patron1 patron2 estrofa = patron1 estrofa && patron2 estrofa

palabraEsdrujula :: Palabra -> Bool
palabraEsdrujula palabra = any tieneTilde (take (length palabra - 2) palabra)

sonEsdrujula :: [Palabra] -> Bool
sonEsdrujula = all palabraEsdrujula 

esEsdrujula :: Patron
esEsdrujula estrofa = sonEsdrujula (map (last.words) estrofa)

esCadena :: Conjugacion -> Estrofa -> [Bool]
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

--Funcion del enunciado
cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)



