import Data.List
import Test.HUnit

data Tarea =
  Basica String Int |
  Independientes Tarea Tarea |
  DependeDe Tarea Tarea Int deriving Eq

instance Show Tarea where
  show = foldTarea (\i h -> i)
    (\a b -> "(" ++ a ++ " y " ++ b ++ ")")
    (\a b h -> "(" ++ b ++ " tras " ++ a ++ ")")

-- Ejercicio 1

-- recTarea
recTarea :: (String -> Int -> c) -> (Tarea -> Tarea -> c -> c -> c) -> (Tarea -> Tarea -> c -> c -> Int-> c) -> Tarea -> c
recTarea recBasica recIndependiente recDependeDe tarea  = case tarea of Basica a b -> recBasica a b 
                                                                        Independientes t1 t2 -> recIndependiente t1 t2 (rec t1) (rec t2)
                                                                        DependeDe t1 t2 a -> recDependeDe t1 t2 (rec t1) (rec t2) a
                                 where rec = recTarea recBasica recIndependiente recDependeDe           



-- foldTarea
foldTarea :: (String -> Int -> c) -> (c -> c -> c) -> (c -> c -> Int -> c) -> Tarea -> c
foldTarea fBasica fIndependiente fDependeDe tarea = case tarea of Basica a b -> fBasica a b
                                                                  Independientes t1 t2 -> fIndependiente (rec t1)(rec t2)
                                                                  DependeDe t1 t2 a -> fDependeDe (rec t1)(rec t2) a
                                 where rec = foldTarea fBasica fIndependiente fDependeDe
-- Ejercicio 2

-- cantidadDeTareasBasicas
cantidadDeTareasBasicas :: [Tarea] -> Int
cantidadDeTareasBasicas xs = sum (map (cantidadDeTareasDe) xs)

cantidadDeTareasDe :: Tarea -> Int
cantidadDeTareasDe = foldTarea (\_ _ -> 1) (+) (\t1 t2 _ -> t1 + t2) 

-- cantidadMaximaDeHoras
cantidadMaximaDeHoras :: [Tarea] -> Int
cantidadMaximaDeHoras xs = sum (map (cantidadMaxima) xs)

cantidadMaxima :: Tarea -> Int
cantidadMaxima = foldTarea (const id) (+) (\num1 num2 n -> num1 + num2 + n) 
-- tareasMasLargas
tareasMasLargas :: Int -> [Tarea] -> [Tarea]
tareasMasLargas h = filter (\x -> h < cantidadMaxima x) 
-- Ejercicio 3

-- chauListas
chauListas :: [Tarea] -> Tarea
chauListas = foldr1 Independientes -- este lo puse como dijo ella, no sabia que funcionaba asi tambien

-- Ejercicio 4

-- tareasBasicas
tareasBasicas :: Tarea -> [Tarea]
tareasBasicas = foldTarea (\x n-> [Basica x n]) (++) (\t1 t2 _ -> t1++t2) 

-- esSubTareaDe
esSubTareaDe :: String -> Tarea -> Bool
esSubTareaDe s = foldTarea(\x _-> s == x)(||) (\t1 t2 _ -> t1 || t2)

-- tareasBasicasIniciales
tareasBasicasIniciales :: Tarea -> [Tarea]
tareasBasicasIniciales = foldTarea (\x n-> [Basica x n]) (\t1 t2-> t1++t2) (\t1 t2 n -> t2)

-- tareasBasicasQueDependenDe

tareasBasicasQueDependenDe :: String -> Tarea -> [Tarea]
tareasBasicasQueDependenDe n = recTarea (\s n -> []) (const $ const (++)) (\t1 t2 rec1 rec2 h -> if esSubTareaDe n t2 then (tareasBasicas t1) ++ rec2 else rec1)
-- en este podriamos poner un where para el segundo caso que nos quedo bastante largo por el if

-- Ejercicio 5

-- cuelloDeBotella
cuelloDeBotella :: Tarea -> String
-- cuelloDeBotella t1 =  nombre (fst (head (sortBy (\x y -> compare (snd y) (snd x)) (listaDependientesTupla t1))))
-- cuelloDeBotella t1 =  nombre ( fst ( mejorSegun (\x y -> (snd x) > (snd y)) (listaDependientesTupla t1) ))
cuelloDeBotella t1 = nombre (mejorSegun (tieneMasTareasDependientesEn t1) (tareasBasicas t1))
--

listaDependientesTupla :: Tarea -> [(Tarea, Int)]
listaDependientesTupla t1 = map (\x-> (x, length ( tareasBasicasQueDependenDe (nombre x) t1))) (tareasBasicas t1)

tieneMasTareasDependientesEn t1 t2 t3 = let f = (cantidadTareasDependientesEn t1) in (f t2) > (f t1) 
cantidadTareasDependientesEn t1 x = length (tareasBasicasQueDependenDe (nombre x) t1)

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun _ [x] = x
mejorSegun esMejor (x:xs) =
           let m = mejorSegun esMejor xs
           in if esMejor x m then x else m

nombre :: Tarea -> String
nombre tarea = case tarea of Basica a b -> a
                             Independientes t1 t2 -> []
                             DependeDe t1 t2 n -> []

-- Ejercicio 6

type LuzMagica a = (a -> a)

-- pasos
pasos :: (Eq a) => a -> [LuzMagica a] -> a -> Int
pasos pf = foldr (\f rec -> \x -> if x == pf then 0 else 1 + rec (f x) ) (\a -> 0)


-- Tests
main :: IO Counts
main = do runTestTT allTests

tarea1 = Basica "a" 3
tarea2 = Basica "b" 1
tarea3 = Basica "c" 1
tarea4 = Basica "d" 2
tarea5 = DependeDe (Independientes tarea2 tarea3) tarea4 2
tarea6 = DependeDe tarea2 tarea4 2
tarea7 = Independientes tarea2 (Independientes tarea3 tarea4)
tarea8 = Independientes tarea1 tarea5
tarea9 = DependeDe tarea4 (Independientes tarea2 tarea3) 2
tarea10 = DependeDe (Independientes tarea1 tarea2) (DependeDe tarea3 tarea4 3) 2
tarea11 = DependeDe tarea5 tarea1 3
tarea12 = Independientes tarea5 tarea11
tarea13 = Independientes tarea5 tarea10
lista1 = [tarea1]
lista2 = [tarea2,tarea3,tarea4] -- todas basicas
lista3 = [tarea1,tarea5] -- basica y depende
lista4 = [tarea7] -- independiente independiente 
lista5 = [tarea8] -- independiente
lista6 = [tarea1,tarea2,tarea3,tarea4] -- todas basicas x2

allTests = test [
    "ejercicio1" ~: testsEj1,
    "ejercicio2" ~: testsEj2,
    "ejercicio3" ~: testsEj3,
    "ejercicio4" ~: testsEj4,
    "ejercicio5" ~: testsEj5,
    "ejercicio6" ~: testsEj6
 ]


sumas1 :: [LuzMagica Int]
sumas1 = ((+1):sumas1)
sumas123 :: [LuzMagica Int]
sumas123 = ((+1):((+2):((+3):sumas123)))
sumas3 :: [LuzMagica Int]
sumas3 = [(+1),(+2)]
mult2 :: [LuzMagica Int]
mult2 = ((*2):mult2)

testsEj1 = test [
  "a" ~=? recTarea (\n h -> n) (\t1 t2 s1 s2 -> s1) (\t1 t2 s1 s2 h -> s1) tarea1,
  "a" ~=? foldTarea (\n h -> n) (\s1 s2 -> s1) (\s1 s2 h -> s1) tarea1,
  "bcd" ~=? recTarea (\n h -> n) (\t1 t2 s1 s2 -> s1 ++ s2) (\t1 t2 s1 s2 h -> s1) tarea7, 
  "abcd" ~=? recTarea (\n h -> n) (\t1 t2 s1 s2 -> s1 ++ s2) (\t1 t2 s1 s2 h -> s1 ++ s2) tarea8,
  "bcdbcda" ~=? foldTarea (\n h -> n) (++) (\s1 s2 h -> s1 ++ s2) tarea12, --la 12 usa la 5 y la 11 que usa la 5
  "bcdabcd" ~=? foldTarea (\n h -> n) (++) (\s1 s2 h -> s1 ++ s2) tarea13
  ]

testsEj2 = test [
  1 ~=? cantidadDeTareasBasicas lista1,
  4 ~=? cantidadDeTareasBasicas lista3,
  3 ~=? cantidadMaximaDeHoras lista1,
  9 ~=? cantidadMaximaDeHoras lista3,
  7 ~=? cantidadMaximaDeHoras lista6,
  [] ~=? tareasMasLargas 3 lista1,
  [tarea5] ~=? tareasMasLargas 3 lista3
  ]

testsEj3 = test [
  tarea1 ~=? chauListas lista1,
  tarea7 ~=? chauListas lista2,
  tarea8 ~=? chauListas lista3,
  (Independientes tarea1 (Independientes tarea2 (Independientes tarea3 tarea4))) ~=? chauListas lista6,
  (Independientes tarea4 (Independientes tarea3 (Independientes tarea2 tarea1))) ~=? chauListas (reverse lista6),
  (Independientes tarea1 tarea5) ~=? chauListas lista3,
  (Independientes tarea1 (Independientes tarea2 (Independientes tarea3 tarea4))) ~=? chauListas (concat [lista1,lista2]),
  (Independientes tarea4 (Independientes tarea3 (Independientes tarea2 tarea1))) ~=? chauListas (reverse (concat [lista1,lista2]))
  ]

testsEj4 = test [
  lista1 ~=? tareasBasicas tarea1,
  lista2 ~=? tareasBasicas tarea5,
  False ~=? esSubTareaDe "b" tarea1,
  True ~=? esSubTareaDe "b" tarea5,
  True ~=? esSubTareaDe "b" tarea12,
  True ~=? esSubTareaDe "a" tarea12,
  False ~=? esSubTareaDe "h" tarea12,
  [tarea1] ~=? tareasBasicasIniciales tarea1,
  [tarea4] ~=? tareasBasicasIniciales tarea5,
  [] ~=? tareasBasicasQueDependenDe "b" tarea2,
  [] ~=? tareasBasicasQueDependenDe "b" tarea6,  
  [tarea2] ~=? tareasBasicasQueDependenDe "d" tarea6,
  [] ~=? tareasBasicasQueDependenDe "b" tarea5,
  [tarea2,tarea3] ~=? tareasBasicasQueDependenDe "d" tarea5,
  [tarea2,tarea3,tarea2,tarea3] ~=? tareasBasicasQueDependenDe "d" tarea12,
  [] ~=? tareasBasicasQueDependenDe "h" tarea12,
  [] ~=? tareasBasicasQueDependenDe "b" tarea12,
  [tarea2,tarea3,tarea4] ~=? tareasBasicasQueDependenDe "a" tarea11
  ]

testsEj5 = test [
  "a" ~=? cuelloDeBotella tarea1,
  "d" ~=? cuelloDeBotella tarea5,
  "b" ~=? cuelloDeBotella tarea7,
  "d" ~=? cuelloDeBotella tarea8
  ]


testsEj6 = test [
  5 ~=? pasos 10 sumas1 5,
  2 ~=? pasos 10 sumas3 7,
  1 ~=? pasos 2 sumas1 1,
  10 ~=? pasos 1024 mult2 1,
  30 ~=? pasos 60 sumas123 0
  ]
