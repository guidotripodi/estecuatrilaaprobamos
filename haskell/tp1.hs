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
cantidadDeTareasBasicas = foldr (\n rec -> cantidadTareasB (n) + rec) 0

cantidadTareasB :: Tarea -> Int
cantidadTareasB = foldTarea (\s n -> 1) (\t1 t2 -> t1 + t2) (\t1 t2 n -> t1 + t2) 

-- cantidadMaximaDeHoras
cantidadMaximaDeHoras :: [Tarea] -> Int
cantidadMaximaDeHoras = foldr(\n rec -> cantidadMaxima(n) + rec ) 0

cantidadMaxima :: Tarea -> Int
cantidadMaxima = foldTarea (\s n -> n) (\t1 t2 -> t1 + t2) (\t1 t2 n -> t1 + t2 + n) 
-- tareasMasLargas
tareasMasLargas :: Int -> [Tarea] -> [Tarea]
tareasMasLargas h = filter (\x -> h < cantidadMaxima x) 
-- Ejercicio 3

-- chauListas
chauListas :: [Tarea] -> Tarea
chauListas xs = foldr1(\n rec -> (Independientes n rec)) xs

-- Ejercicio 4

-- tareasBasicas
tareasBasicas :: Tarea -> [Tarea]
tareasBasicas = foldTarea (\x n-> [Basica x n]) (\t1 t2-> t1++t2) (\t1 t2 n -> t1++t2) 

-- esSubTareaDe
esSubTareaDe :: String -> Tarea -> Bool
esSubTareaDe s = foldTarea(\x n-> s == x)(\t1 t2 -> t1 || t2) (\t1 t2 h -> t1 || t2)

-- tareasBasicasIniciales
tareasBasicasIniciales :: Tarea -> [Tarea]
tareasBasicasIniciales = foldTarea (\x n-> [Basica x n]) (\t1 t2-> t1++t2) (\t1 t2 n -> t2)

-- tareasBasicasQueDependenDe
tareasBasicasQueDependenDe :: String -> Tarea -> [Tarea]
tareasBasicasQueDependenDe n = recTarea (\x s -> []) (\t1 t2 rec1 rec2 -> rec1 ++ rec2) (\t1 t2 rec1 rec2 h-> if (esSubTareaDe n t2) then (tareasBasicas t1) else []) 

-- Ejercicio 5

-- cuelloDeBotella
cuelloDeBotella :: Tarea -> String
cuelloDeBotella t1 =  nombre (fst (head (sortBy (\x y -> compare (snd y) (snd x)) (listaDependientesTupla t1))))
--
listaDependientesTupla :: Tarea -> [(Tarea, Int)]
listaDependientesTupla t1 = map (\x-> (x, length ( tareasBasicasQueDependenDe (nombre x) t1))) (tareasBasicas t1)


nombre :: Tarea -> String
nombre tarea = case tarea of Basica a b -> a
                             Independientes t1 t2 -> []
                             DependeDe t1 t2 a -> []

-- Ejercicio 6

type LuzMagica a = (a -> a)

-- pasos
pasos :: (Eq a) => a -> a -> [(a -> a)] -> Int
pasos zi zf xs = length( takeWhile (/=zf) (foldl g [zi] xs))
                        where g = (\ys f -> ys ++ [f (last ys)])

--evaluarListaMagicaN :: Int -> [LuzMagica a] -> a -> a
--evaluarListaMagicaN n xs i = evaluarListaMagica (take n xs) i

--evaluarListaMagica :: [LuzMagica a] -> a -> a
--evaluarListaMagica xs i = foldl (flip ($)) i xs


-- Tests
main :: IO Counts
main = do runTestTT allTests

tarea1 = Basica "a" 3
tarea2 = Basica "b" 1
tarea3 = Basica "c" 1
tarea4 = Basica "d" 2
tarea5 = DependeDe (Independientes tarea2 tarea3) tarea4 2
tarea6 = DependeDe tarea2 tarea4 2
lista1 = [tarea1]
lista2 = [tarea2,tarea3,tarea4]
lista3 = [tarea1,tarea5]

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

testsEj1 = test [
  "a" ~=? recTarea (\n h -> n) (\t1 t2 s1 s2 -> s1) (\t1 t2 s1 s2 h -> s1) tarea1,
  "a" ~=? foldTarea (\n h -> n) (\s1 s2 -> s1) (\s1 s2 h -> s1) tarea1
  ]

testsEj2 = test [
  1 ~=? cantidadDeTareasBasicas lista1,
  4 ~=? cantidadDeTareasBasicas lista3,
  3 ~=? cantidadMaximaDeHoras lista1,
  9 ~=? cantidadMaximaDeHoras lista3,
  [] ~=? tareasMasLargas 3 lista1,
  [tarea5] ~=? tareasMasLargas 3 lista3
  ]

testsEj3 = test [
  tarea1 ~=? chauListas lista1
  ]

testsEj4 = test [
  lista1 ~=? tareasBasicas tarea1,
  lista2 ~=? tareasBasicas tarea5,
  False ~=? esSubTareaDe "b" tarea1,
  True ~=? esSubTareaDe "b" tarea5,
  [tarea1] ~=? tareasBasicasIniciales tarea1,
  [tarea4] ~=? tareasBasicasIniciales tarea5,
  [] ~=? tareasBasicasQueDependenDe "b" tarea2,
  [] ~=? tareasBasicasQueDependenDe "b" tarea6, --lo hicimos nosotros 
  [tarea2] ~=? tareasBasicasQueDependenDe "d" tarea6,
  [] ~=? tareasBasicasQueDependenDe "b" tarea5,
  [tarea2,tarea3] ~=? tareasBasicasQueDependenDe "d" tarea5
  ]

testsEj5 = test [
  "a" ~=? cuelloDeBotella tarea1,
  "d" ~=? cuelloDeBotella tarea5
  ]


testsEj6 = test [
  5 ~=? pasos 10 sumas1 5,
  30 ~=? pasos 60 sumas123 0
  ]
