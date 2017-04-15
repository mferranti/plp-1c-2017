module NavesEspaciales (Componente(Contenedor, Motor, Escudo, Cañón), NaveEspacial(Módulo, Base), Dirección(Babor, Estribor), TipoPeligro(Pequeño, Grande, Torpedo), Peligro, foldNave, capacidad, poderDeAtaque, puedeVolar, mismoPotencial, mayorCapacidad, transformar, impactar, maniobrar, pruebaDeFuego, componentesPorNivel, dimensiones) where

import Data.List

data Componente = Contenedor | Motor | Escudo | Cañón deriving (Eq, Show, Enum)

data NaveEspacial = Módulo Componente NaveEspacial NaveEspacial | Base Componente deriving Eq

data Dirección = Babor | Estribor deriving Eq

data TipoPeligro = Pequeño | Grande | Torpedo deriving Eq

type Peligro = (Dirección, Int, TipoPeligro)

instance Show NaveEspacial where
  show = ("\n" ++) . (padNave 0 0 False)
  
-------------------------------------------------------------------------------------------------
-------------------------------Naves ejemplo-----------------------------------------------------
-------------------------------------------------------------------------------------------------
naveSinMotor = Base Contenedor  
nave1 = Base Motor
nave2 = Módulo Cañón (Base Escudo) (Base Motor)
nave3 = Módulo Motor (Base Escudo) (Base Cañón)
nave4 = Módulo Contenedor nave2 nave3
nave5 = Módulo Contenedor nave3 nave2
nave6 = Módulo Contenedor nave4 nave1
nave7 = Módulo Contenedor nave1 nave5
nave8 = Módulo Contenedor nave1 nave6
nave9 = Módulo Escudo 
    (Módulo Escudo (Módulo Escudo (Base Escudo) (Base Cañón)) (Módulo Motor (Base Contenedor) (Base Motor))) 
    (Módulo Escudo (Módulo Contenedor (Base Motor) (Base Contenedor)) (Módulo Escudo (Base Cañón) (Base Escudo))) 

naveSinCanon = Módulo Escudo
    (Módulo Escudo (Módulo Escudo (Base Escudo) (Base Motor)) (Módulo Motor (Base Contenedor) (Base Motor)))
    (Módulo Escudo (Módulo Contenedor (Base Motor) (Base Contenedor)) (Módulo Escudo (Base Motor) (Base Escudo)))
	

nave14 = Módulo Contenedor (Módulo Cañón (Base Motor) (Base Cañón)) (Módulo Contenedor (Módulo Cañón (Base Cañón) (Base Motor)) (Base Motor))


padNave nivel acum doPad (Base c) = (if doPad then pad (4*nivel + acum) else "") ++ show c
padNave nivel acum doPad (Módulo x i d) = (if doPad then pad (4*nivel + acum) else "") ++ show x ++ 
            pad 4 ++ padNave (nivel+1) (acum+l) False i ++ "\n" ++
            padNave (nivel+1) (acum+l) True d where l = length $ show x

componenteAEscudo :: Componente -> Componente
componenteAEscudo c = Escudo 
					  
losComponentes = [Contenedor ..]

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

pad :: Int -> String
pad i = replicate i ' '

--Ejercicio 1
--Modulo : Componente -> r -> r -> r
--Base : Componente -> r
foldNave :: (Componente -> r -> r -> r) -> (Componente -> r) -> NaveEspacial -> r 
foldNave fNaveCompleta fNaveBase (Base c) = fNaveBase c
foldNave fNaveCompleta fNaveBase (Módulo c n m) = fNaveCompleta c (subNave n) (subNave m)
                                                where subNave = foldNave fNaveCompleta fNaveBase

--Ejercicio 2
capacidad :: NaveEspacial -> Int
capacidad = contarComponentes Contenedor

poderDeAtaque :: NaveEspacial -> Int
poderDeAtaque = contarComponentes Cañón   

--Arreglarlo, sacarle el parametro y usar flip o .
puedeVolar :: NaveEspacial -> Bool
puedeVolar n = (>0) (contarComponentes Motor n)

--Sacarle el argumento
mismoPotencial :: NaveEspacial -> NaveEspacial -> Bool
mismoPotencial n m = (contarTodosComponentes n) == (contarTodosComponentes m)
                   where contarTodosComponentes = \nave -> (map (flip contarComponentes nave) losComponentes)

--mismoPotencial
--Ejercicio 3

mayorCapacidad :: [NaveEspacial] -> NaveEspacial
mayorCapacidad ns = head (filter (\n -> capacidad n == maxC) ns)
                  where maxC = maximum $ map capacidad ns

--Ejercicio 4

transformar :: (Componente -> Componente) -> NaveEspacial -> NaveEspacial
transformar compReplace = foldNave (\c -> Módulo (compReplace c)) (Base . compReplace)

-- Ejercicio 5
impactar :: Peligro -> NaveEspacial -> NaveEspacial
impactar (d,0,t) subnave = derribar subnave t
impactar (d,i,t) (Base c) = Base c
impactar (d,i,t) (Módulo c b e) = if d == Babor then Módulo c (impactarRec b) e else Módulo c b (impactar(d,i-1,t) e)
                     where impactarRec = \n -> impactar(d,i-1,t) n

					 
naveMasA :: Dirección -> Int -> NaveEspacial -> NaveEspacial
--naveMasA d 0 n = n 
naveMasA Estribor i n = foldNave (\c b e -> if (esBase e) then b else e) (\c -> n) n  
naveMasA Babor i n = foldNave (\c b e -> if (esBase b) then e else b) (\c -> n) n

esBase:: NaveEspacial -> Bool
esBase (Base c) = True
esBase (Módulo c n m) = False

-- foldNave :: (Componente -> r -> r -> r) -> (Componente -> r) -> NaveEspacial -> r 
-- foldNave fNaveCompleta fNaveBase (Base c) = fNaveBase c
-- foldNave fNaveCompleta fNaveBase (Módulo c n m) = fNaveCompleta c (subNave n) (subNave m)
                                                -- where subNave = foldNave fNaveCompleta fNaveBase


					 
parteNave :: Dirección -> Int -> NaveEspacial -> NaveEspacial
parteNave d 0 n = n
parteNave d i (Base c) = Base c
parteNave d i (Módulo c m n) = if d == Babor then parteNave Babor (i-1) n else parteNave Estribor (i-1) m

derribar :: NaveEspacial -> TipoPeligro -> NaveEspacial
derribar nave tipoPeli = case tipoPeli of
                    Pequeño -> if tieneEscudo nave then nave else (Base Contenedor)
                    Grande -> if protegidoPorCañon nave then derribar nave Pequeño else (Base Contenedor)
                    Torpedo -> (Base Contenedor)

tieneEscudo :: NaveEspacial -> Bool
tieneEscudo n = (cabina n) == Escudo

protegidoPorCañon :: NaveEspacial -> Bool
protegidoPorCañon nave = tieneCañon (subnaveDir Babor) || tieneCañon (subnaveDir Estribor)
                         where subnaveDir = \dir -> parteNave dir 1 nave

tieneCañon :: NaveEspacial -> Bool
tieneCañon = foldNave (\c m n -> (c == Cañón) || m || n) ((==) Cañón)

cabina :: NaveEspacial -> Componente
cabina = foldNave (\c m n -> c) id

-- Ejercicio 6
maniobrar :: NaveEspacial -> [Peligro] -> NaveEspacial
maniobrar = foldl (\naveAcum p -> impactar p naveAcum)


-- Ejercicio 7
pruebaDeFuego :: [Peligro] -> [NaveEspacial] -> [NaveEspacial]
pruebaDeFuego ps nes = filter puedeVolar $ map (flip maniobrar ps) nes

-- Ejercicio 8
componentesPorNivel :: NaveEspacial -> Int -> Int
componentesPorNivel nave nivel = snd $ head $ filter (\t -> fst t == nivel) listaPorNivel
                                where listaPorNivel = foldNave (\c brec erec -> (:) (0,1) $ actualizarNivel $ mergeNivel brec erec) (const [(0,1)]) nave

mergeNivel :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
mergeNivel [] ys = ys
mergeNivel xs [] = xs
mergeNivel (x:xs) (y:ys) = if (fst x == fst y) then (fst x, snd x + snd y) : (mergeNivel xs ys) else (fst x, snd x) : (mergeNivel xs (y:ys))

actualizarNivel :: [(Int,Int)] -> [(Int,Int)]
actualizarNivel = map (\x -> (1 + fst x, snd x))

-- El algoritmo de componentesPorNivel esta basado en la siguiente reduccion.
-- El primer elemento de la tupla es el nivel y el segundo elemento la cantidad acumulada.
--            escudo             [(0,1)]
--      motor              [(0,1),(1,2)]
--            escudo             [(0,1)]
--canon              [(0,1),(1,2),(2,4)]
--            escudo             [(0,1)]
--      motor              [(0,1),(1,2)]
--            escudo             [(0,1)]
--
--
--            escudo             [(0,1)]
--      motor        [(0,1),(1,2)]
--            escudo             [(0,1)]
--canon              [(0,1),(1,2)(2,2)]
--
--      conte           [(0,1)]



dimensiones :: NaveEspacial -> (Int, Int)
dimensiones nave = (maximosComponentes nave, altura nave)

maximosComponentes :: NaveEspacial -> Int
maximosComponentes n =  maximum $ map (componentesPorNivel n) [0..altura n]


-- naveA = Base Contenedor
-- naveB = Módulo Cañón naveA (Base Contenedor)
-- naveC = Módulo Cañón naveB (Base Contenedor)
-- naveD = Módulo Cañón naveC (Base Contenedor)
-- naveE = Módulo Cañón naveD (Base Contenedor)
-- naveF = Módulo Cañón naveE (Base Contenedor)

altura:: NaveEspacial -> Int
altura = foldNave (\c e b -> 1 + max e b ) (const 0)   

-- foldNave :: (Componente -> r -> r -> r) -> (Componente -> r) -> NaveEspacial -> r 
-- foldNave fNaveCompleta fNaveBase (Base c) = fNaveBase c
-- foldNave fNaveCompleta fNaveBase (Módulo c n m) = fNaveCompleta c (subNave n) (subNave m)


-----------------------------------------------------------------------------------------------
------------------------------AUXILIARES-------------------------------------------------------
-----------------------------------------------------------------------------------------------

contarComponentes ::  Componente -> NaveEspacial -> Int
contarComponentes com = foldNave (\c m n -> esComponente c com + m + n) (\c -> esComponente c com)

--Habria que hacerlo mas elegante
esComponente :: Componente -> Componente -> Int
esComponente c d = if c==d then 1 else 0
