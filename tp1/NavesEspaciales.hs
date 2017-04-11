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
                   where contarTodosComponentes = \nave -> (map (flip contarComponentes n) losComponentes)

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
impactar (d,i,t) n = derribarNave (parteNave d i n) t n

parteNave :: Dirección -> Int -> NaveEspacial -> NaveEspacial
parteNave d 0 n = n
parteNave d i (Base c) = Base c    
parteNave d i (Módulo c n m) = if d == Estribor then parteNave Estribor (i-1) n else parteNave Babor (i-1) m

derribarNave :: NaveEspacial -> TipoPeligro -> NaveEspacial -> NaveEspacial
derribarNave subNave tipoPeli nave = case tipoPeli of
										Pequeño -> if tieneEscudo subNave then nave else quitarSubNave subNave nave
										Grande -> if tieneCañon subNave then derribarNave subNave Pequeño nave else quitarSubNave subNave nave
										Torpedo -> quitarSubNave subNave nave

tieneEscudo :: NaveEspacial -> Bool
tieneEscudo n = (cabina n) == Escudo

tieneCañon :: NaveEspacial -> Bool
tieneCañon (Base c) = c== Cañón
tieneCañon (Módulo c n m) = (c == Cañón) || tieneCañon n || tieneCañon m 

quitarSubNave:: NaveEspacial -> NaveEspacial -> NaveEspacial
quitarSubNave subnave (Base c) = Base c
quitarSubNave subnave (Módulo c n m) = if subnave == n then (Módulo c (Base Contenedor) m) else (if subnave == m then (Módulo c n (Base Contenedor) ) else (Módulo c n m)) 
 
cabina :: NaveEspacial -> Componente
cabina (Base c) = c
cabina (Módulo c m n) = c

-- Ejercicio 6
maniobrar :: NaveEspacial -> [Peligro] -> NaveEspacial
maniobrar = foldl (\naveAcum p -> impactar p naveAcum)

-- Ejercicio 7
pruebaDeFuego :: [Peligro] -> [NaveEspacial] -> [NaveEspacial]
pruebaDeFuego ps nes = filter puedeVolar $ map (flip maniobrar ps) nes

-- Ejercicio 8
componentesPorNivel :: NaveEspacial -> Int -> Int
componentesPorNivel = undefined

dimensiones :: NaveEspacial -> (Int, Int)
dimensiones = undefined

-----------------------------------------------------------------------------------------------
------------------------------AUXILIARES-------------------------------------------------------
-----------------------------------------------------------------------------------------------

contarComponentes ::  Componente -> NaveEspacial -> Int
contarComponentes com = foldNave (\c m n -> esComponente c com + m + n) (\c -> esComponente c com) 

--Habria que hacerlo mas elegante
esComponente :: Componente -> Componente -> Int
esComponente c d = if c==d then 1 else 0
