module Main where
import NavesEspaciales
import Test.HUnit
import Data.List



--Naves para pruebas:
contenedorSolo = Base Contenedor
soloEscudo = Base Escudo
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
nave10 = Módulo Contenedor (Módulo Contenedor (Módulo Cañón (Base Cañón) (Base Motor)) (Módulo Motor (Base Cañón) (Base Cañón))) (Base Motor)
nave11 = Módulo Contenedor (Módulo Escudo (Base Cañón) (Base Motor)) (Base Motor)
nave12 = Módulo Contenedor (Módulo Escudo (Base Motor) (Base Motor)) (Base Motor)
nave13 = Módulo Contenedor (Base Contenedor) (Base Motor)
nave14 = Módulo Contenedor (Módulo Cañón (Base Motor) (Base Cañón)) (Módulo Contenedor (Módulo Cañón (Base Cañón) (Base Motor)) (Base Motor))
nave15 = Módulo Contenedor (Módulo Cañón (Base Motor) (Base Cañón)) (Módulo Contenedor (Módulo Cañón (Base Cañón) (Base Motor)) (Base Contenedor))
nave16 = Módulo Contenedor (Módulo Cañón (Base Motor) (Base Cañón)) (Módulo Contenedor (Módulo Cañón (Base Cañón) (Base Contenedor)) (Base Motor))

soloUnMotor = Base Motor
puroContenedor = Módulo Contenedor (Base Contenedor) (Base Contenedor)
tresCañones = Módulo Cañón (Base Cañón) (Base Cañón)

contenedorYCañon = Módulo Contenedor (Base Cañón) (Base Contenedor)
otroCañon = Módulo Contenedor (Base Contenedor) (Base Cañón)

escudoSinCañon = Módulo Escudo (Base Contenedor) (Base Contenedor)

protegido = Módulo Escudo (Base Contenedor) (Base Cañón)
protegidoNivel1Estribor = Módulo Contenedor soloUnMotor protegido

superProtegido = Módulo Motor protegido protegido

desbalanceado = Módulo Escudo (Base Contenedor) protegido

todoAEscudo :: Componente -> Componente
todoAEscudo c = Escudo

escudoACanon :: Componente -> Componente
escudoACanon c = if (c == Escudo) then Cañón else c 

--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8
  ]

testsEj2 = test [
  0 ~=? capacidad soloUnMotor,
  3 ~=? capacidad puroContenedor,
  3 ~=? poderDeAtaque tresCañones,
  2 ~=? poderDeAtaque nave9,
  True ~=? puedeVolar soloUnMotor,
  False ~=? puedeVolar contenedorSolo,
  True ~=? mismoPotencial nave4 nave5,
  True ~=? mismoPotencial nave6 nave7,
  False ~=? mismoPotencial nave1 nave6
  ]

testsEj3 = test [
  nave8 ~=? mayorCapacidad [nave1, nave2, nave3, nave4, nave5, nave6, nave7, nave8],
  nave1 ~=? mayorCapacidad [nave1, nave2, nave3],
  nave1 ~=? mayorCapacidad [nave1],
  nave4 ~=? mayorCapacidad [nave1, nave4],
  nave9 ~=? mayorCapacidad [nave1, nave4, nave9]
  ]

testsEj4 = test [
  soloEscudo ~=? transformar todoAEscudo nave1,
  nave10 ~=? transformar escudoACanon nave6
  ]

testsEj5 = test [
  nave13 ~=? impactar (Babor, 1, Torpedo) nave11,
  nave12 ~=? impactar (Babor, 1 , Pequeño) nave12,
  nave13 ~=? impactar (Babor, 1, Grande) nave12,
  nave11 ~=? impactar (Babor, 1, Grande) nave11,
  nave14 ~=? impactar (Babor, 3, Grande) nave14,
  nave15 ~=? impactar (Estribor, 2, Grande) nave14
-------------------------------------------------------------------------------
  --nave16 ~=? impactar (Estribor, 3, Grande) nave14
--              Contenedor
--    Cañón                 Contenedor
--Motor Contenedor      Cañón       Motor
--                    Cañón Motor          <-- Peligro (Estribor, 3, Grande)
--Deberia dar:
--              Contenedor
--    Cañón                 Contenedor
--Motor Contenedor      Cañón       Motor
--                    Cañón Contenedor        
-------------------------------------------------------------------------------
  --nave14 ~=? impactar (Estribor, 1, Grande) nave14
--              Contenedor
--    Cañón                 Contenedor    <-- Peligro (Estribor, 1, Grande)
--Motor Contenedor      Cañón       Motor
--                    Cañón Motor          
--Deberia dar: el mismo arbol porque hay un Cañón en su subarbol
-------------------------------------------------------------------------------
  ]

testsEj6 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj7 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  --[nave1,nave3,nave9] ~=? pruebaDeFuego [(Babor,1,Grande),(Babor,2,Torpedo),(Estribor, 1, Pequeño)] [nave1,nave2,nave3,nave4,nave5,nave6,nave7,nave8,nave9]
  ]

testsEj8 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  --(4,6) ~=? (dimensiones $ maniobrar nave9 [(Babor,1,Grande),(Babor,2,Torpedo)])
  ]


--Ejemplos de referencia para maniobrar:	
--maniobrar nave9 [(Babor, 0, Grande),(Babor,2,Torpedo),(Estribor,0,Pequeño)] destruye solo el subárbol izquierdo del subárbol izquierdo.
--maniobrar nave9 [(Estribor,0,Pequeño),(Babor,2,Torpedo),(Babor, 1, Grande)] destruye todo el subárbol izquierdo.