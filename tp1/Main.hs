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

--naves agregadas para ejercicio 4
nave10 = Módulo Contenedor (Módulo Contenedor (Módulo Cañón (Base Cañón) (Base Motor)) (Módulo Motor (Base Cañón) (Base Cañón))) (Base Motor)

--naves agregadas para ejercicio 5
nave11 = Módulo Contenedor (Módulo Escudo (Base Cañón) (Base Motor)) (Base Motor)
nave12 = Módulo Contenedor (Módulo Escudo (Base Motor) (Base Motor)) (Base Motor)
nave13 = Módulo Contenedor (Base Contenedor) (Base Motor)
nave14 = Módulo Contenedor (Módulo Cañón (Base Motor) (Base Cañón)) (Módulo Contenedor (Módulo Cañón (Base Cañón) (Base Motor)) (Base Motor))
nave15 = Módulo Contenedor (Módulo Cañón (Base Motor) (Base Cañón)) (Módulo Contenedor (Módulo Cañón (Base Cañón) (Base Motor)) (Base Contenedor))
nave16 = Módulo Contenedor (Módulo Cañón (Base Motor) (Base Cañón)) (Módulo Contenedor (Módulo Cañón (Base Cañón) (Base Contenedor)) (Base Motor))
nave17 = Módulo Contenedor (Módulo Contenedor (Módulo Cañón (Base Escudo) (Base Motor)) (Base Contenedor)) (Base Contenedor)
nave18 = Módulo Contenedor (Módulo Cañón (Base Motor) (Base Cañón)) (Base Contenedor)
nave28 = Módulo Contenedor (Módulo Cañón (Base Motor) (Base Cañón)) (Módulo Contenedor (Módulo Cañón (Base Contenedor) (Base Motor)) (Base Motor))

--naves agregadas para ejercicio 6
nave19 = Módulo Contenedor (Módulo Contenedor (Módulo Cañón (Base Escudo) (Base Motor)) (Módulo Motor (Base Escudo) (Base Cañón))) (Base Contenedor)
nave20 = Módulo Contenedor (Base Escudo) (Módulo Escudo (Base Escudo) (Base Cañón))
nave21 = Módulo Contenedor (Base Escudo) (Módulo Escudo (Base Escudo) (Base Contenedor))
nave22 = Módulo Contenedor (Base Escudo) (Base Contenedor)

--naves agregadas para ejercicio 7
nave23 = Módulo Escudo (Módulo Cañón (Módulo Contenedor (Base Escudo) (Base Motor)) (Base Motor)) (Módulo Motor (Base Escudo) (Módulo Escudo (Base Contenedor) (Base Contenedor)))
nave24 = Módulo Contenedor (Módulo Motor (Módulo Escudo (Base Contenedor) (Base Contenedor)) (Base Escudo)) (Módulo Motor (Base Motor) (Módulo Escudo (Base Contenedor) (Base Contenedor)))
nave25 = Módulo Escudo (Módulo Escudo (Base Motor) (Base Cañón)) (Módulo Cañón (Base Escudo) (Módulo Contenedor (Base Motor) (Base Escudo)))
nave26 = Módulo Escudo (Módulo Cañón (Base Contenedor) (Base Motor)) (Módulo Motor (Base Escudo) (Módulo Escudo (Base Contenedor) (Base Contenedor)))
nave27 = Módulo Escudo (Módulo Escudo (Base Contenedor) (Base Cañón)) (Módulo Cañón (Base Escudo) (Módulo Contenedor (Base Motor) (Base Contenedor)))

--naves agregadas para el ejercicio 8
nave29 = Módulo Escudo 
		(Módulo Escudo (Base Contenedor) (Módulo Motor (Base Contenedor) (Base Motor))) 
		(Módulo Escudo (Módulo Contenedor (Base Motor) (Base Contenedor)) (Módulo Escudo (Base Cañón) (Base Escudo)))

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
--                                   Contenedor
--Peligro (Babor, 1, Torpedo) --> Escudo      Motor
--                            Cañón Motor  
--Deberia dar:
--               Contenedor
--          Contenedor       Motor  
-------------------------------------------------------------------------------
  nave12 ~=? impactar (Babor, 1 , Pequeño) nave12,
--                                   Contenedor
--Peligro (Babor, 1, Pequeño) --> Escudo      Motor
--                             Motor Motor             
--Deberia dar: (no se debe modificar)
--               Contenedor
--          Escudo       Motor  
--       Motor  Motor
-------------------------------------------------------------------------------
  nave13 ~=? impactar (Babor, 1, Grande) nave12,
--                                   Contenedor
--Peligro (Babor, 1, Grande) --> Escudo      Motor
--                            Motor Motor             
--Deberia dar:
--               Contenedor
--          Contenedor       Motor  
-------------------------------------------------------------------------------
  nave11 ~=? impactar (Babor, 1, Grande) nave11,
--                                   Contenedor
--Peligro (Babor, 1, Grande) --> Escudo      Motor
--                            Cañón Motor             
--Deberia dar: (no se debe modificar)
--               Contenedor
--          Escudo       Motor  
--      Cañón Motor  
-------------------------------------------------------------------------------
  nave28 ~=? impactar (Babor, 3, Grande) nave14,
--                                           Contenedor
--                                  Cañón                 Contenedor
--                               Motor Contenedor      Cañón       Motor
--Peligro (Babor, 3, Grande) -->                    Cañón Motor          
--Deberia dar:
--                    Contenedor
--          Cañón                 Contenedor  
--      Motor Contenedor      Cañón       Motor
--                      Contenedor Motor
-------------------------------------------------------------------------------
  nave15 ~=? impactar (Estribor, 2, Grande) nave14,
--              Contenedor
--    Cañón                 Contenedor  
--Motor Contenedor      Cañón       Motor <-- Peligro (Estribor, 2, Grande)
--                    Cañón Motor          
--Deberia dar:
--              Contenedor
--    Cañón                 Contenedor  
--Motor Contenedor      Cañón       Contenedor
--                    Cañón Motor       
-------------------------------------------------------------------------------
  nave18 ~=? impactar (Estribor, 1, Grande) nave14,
--              Contenedor
--    Cañón                 Contenedor  <-- Peligro (Estribor, 1, Grande)
--Motor Contenedor      Cañón       Motor
--                    Cañón Motor          
--Deberia dar:
--              Contenedor
--    Cañón                 Contenedor
--Motor Contenedor
-------------------------------------------------------------------------------
  nave16 ~=? impactar (Estribor, 3, Grande) nave14
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
  ]

testsEj6 = test [
-------------------------------------------------------------------------------
  nave19 ~=? maniobrar nave6 [(Estribor, 1, Pequeño), (Babor, 3, Pequeño)],
--                                                 Contenedor
--                                            Contenedor        Motor <-- (1°) Peligro (Estribor, 1, Pequeño)
--                                        Cañón        Motor
--Peligro (Babor, 3, Pequeño) (2°) --> Escudo Motor Escudo Motor
--Deberia dar:
--                  Contenedor 
--        Contenedor        Contenedor
--    Cañón         Motor 
--Escudo Motor  Escudo Cañón
-------------------------------------------------------------------------------
  contenedorSolo ~=? maniobrar nave6 [(Estribor, 1, Pequeño), (Babor, 3, Pequeño), (Estribor, 0, Torpedo)],
--                                                 Contenedor  <-- (3°) Peligro (Estribor, 0, Torpedo)
--   Peligro (Estribor, 1, Pequeño) (1°) --> Contenedor        Motor 
--                                        Cañón        Motor
--Peligro (Babor, 3, Pequeño) (2°) --> Escudo Motor Escudo Motor
--Deberia dar: (el torpedo destruye todo)
-- Contenedor
-------------------------------------------------------------------------------
  contenedorSolo ~=? maniobrar nave6 [(Estribor, 0, Torpedo), (Estribor, 1, Pequeño), (Babor, 3, Pequeño)],
-- Cambiamos el orden al test de arriba para atacar componentes que ya no existen
-------------------------------------------------------------------------------
  nave21 ~=? maniobrar nave20 [(Estribor, 1, Grande), (Estribor, 2, Pequeño)],
--    Contenedor
-- Escudo    Escudo <-- (1°) Peligro (Estribor, 1, Grande)
--        Escudo Cañón <-- (2°) Peligro (Estribor, 2, Pequeño)
-- Deberia dar:
--     Contenedor
-- Escudo    Escudo
--        Escudo Contenedor
-------------------------------------------------------------------------------
  nave22 ~=? maniobrar nave20 [(Estribor, 2, Pequeño), (Estribor, 1, Grande)],
--    Contenedor
-- Escudo    Escudo <-- (2°) Peligro (Estribor, 1, Grande)
--        Escudo Cañón <-- (1°) Peligro (Estribor, 2, Pequeño)
-- Deberia dar:
--    Contenedor
-- Escudo Contenedor
-------------------------------------------------------------------------------
  nave17 ~=? maniobrar nave6 [(Estribor, 1, Pequeño), (Estribor, 2, Grande), (Babor, 3, Pequeño)]
--                                                 Contenedor  
--                                             Contenedor        Motor  <-- (1°) Peligro (Estribor, 1, Pequeño)
--                                        Cañón        Motor  <-- (2°) Peligro (Estribor, 2, Grande)
-- Peligro (Babor, 3, Pequeño) (3°) --> Escudo Motor Escudo Motor  
-- Deberia dar:
--                  Contenedor
--          Contenedor      Contenedor
--     Cañón      Contenedor
-- Escudo Motor
-------------------------------------------------------------------------------
  ]

testsEj7 = test [
  3 ~=? length (pruebaDeFuego [(Babor,1,Grande),(Babor,2,Torpedo),(Estribor, 1, Pequeño)] [nave1,nave2,nave3,nave4,nave5,nave6,nave7,nave8,nave9]),
  [nave26,nave27] ~=? pruebaDeFuego [(Babor, 2, Torpedo), (Estribor, 3, Grande), (Estribor, 0, Pequeño)] [nave23,nave24,nave25]
  ]

testsEj8 = test [
  8 ~=? componentesPorNivel nave9 3,
  4 ~=? componentesPorNivel nave14 2,
  2 ~=? componentesPorNivel nave14 3,
  nave29 ~=? maniobrar nave9 [(Babor,1,Grande),(Babor,2,Torpedo)],
  (4,6) ~=? (dimensiones $ maniobrar nave9 [(Babor,1,Grande),(Babor,2,Torpedo)]),
  (3,2) ~=? dimensiones nave11,
  (2,2) ~=? (dimensiones $ maniobrar nave20 [(Estribor, 2, Pequeño), (Estribor, 1, Grande)])
  ]


--Ejemplos de referencia para maniobrar:	
--maniobrar nave9 [(Babor, 0, Grande),(Babor,2,Torpedo),(Estribor,0,Pequeño)] destruye solo el subárbol izquierdo del subárbol izquierdo.
--maniobrar nave9 [(Estribor,0,Pequeño),(Babor,2,Torpedo),(Babor, 1, Grande)] destruye todo el subárbol izquierdo.