module Pruebas where
import Tipos

-------------------------------------------
-- Autómatas finitos deterministas (AFD) --
-------------------------------------------

-- AFD D para el lenguaje de las palabras que terminan con 'a'
getAutomataD :: Af
getAutomataD = ([0,1],['a','b','c'],[(0,'a',1),(0,'b',0),(0,'c',0),(1,'a',1),(1,'b',0),(1,'c',0)],0,[1])

-- AFD D1 para el lenguaje de las palabras que no contienen 'bb'
getAutomataD1 :: Af
getAutomataD1 = ([0,1,2,3,4],['a','b','c'],[(0,'a',1),(0,'b',2),(0,'c',1),(1,'a',1),(1,'b',3),(1,'c',1),(2,'a',1),(2,'b',4),(2,'c',1),(3,'a',1),(3,'b',4),(3,'c',1),(4,'a',4),(4,'b',4),(4,'c',4)],0,[0,1,2,3])

-- AFD D2 para el lenguaje de las palabras que contienen 'aaa' o terminan en 'a' y, además, no contienen 'c'
getAutomataD2 :: Af
getAutomataD2 = ([0,1,2,3,4],['a','b','c'],[(0,'a',1),(0,'b',0),(0,'c',4),(1,'a',2),(1,'b',0),(1,'c',4),(2,'a',3),(2,'b',0),(2,'c',4),(3,'a',3),(3,'b',3),(3,'c',4),(4,'a',4),(4,'b',4),(4,'c',4)],0,[1,2,3])

-- AFD D3 para el lenguaje de las palabras que contienen 'aaa' o terminan en 'a' y, además, no contienen 'c'
getAutomataD3 :: Af
getAutomataD3 = ([0,1,2,3,4,5,6,7,8,9],['a','b','c'],[(0,'a',1),(0,'b',0),(0,'c',4),(1,'a',2),(1,'b',0),(1,'c',4),(2,'a',3),(2,'b',0 ),(2,'c',4),(3,'a',3),(3,'b',3),(3,'c',4),(4,'a',9),(4,'b',9),(4,'c',4),(5,'a',5 ),(5,'b',5),(5,'c',6),(6,'a',3),(6,'b',6),(6,'c',3),(7,'a',8),(7,'b',8),(7,'c',8 ),(8,'a',7),(8,'b',7),(8,'c',7),(9,'a',4),(9,'b',4),(9,'c',9)],0,[1,2,3,5,7])

-- AFD D3s para el lenguaje de las palabras que contienen 'aaa' o terminan en 'a' y, además, no contienen 'c'
getAutomataD3s :: Af
getAutomataD3s = ([0,1,2,3,4,9],['a','b','c'],[(0,'a',1),(0,'b',0),(0,'c',4),(1,'a',2),(1,'b',0),(1,'c',4),(2,'a',3),(2,'b',0 ),(2,'c',4),(3,'a',3),(3,'b',3),(3,'c',4),(4,'a',9),(4,'b',9),(4,'c',4 ),(9,'a',4),(9,'b',4),(9,'c',9)],0,[1,2,3])

-----------------------------------------------
-- Autómatas finitos no-deterministas (AFND) --
-----------------------------------------------

-- AFND N para el lenguaje de las palabras que terminan con 'a'
getAutomataN :: Af
getAutomataN = ([0,1],['a','b','c'],[(0,'a',0),(0,'a',1),(0,'b',0),(0,'c',0)],0,[1])

-- AFND N1 para el lenguaje de las palabras que no contienen 'bb'
getAutomataN1 :: Af
getAutomataN1 = ([0,1,2,3,4],['a','b','c'],[(0,'a',1),(0,'b',2),(0,'c',1),(1,'a',1),(1,'b',3),(1,'c',1),(2,'a',1),(2,'b',4 ),(2,'c',1),(3,'a',1),(3,'b',4),(3,'c',1)],0,[0,1,2,3])

-- AFND N1s para el lenguaje de las palabras que no contienen 'bb'
getAutomataN1s :: Af
getAutomataN1s = ([0,1,2,3],['a','b','c'],[(0,'a',1),(0,'b',2),(0,'c',1),(1,'a',1),(1,'b',3),(1,'c',1),(2,'a',1 ),(2,'c',1),(3,'a',1),(3,'c',1)],0,[0,1,2,3])

-- AFND N2 para el lenguaje de las palabras que contienen 'aaa' o terminan en 'a' y, además, no contienen 'c'
getAutomataN2 :: Af
getAutomataN2 = ([0,1,2,3],['a','b','c'],[(0,'a',0),(0,'a',1),(0,'b',0),(1,'a',2),(2,'a',3),(3,'a',3),(3,'b',3)],0,[1,2,3])

-- AFND N3 para el lenguaje de las palabras que contienen 'aaa' o terminan en 'a' y, además, no contienen 'c'
getAutomataN3 :: Af
getAutomataN3 = ([0,1,2,3,4,5,6,7,8,9,10],['a','b','c'],[(0,'a',0),(0,'a',3),(0,'a',8),(0,'b',0),(0,'c',1),(1,'a',2),(1,'b',2),(1,'c',1 ),(1,'c',2),(2,'a',1),(2,'b',1),(2,'c',2),(3,'a',4),(3,'a',8),(3,'c',1),(4,'a',5 ),(4,'a',8),(4,'c',1),(5,'a',5),(5,'b',5),(6,'a',6),(6,'b',6),(6,'c',7),(7,'a',5 ),(7,'b',7),(9,'a',10),(10,'a',9)],0,[5,6,8,9])

-- AFND N3s para el lenguaje de las palabras que contienen 'aaa' o terminan en 'a' y, además, no contienen 'c'
getAutomataN3s :: Af
getAutomataN3s = ([0,3,4,5,8],['a','b','c'],[(0,'a',0),(0,'a',3),(0,'a',8),(0,'b',0),(3,'a',4),(3,'a',8),(4,'a',5),(4,'a',8 ),(5,'a',5),(5,'b',5)],0,[5,8])

-- AFND N4 para el lenguaje de las palabras que contienen 'aaa' o terminan en 'a' y, además, no contienen 'c'
getAutomataN4 :: Af
getAutomataN4 = ([0,1,2,3,4],['a','b','c'],[(0,'a',2),(2,'a',4),(3,'a',0),(3,'a',1),(3,'a',3),(3,'b',3),(4,'a',4),(4,'b',4)],3,[1,4])

-- AFND N5 para el lenguaje de las palabras que contienen 'aa' o terminan en 'a' y, además, no contienen 'c'
getAutomataN5 :: Af
getAutomataN5 = ([0,1,2,3],['a','b','c'],[(0,'a',0),(0,'a',1),(0,'a',3),(1,'a',2),(2,'a',2),(2,'b',2)],0,[2,3])

-- AFND N6 para el lenguaje de las palabras que solo contienen una 'a' o que terminan en 'a'
getAutomataN6 :: Af
getAutomataN6 = ([0,1,2,3],['a','b','c'],[(0,'a',1),(0,'a',2),(0,'b',0),(0,'b',2),(0,'c',0),(0,'c',2),(1,'b',1),(1,'c',1),(2,'a',2),(2,'a',3),(2,'b',2),(2,'c',2)],0,[1,3])

-- AFND N7 para el lenguaje ({a}({b}({a}U{b})*{a})*)+{c}*
getAutomataN7 :: Af
getAutomataN7 = ([0,1,2,3],['a','b','c'],[(0,'a',0),(0,'a',1),(1,'b',2),(1,'c',3),(2,'a',0),(2,'a',1),(2,'a',2),(2,'b',2),(3,'c',3)],0,[1,3])

------------------------------------------------------------------------
-- Autómatas finitos deterministas (AFD) obtenidos por transformación --
------------------------------------------------------------------------

-- AFD N6a para el lenguaje de las palabras que solo contienen una 'a' o que terminan en 'a'
getAutomataN6a :: Af
getAutomataN6a = ([0,1,2,3,4,5],['a','b','c'],[(0,'a',1),(0,'b',2),(0,'c',2),(1,'a',3),(1,'b',1),(1,'c',1),(2,'a',4),(2,'b',2),(2,'c',2),(3,'a',3),(3,'b',5),(3,'c',5),(4,'a',3),(4,'b',1),(4,'c',1),(5,'a',3),(5,'b',5),(5,'c',5)],0,[1,3,4])

-- AFD N7a para el lenguaje ({a}({b}({a}U{b})*{a})*)+{c}*
getAutomataN7a :: Af
getAutomataN7a = ([0,1,2,3,4,5],['a','b','c'],[(0,'a',2),(0,'b',1),(0,'c',1),(1,'a',1),(1,'b',1),(1,'c',1),(2,'a',2),(2,'b',3),(2,'c',4),(3,'a',5),(3,'b',3),(3,'c',1),(4,'a',1),(4,'b',1),(4,'c',4),(5,'a',5),(5,'b',3),(5,'c',4)],0,[2,4,5])

----------------------------
-- Autómatas finitos (AF) --
----------------------------

-- AF F1 para el lenguaje de las palabras que contienen 'aa' y terminan en 'b'
getAutomataF1 :: Af
getAutomataF1 = ([0,1,2,3],['a','b','c'],[(0,'a',1),(0,'b',0),(0,'c',0),(1,'a',2),(1,'b',0),(1,'c',0 ),(2,'a',2),(2,'b',3),(2,'c',2),(3,'a',2),(3,'b',3),(3,'c',2)],0,[3])

-- AF F2 para el lenguaje de las palabras que contienen 'aa' y terminan en 'b'
getAutomataF2 :: Af
getAutomataF2 = ([0,1,2,3],['a','b','c'],[(0,'a',0),(0,'a',1),(0,'b',0),(0,'c',0),(1,'a',2),(2,'a',2),(2,'b',2),(2,'b',3),(2,'c',2)],0,[3])

-- AF F3 para el lenguaje de las palabras que contienen solo dos apariciones de 'c', esas dos apariciones van juntas y, además, terminan en 'a'
getAutomataF3 :: Af
getAutomataF3 = ([0,1,2,3],['a','b','c'],[(0,'a',0),(0,'b',0),(0,'c',1),(1,'c',2),(2,'a',2),(2,'a',3),(2,'b',2)],0,[3])
