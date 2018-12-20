import Tipos
import Pruebas
import Entregable2

automataCorrecto :: Af -> Bool
automataCorrecto (q,a,tau,sigma,y) = and ((sigma `elem` q):[x `elem` q | x <- y] ++ [qin `elem` q && simb `elem` a && qf `elem` q | (qin,simb,qf) <- tau])
-- Coste O(q*y + tau*(2q + a)) con respecto al número de transiciones, estados y símbolos del alfabeto.

transformacion :: Af -> Af
transformacion (q,a,tau,sigma,y)
    | not (automataCorrecto (q,a,tau,sigma,y)) = error "Autómata incorrecto, no es posible ejecutar la transformación."
    | determinista (q,a,tau,sigma,y) = (q,a,tau,sigma,y)
    | otherwise = transAFND_AFD (q,a,tau,sigma,y) [[sigma]] [[sigma]] [] [] []

transAFND_AFD :: Af -> [Estados] -> [Estados] -> [Estados] -> [(Estados,Simbolo,Estados)] -> [Estados] -> Af
transAFND_AFD (q,a,tau,sigma,y) porExaminar examinados nuevoQ nuevoTau nuevoY
    -- | not (automataCorrecto (q,a,tau,sigma,y)) = error "Autómata incorrecto, no es posible ejecutar la transformación."
    | length porExaminar == 0 && comprobarEstadoVacio nuevoTau = mapearAf ([[]] ++ nuevoQ, a, [([],'a',[]),([],'b',[]),([],'c',[])] ++ nuevoTau, sigma, [x | x <- nuevoY, x /= []]) (-1)
    | length porExaminar == 0 = mapearAf (nuevoQ,a,nuevoTau,sigma,[x | x <- nuevoY, x /= []]) 0
    | otherwise = transAFND_AFD (q,a,tau,sigma,y) ((tail porExaminar) ++ alcanzablesSinExaminar) (alcanzablesSinExaminar ++ examinados) (nuevoQ ++ [(head porExaminar)]) (nuevoTau ++ (zip3 [(head porExaminar) | x <- a] a alcanzablesDesde)) (nuevoY ++ [(esFinal y (head porExaminar))])
    where alcanzablesDesde = [ms_rf(elim_repetidos [ qf | (qin, alpha, qf) <- tau, simb == alpha, qin `elem` (head porExaminar)]) | simb <- a ]
    -- Primero se recorre el alfabeto, Por ejemplo:  (a , b , c) y por cada simbolo "x" se recorre la lista de transiciones
    -- en la que compruebas que el simbolo de cada transición corresponde con "x" y que qin pertenece a la lista del estado actual
    -- devolviendo el qf de de la trancisones que cumpla las condiciones mencionadas    
          alcanzablesSinExaminar = elim_repetidos [x | x <- alcanzablesDesde, x /= [], x `notElem` examinados]

esFinal :: Estados -> Estados -> Estados
esFinal y x
    | any (`elem` y) x = x
    | otherwise = []

mapearAf :: ([Estados],Alfabeto,[(Estados,Simbolo,Estados)],Estado,[Estados]) -> Int -> Af
mapearAf (nuevoQ,a,nuevoTau,sigma,nuevoY) idMenorEstado = (qFinal,a,tauFinal,sigma,yFinal)
    where hashMap =  zip qFinal nuevoQ
          qFinal = [toInteger x | x <- [idMenorEstado..((length nuevoQ) - 1 + idMenorEstado)]]
          tauInicial = [(x, simb, qf) | (qin,simb,qf) <- nuevoTau, (x,y) <- hashMap, qin == y]
          tauFinal = [(qin,simb,x) | (qin,simb,qf) <- tauInicial, (x,y) <- hashMap, qf == y]
          yFinal = [x | q <- nuevoY, (x,y) <- hashMap, q == y]

comprobarEstadoVacio :: [(Estados,Simbolo,Estados)] -> Bool
comprobarEstadoVacio nuevoTau = or([qf == [] | ( qin, simb, qf) <- nuevoTau])

--Casos de prueba
--([0,1,2,3],"abc",[(0,'a',1),(0,'a',2),(0,'b',0),(0,'b',2),(0,'c',0),(0,'c',2),(1,'b',1),(1,'c',1),(2,'a',2),(2,'a',3),(2,'b',2),(2,'c',2)],0,[1,3])
--([0,1],"abc",[(0,'a',1)],0,[1])
--([0,1,2,3],"abc",[(0,'a',0),(0,'a',1),(1,'a',2),(1,'b',1),(1,'b',2),(1,'c',3),(2,'a',0),(2,'a',1),(3,'c',3)],0,[3])

--Resultados
--([0,1,2,3,4,5],"abc",[(0,'a',1),(0,'b',2),(0,'c',2),(1,'a',3),(1,'b',1),(1,'c',1),(2,'a',4),(2,'b',2),(2,'c',2),(3,'a',3),(3,'b',5),(3,'c',5),(4,'a',3),(4,'b',1),(4,'c',1),(5,'a',3),(5,'b',5),(5,'c',5)],0,[1,3,4])
--([-1,0,1],"abc",[(-1,'a',-1),(-1,'b',-1),(-1,'c',-1),(0,'a',1),(0,'b',-1),(0,'c',-1),(1,'a',-1),(1,'b',-1),(1,'c',-1)],0,[1])
--([-1,0,1,2,3,4],"abc",[(-1,'a',-1),(-1,'b',-1),(-1,'c',-1),(0,'a',1),(0,'b',-1),(0,'c',-1),(1,'a',2),(1,'b',3),(1,'c',4),(2,'a',2),(2,'b',3),(2,'c',4),(3,'a',2),(3,'b',3),(3,'c',4),(4,'a',-1),(4,'b',-1),(4,'c',4)],0,[4])