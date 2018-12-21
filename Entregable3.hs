import Tipos
import Pruebas
import Entregable2

-----------ACLARACIÓN: EL ESTADO VACÍO SE REPRESENTA MEDIANTE UN -1-----------

------------------------------ANÁLISIS DEL COSTE------------------------------
--El coste de la operación simplificación para un AFD es constante O(1), pues 
--directamente se  devuelve dicho autómata.

--Para un AFND el coste en el peor caso sería O(combinaciones estados), pues en el peor caso
--el AFD equivalente podría tener todas las posibles combinaciones de los estados
--que conforman el AFND. Como la operación de simplificación hace recursión
--sobre dichos estados, concluimos que es lineal con respecto al número de
--combinaciones de los estados componentes de AFND.
------------------------------------------------------------------------------

automataCorrecto :: Af -> Bool
automataCorrecto (q,a,tau,sigma,y) = and ((sigma `elem` q):[x `elem` q | x <- y] ++ [qin `elem` q && simb `elem` a && qf `elem` q | (qin,simb,qf) <- tau])
-- Coste O(q*y + tau*(2q + a)) con respecto al número de transiciones, estados y símbolos del alfabeto.

transformacion :: Af -> Af
transformacion (q,a,tau,sigma,y)
    | not (automataCorrecto (q,a,tau,sigma,y)) = error "Autómata incorrecto, no es posible ejecutar la transformación."
    --Si el autómata es incorrecto se manda un mensaje de error
    | determinista (q,a,tau,sigma,y) = (q,a,tau,sigma,y)
    --Si el autómata es determinista directamente se devuelve el autómata
    | otherwise = transAFND_AFD (q,a,tau,sigma,y) [[sigma]] [[sigma]] [] [] []
    --Sino se devuelve el autómata transformado

transAFND_AFD :: Af -> [Estados] -> [Estados] -> [Estados] -> [(Estados,Simbolo,Estados)] -> [Estados] -> Af
transAFND_AFD (q,a,tau,sigma,y) porExaminar examinados nuevoQ nuevoTau nuevoY
    | not (automataCorrecto (q,a,tau,sigma,y)) = error "Autómata incorrecto, no es posible ejecutar la transformación."
    --Si el autómata es incorrecto se manda un mensaje de error
    | length porExaminar == 0 && comprobarEstadoVacio nuevoTau = mapearAf ([[]] ++ nuevoQ, a, [([],'a',[]),([],'b',[]),([],'c',[])] ++ nuevoTau, sigma, [x | x <- nuevoY, x /= []]) (-1)
    --Si desde algún estado se llega al estado vacío, se añade y se renombran los estados
    | length porExaminar == 0 = mapearAf (nuevoQ,a,nuevoTau,sigma,[x | x <- nuevoY, x /= []]) 0
    --Si no aparece el estado vacío directamente se renombran los estados
    | otherwise = transAFND_AFD (q,a,tau,sigma,y) ((tail porExaminar) ++ alcanzablesSinExaminar) (alcanzablesSinExaminar ++ examinados) (nuevoQ ++ [(head porExaminar)]) (nuevoTau ++ (zip3 [(head porExaminar) | x <- a] a alcanzablesDesde)) (nuevoY ++ [(esFinal y (head porExaminar))])
    --Si todavía quedan estados por examinar se generan los nuevos estados que se alcanzan desde el estado actual y se añaden a porEaminar y a examinados
    --Además, el estado actual pasará a ser parte de la solución, por tanto se va construyendo la nueva lista de estados, la nueva lista de transiciones y
    --de estados de aceptación
    where alcanzablesDesde = [ms_rf(elim_repetidos [ qf | (qin, alpha, qf) <- tau, simb == alpha, qin `elem` (head porExaminar)]) | simb <- a ]
          -- Primero se recorre el alfabeto, Por ejemplo:  (a , b , c) y por cada simbolo "x" se recorre la lista de transiciones
          -- en la que compruebas que el símbolo de cada transición corresponde con "x" y que qin pertenece a la lista del estado actual
          -- devolviendo el qf de las transiciones que cumplan las condiciones mencionadas.
          --Los componentes de cada estado se ordenan y se eliminan los repetidos para que no aparezcan estados del tipo: [0,1] [1,1,0]
          --pues ambos estados hacen referencia al mismo. 
          alcanzablesSinExaminar = elim_repetidos [x | x <- alcanzablesDesde, x /= [], x `notElem` examinados]
          --Se eliminan los estados repetidos para no añadirlos varias veces a porExaminar ni a examinados

esFinal :: Estados -> Estados -> Estados
esFinal y x
    | any (`elem` y) x = x
    --Si algún elemento de x perteneciente a los estados de aceptación se devuelve x
    | otherwise = []
    --Sino se devuelve lista vacía

mapearAf :: ([Estados],Alfabeto,[(Estados,Simbolo,Estados)],Estado,[Estados]) -> Int -> Af
mapearAf (nuevoQ,a,nuevoTau,sigma,nuevoY) idMenorEstado = (qFinal,a,tauFinal,sigma,yFinal)
--idMenorEstado será -1 si hay estado vacío y 0 en caso contrario
    where hashMap =  zip qFinal nuevoQ
          --A cada estado se le asigna un identificador
          qFinal = [toInteger x | x <- [idMenorEstado..((length nuevoQ) - 1 + idMenorEstado)]]
          --Los id se convierten a Integer para que cuadre con la definición de tipos de Estado
          tauInicial = [(x, simb, qf) | (qin,simb,qf) <- nuevoTau, (x,y) <- hashMap, qin == y]
          --Se van modificando los estados por sus respectivos id en las transiciones
          tauFinal = [(qin,simb,x) | (qin,simb,qf) <- tauInicial, (x,y) <- hashMap, qf == y]
          --Se modifican los estados que quedaban por modificar de la lista de transiciones
          yFinal = [x | q <- nuevoY, (x,y) <- hashMap, q == y]
          --Se modifican los estados de aceptación por sus id correspondientes

comprobarEstadoVacio :: [(Estados,Simbolo,Estados)] -> Bool
comprobarEstadoVacio nuevoTau = or([qf == [] | ( qin, simb, qf) <- nuevoTau])
--Se genera una lista de booleanos (True si qf es un estado vacío y False sino), con que algún elemento sea True se devuelve True

testCases :: IO()
testCases = do putStrLn "-------Pruebas AFD-------"
               caso1
               caso2
               putStrLn "-------Pruebas AFND-------"
               caso3
               caso4

caso1 :: IO()
caso1 = do putStrLn "-------Caso de Prueba 1-------"
           if (transformacion getAutomataD == getAutomataD)
                then putStrLn "Correcto, se ha transformado el autómata correctamente.\n\n"
                else putStrLn "Incorrecto, no se ha transformado correctamente el autómata.\n\n"

caso2 :: IO()
caso2 = do putStrLn "-------Caso de Prueba 1-------"
           if (transformacion getAutomataD2 == getAutomataD2)
                then putStrLn "Correcto, se ha transformado el autómata correctamente.\n\n"
                else putStrLn "Incorrecto, no se ha transformado correctamente el autómata.\n\n"

caso3 :: IO()
caso3 = do putStrLn "-------Caso de Prueba 3-------"
           let automata = ([0,1],"abc",[(0,'a',1)],0,[1])
           let automataTransformado = ([-1,0,1],"abc",[(-1,'a',-1),(-1,'b',-1),(-1,'c',-1),(0,'a',1),(0,'b',-1),(0,'c',-1),(1,'a',-1),(1,'b',-1),(1,'c',-1)],0,[1])
           if (transformacion automata == automataTransformado)
                then putStrLn "Correcto, se ha transformado el autómata correctamente.\n\n"
                else putStrLn "Incorrecto, no se ha transformado correctamente el autómata.\n\n"

caso4 :: IO()
caso4 = do putStrLn "-------Caso de Prueba 4-------"
           let automata = ([0,1,2,3],"abc",[(0,'a',0),(0,'a',1),(1,'a',2),(1,'b',1),(1,'b',2),(1,'c',3),(2,'a',0),(2,'a',1),(3,'c',3)],0,[3])
           let automataTransformado = ([-1,0,1,2,3,4],"abc",[(-1,'a',-1),(-1,'b',-1),(-1,'c',-1),(0,'a',1),(0,'b',-1),(0,'c',-1),(1,'a',2),(1,'b',3),(1,'c',4),(2,'a',2),(2,'b',3),(2,'c',4),(3,'a',2),(3,'b',3),(3,'c',4),(4,'a',-1),(4,'b',-1),(4,'c',4)],0,[4])
           if (transformacion automata == automataTransformado)
                then putStrLn "Correcto, se ha transformado el autómata correctamente.\n\n"
                else putStrLn "Incorrecto, no se ha transformado correctamente el autómata.\n\n"