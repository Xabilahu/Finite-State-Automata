module Entregable2 where

import Tipos
import Pruebas

----------------------Análisis del coste computacional----------------------
-- El coste computacional del método determinista, es el número de estados multiplicado 
-- por el número de simbolos del alfabeto, O(estados*simbolos).

-- El coste computacional del método alcanzables en el peor de los casos es 
-- O(estados²*transiciones + estados*log₂(estados))
-- En el peor caso, todos los estados son alcanzables, por tanto, se van a recorrer
-- todos los estados del AF, y por cada uno se van a recorrer todas las transiciones
-- del AF, y por cada transición se va a recorrer la lista de examinados (en un principio,
-- dicha lista estará vacía y se irá incrementando hasta almacenar todos los estados, 
-- como promedio se recorrerán estados/2).

-- El coste computacional del método elim_repetidos es cuadrático con respecto al numero de 
-- elementos de la lista.

-- El coste computacional del método ms_rf es quasilinial con respecto número de elementos a ordenar.
-- Suponiendo que el número de elementos a ordenar es N, el coste sería O(N*log₂(N)).

-- El coste computacional del método aceptación es O(estados*alcanzables*estadosAceptación),
-- donde alcanzables es el coste computacional de la función alcanzables.

-- El coste computacional del método simplificación: 

-- Si es determinista, el coste sería O(alcanzables), porque el coste se calcula como la suma de calcular los nuevos estados,
-- las nuevas transiciones y los nuevos estados de aceptación. Como calcular las nuevas transiciones
-- y los nuevos estados de aceptación se consideran despreciables respecto a calcular los nuevos estados  
-- porque se hace una llamada a alcanzables), el coste lo fija esa llamada alcanzables.

-- En el caso de ser no determinista, el coste sería O(aceptación), pues el coste se calcula como la suma
-- de calcular la simplificación de un automata determinista, los nuevos estados en base a los que tendría un
-- automata determinista simplificado (se produce una llamada a aceptación), las nuevas transiciones
-- en base a las transiciones que tendría un autómata determinista simplificado, y los nuevos estados de aceptación
-- en base a los estados de aceptación que tendría un autómata determinista simplificado. Todos estos
-- componentes se consideran despreciables respecto a calcular los nuevos estados porque se hace una llamada
-- a aceptación.

----------------------------------------------------------------------------

determinista :: Af -> Bool
determinista (q,a,tau,sigma,y)
    | length tau == length [(qin,simb) | qin <- q, simb <- a] = True
    --Generamos una lista con tantos componentes como transiciones debería haber desde cada estado si fuera AFD (número de estados * número de símbolos en el alfabeto).
    --Si hay tantas transiciones en tau es AFD, sino es AFND.
    | otherwise = False

alcanzables :: Af -> Estado -> Estados
alcanzables (q,a,tau,sigma,y) estActual = ms_rf (alcanzables_aux (q,a,tau,sigma,y) estActual [] [] (elim_repetidos ([qf | (qin,simb,qf) <- tau, qin == estActual])))
--Se devuelve la lista ordenada de todos los estados que se pueden alcanzar desde es estado estActual.

alcanzables_aux :: Af -> Estado -> Estados -> Estados -> Estados -> Estados
alcanzables_aux (q,a,tau,sigma,y) estActual examinados resultado porExaminar
--Se utiliza la lista examinados para ir almacenando los estados de los que ya se han obtenido los alcanzables y no ciclar.
--Se utiliza la lista porExaminar para ir almacenando los estados de los que todavía no se han obtenido los alcanzables.
    | length porExaminar == 0 = elim_repetidos resultado
    --Cuando ya se han examinado todos los estados se devuelve la lista resultado sin repetidos.
    | otherwise = alcanzables_aux (q,a,tau,sigma,y) (head(porExaminar)) ((head porExaminar):examinados) (alcanzablesDesde ++ resultado) ((tail porExaminar) ++ alcanzablesDesde)
    --Si faltan estados por examinar, se toma el primer elemento de la lista porExaminar, se añade a examinados, se calculan todos los estados alcanzables desde el mismo y se añaden tanto a porExaminar como a resultado.
    where alcanzablesDesde = elim_repetidos [qf | (qin,simb,qf) <- tau, qin == estActual, qf `notElem` examinados]
    --alcanzablesDesde almacena la lista de estados que se alcanzan desde el estado actual y que no hayan sido examinados previamente (en caso de que ya hayan sido examinados ya estarán en la lista resultado).


elim_repetidos :: (Eq t) => [t] -> [t]
elim_repetidos [] = []
elim_repetidos (x:s)
    | x `elem` s = elim_repetidos s
    --Si x está repetido en el resto de la lista no lo añadimos a la lista resultado.
    | otherwise = x : elim_repetidos s
    --Si x no está repetido lo añadimos a la lista resultado.

aceptacion :: Af -> Estados
aceptacion (q,a,tau,sigma,y) = ms_rf (elim_repetidos (y ++ [x | x <- q, iterarEstAceptacion x (q,a,tau,sigma,y)]))
--Se devuelve la lista de todos los estados desde los cuales se puede alcanzar un estado de aceptación y los estados de aceptación.

iterarEstAceptacion :: Estado -> Af -> Bool
--Se crea una función auxiliar cuya función es dado un estado x y el Af , devuelve true si existe al menos un estado de aceptación alcanzable desde x.
--Esta función tiene como único objetivo calcular una única vez la lista de estados alcanzables desde x, de no implementar esta función, el coste sería mayor,
--pues se estaría calculando una vez por cada elemento de y.
iterarEstAceptacion x (q,a,tau,sigma,y) = length [z | z <- y, z `elem` alcanzablesDesde] > 0
    where alcanzablesDesde = alcanzables (q,a,tau,sigma,y) x

-- Método sacado de los apuntes de LCSI, mergesort ---------------
mezclar_aux::(Ord t, Eq t) => [t] -> [t] -> [t] -> [t]
mezclar_aux [] r q = q ++ r
mezclar_aux (x:s) r q | r == [] = q ++ (x:s)
    | x <= (head r) = mezclar_aux s r (q ++ [x])
    | otherwise = mezclar_aux (x:s) (tail r) (q ++ [head r])

mezclar_rf:: (Ord t, Eq t) => [t] -> [t] -> [t]
mezclar_rf r w = mezclar_aux r w []

ms_aux :: (Ord t, Eq t) => [[t]] -> [t]
ms_aux [] = []
ms_aux (x:s)
    | s == [] = x
    | otherwise = ms_aux (q : (tail s))
    where q = mezclar_rf x (head s)

ms_rf :: (Ord t, Eq t) => [t] -> [t]
ms_rf r = ms_aux [[y] | y <- r]
------------------------------------------------------------------

simplificacion :: Af -> Af
simplificacion (q,a,tau,sigma,y)
    | determinista (q,a,tau,sigma,y) = (nuevoQDet,a,nuevoTauDet,sigma,nuevoYDet)
    --En caso de ser un AFD, se eliminan aquellos estados que no son alcanzables desde el estado inicial y todas las transiciones correspondientes.
    | otherwise = (nuevoQNoDet,a,nuevoTauNoDet,sigma,nuevoYNoDet)
    --En caso de ser un AFND, además de eliminar todos los estados que no son alcanzables desde el estado inicial y todas las transiciones correspondientes,
    --se eliminan aquellos estados desde los cuales no es posible alcanzar un estado de aceptación.
    where
        nuevoQDet = elim_repetidos (sigma:(alcanzables (q,a,tau,sigma,y) sigma))
        --Lista de estados resultante de eliminar todos aquellos estados que no son alcanzables desde el estado inicial.
        nuevoTauDet = [(qin,simb,qf) | (qin,simb,qf) <- tau, (qin `elem` nuevoQDet && qf `elem` nuevoQDet)]
        --Lista de transiones resultante de eliminar aquellas que contienen algún estado no alcanzable desde el estado inicial.
        nuevoYDet = [qact | qact <- nuevoQDet, qact `elem` y]
        --Lista de estados de aceptación resultante de eliminar aquellos estados no alcanzables desde el estado inicial.
        estadosAlcAceptacion = aceptacion (q,a,tau,sigma,y)
        --Lista de estados desde los cuales es posible alcanzar un estado de aceptación
        nuevoQNoDet = elim_repetidos (sigma:[qact | qact <- nuevoQDet, qact `elem` estadosAlcAceptacion])
        --Lista de estados resultante de eliminar además de los no alcanzables desde el estado inicial, los estados desde los cuales no es posible alcanzar uno de aceptación.
        nuevoTauNoDet = [(qin,simb,qf) | (qin,simb,qf) <- nuevoTauDet, (qin `elem` nuevoQNoDet && qf `elem` nuevoQNoDet)]
        --Lista de transiciones resultante de eliminar además de la transiciones en las que aparece algún estado no alcanzable desde el estado inicial,
        --las transiciones en las que aparece algún estado desde el cual no es posible alcanzar uno de aceptación.
        nuevoYNoDet = [qact | qact <- nuevoQNoDet, qact `elem` nuevoYDet]
        --Lista de estados de aceptación resultante de eliminar además de quellos estados no alcanzables desde el estado inicial, los estados desde los cuales
        --no es posible alcanzar uno de aceptación.

casosDePrueba :: IO ()
casosDePrueba = do putStrLn "-----Pruebas método determinista-----\n" 
                   pruebaDet1
                   pruebaDet2
                   pruebaDet3
                   pruebaDet4
                   pruebaDet5
                   putStrLn "-----Pruebas método alcanzables-----\n"
                   pruebaAlc1
                   pruebaAlc2
                   pruebaAlc3
                   pruebaAlc4
                   pruebaAlc5
                   putStrLn "-----Pruebas método aceptación-----\n"
                   pruebaAcep1
                   pruebaAcep2
                   pruebaAcep3
                   pruebaAcep4
                   pruebaAcep5
                   putStrLn "-----Pruebas método simplificacion-----\n"
                   pruebaSimpl1
                   pruebaSimpl2
                   pruebaSimpl3

pruebaDet1 :: IO ()
pruebaDet1 = do putStrLn "-----Caso de prueba 1-----"
                if (determinista getAutomataD)
                    then putStrLn "Correcto, AFD D es determinista.\n\n"
                    else putStrLn "Incorrecto, AFD D es determinista.\n\n"

pruebaDet2 :: IO ()
pruebaDet2 = do putStrLn "-----Caso de prueba 2-----"
                if (determinista getAutomataD2)
                    then putStrLn "Correcto, AFD D2 es determinista.\n\n"
                    else putStrLn "Incorrecto, AFD D2 es determinista.\n\n"

pruebaDet3 :: IO ()
pruebaDet3 = do putStrLn "-----Caso de prueba 3-----"
                if (determinista getAutomataD3s)
                    then putStrLn "Correcto, AFD D3s es determinista.\n\n"
                    else putStrLn "Incorrecto, AFD D3s es determinista.\n\n"

pruebaDet4 :: IO ()
pruebaDet4 = do putStrLn "-----Caso de prueba 4-----"
                if (determinista getAutomataN)
                    then putStrLn "Incorrecto, AFND N no es determinista.\n\n"
                    else putStrLn "Correcto, AFND N no es determinista.\n\n"

pruebaDet5 :: IO ()
pruebaDet5 = do putStrLn "-----Caso de prueba 5-----"
                if (determinista getAutomataN3)
                    then putStrLn "Incorrecto, AFND N3 no es determinista.\n\n"
                    else putStrLn "Correcto, AFND N3 no es determinista.\n\n"

pruebaAlc1 :: IO ()
pruebaAlc1 = do putStrLn "-----Caso de prueba 1-----"
                if ([0,1] == (alcanzables getAutomataD 0))
                    then putStrLn "Correcto, los estados alcanzables desde el estado 0 del automataD son [0,1].\n\n"
                    else putStrLn "Incorrecto, los estados alcanzables desde el estado 0 del automataD son [0,1].\n\n"
                
pruebaAlc2 :: IO ()
pruebaAlc2 = do putStrLn "-----Caso de prueba 2-----"
                if ([3,4] == (alcanzables getAutomataD2 3))
                    then putStrLn "Correcto, los estados alcanzables desde el estado 3 del automataD2 son [3,4].\n\n"
                    else putStrLn "Incorrecto, los estados alcanzables desde el estado 3 del automataD2 son [3,4].\n\n"

pruebaAlc3 :: IO ()
pruebaAlc3 = do putStrLn "-----Caso de prueba 3-----"
                if ([3,4,9] == (alcanzables getAutomataD3s 3))
                    then putStrLn "Correcto, los estados alcanzables desde el estado 3 del automataD3 son [3,4,9].\n\n"
                    else putStrLn "Incorrecto, los estados alcanzables desde el estado 3 del automataD3 son [3,4,9].\n\n"

pruebaAlc4 :: IO ()
pruebaAlc4 = do putStrLn "-----Caso de prueba 4-----"
                if ([] == (alcanzables getAutomataN 1))
                    then putStrLn "Correcto, los estados alcanzables desde el estado 1 del automataN son [].\n\n"
                    else putStrLn "Incorrecto, los estados alcanzables desde el estado 1 del automataN son [].\n\n"

pruebaAlc5 :: IO ()
pruebaAlc5 = do putStrLn "-----Caso de prueba 5----"
                if ([1,2,4,5,8] == (alcanzables getAutomataN3 3))
                    then putStrLn "Correcto, los estados alcanzables desde el estado 3 del automataN3 son [1,2,4,5,8].\n\n"
                    else putStrLn "Incorrecto, los estados alcanzables desde el estado 3 del automataN3 son [1,2,4,5,8].\n\n"

pruebaAcep1 :: IO ()
pruebaAcep1 = do putStrLn "-----Caso de prueba 1----"
                 if ([0,1] == (aceptacion getAutomataD))
                    then putStrLn "Correcto, los estados desde los cuales se puede alcanzar un estado de aceptación del automataD son [0,1].\n\n"
                    else putStrLn "Incorrecto, los estados desde los cuales se puede alcanzar un estado de aceptación del automataD son [0,1].\n\n"

pruebaAcep2 :: IO ()
pruebaAcep2 = do putStrLn "-----Caso de prueba 2----"
                 if ([0,1,2,3] == (aceptacion getAutomataD2))
                    then putStrLn "Correcto, los estados desde los cuales se puede alcanzar un estado de aceptación del automataD2 son [0,1,2,3].\n\n"
                    else putStrLn "Incorrecto, los estados desde los cuales se puede alcanzar un estado de aceptación del automataD2 son [0,1,2,3].\n\n"

pruebaAcep3 :: IO ()
pruebaAcep3 = do putStrLn "-----Caso de prueba 3----"
                 if ([0,1,2,3] == (aceptacion getAutomataD3s))
                    then putStrLn "Correcto, los estados desde los cuales se puede alcanzar un estado de aceptación del automataD3s son [0,1,2,3].\n\n"
                    else putStrLn "Incorrecto, los estados desde los cuales se puede alcanzar un estado de aceptación del automataD3s son [0,1,2,3].\n\n"

pruebaAcep4 :: IO ()
pruebaAcep4 = do putStrLn "-----Caso de prueba 4----"
                 if ([0,1] == (aceptacion getAutomataN))
                    then putStrLn "Correcto, los estados desde los cuales se puede alcanzar un estado de aceptación del automataN son [0,1].\n\n"
                    else putStrLn "Incorrecto, los estados desde los cuales se puede alcanzar un estado de aceptación del automataN son [0,1].\n\n"

pruebaAcep5 :: IO ()
pruebaAcep5 = do putStrLn "-----Caso de prueba 5----"
                 if ([0,3,4,5,6,7,8,9,10] == (aceptacion getAutomataN3))
                    then putStrLn "Correcto, los estados desde los cuales se puede alcanzar un estado de aceptación del automataN3 son [0,3,4,5,6,7,8,9,10].\n\n"
                    else putStrLn "Incorrecto, los estados desde los cuales se puede alcanzar un estado de aceptación del automataN3 son [0,3,4,5,6,7,8,9,10].\n\n"

pruebaSimpl1 :: IO ()
pruebaSimpl1 = do putStrLn "-----Caso de prueba 1 (Automata D3)-----"
                  if (getAutomataD3s == (simplificacion getAutomataD3))
                    then putStrLn "Correcto\n\n"
                    else putStrLn "Incorrecto\n\n"                    

pruebaSimpl2 :: IO ()
pruebaSimpl2 = do putStrLn "-----Caso de prueba 2 (Automata N1)-----"
                  if (getAutomataN1s == (simplificacion getAutomataN1))
                    then putStrLn "Correcto\n\n"
                    else putStrLn "Incorrecto\n\n"

pruebaSimpl3 :: IO ()
pruebaSimpl3 = do putStrLn "-----Caso de prueba 3 (Automata N3)-----"
                  if (getAutomataN3s == (simplificacion getAutomataN3))
                    then putStrLn "Correcto\n\n"
                    else putStrLn "Incorrecto\n\n"