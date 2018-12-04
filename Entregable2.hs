import Tipos
import Pruebas

----------------------Análisis del coste computacional----------------------
-- 
----------------------------------------------------------------------------

determinista :: Af -> Bool
determinista (q,a,tau,sigma,y)
    | length tau == length [(qin,simb) | qin <- q, simb <- a] = True
    --Generamos una lista con tantos componentes como transiciones debería haber desde cada estado si fuera AFD (número de estados * número de símbolos en el alfabeto).
    --Si hay tantas transiciones en tau es AFD, sino es AFND.
    | otherwise = False

alcanzables :: Af -> Estado -> Estados
alcanzables (q,a,tau,sigma,y) estActual = alcanzables_aux (q,a,tau,sigma,y) estActual [] [] [estActual]
--Se devuelve la lista de todos los estados que se pueden alcanzar desde es estado estActual.

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
aceptacion (q,a,tau,sigma,y) = [x | x <- q, iterarEstAceptacion x (q,a,tau,sigma,y)] 
--Se devuelve la lista de todos los estados desde los cuales se puede alcanzar un estado de aceptación.

--([0,1,2,3,4],"abc",[(0,'a',1),(0,'a',2),(1,'b',3),(1,'c',4)],0,[4])

iterarEstAceptacion :: Estado -> Af -> Bool
--Se crea una función auxiliar cuya función es dado un estado x y el Af , devuelve true si existe al menos un estado de aceptación alcanzable desde x.
--Esta función tiene como único objetivo calcular una única vez la lista de estados alcanzables desde x, de no implementar esta función, el coste sería mayor,
--pues se estaría calculando una vez por cada elemento de y.
iterarEstAceptacion x (q,a,tau,sigma,y) = length [z | z <- y, z `elem` alcanzablesDesde] > 0
    where alcanzablesDesde = alcanzables (q,a,tau,sigma,y) x

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
                   pruebaSimpl4
                   pruebaSimpl5
                   pruebaSimpl6

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
                
pruebaAlc2 :: IO ()
pruebaAlc2 = do putStrLn "-----Caso de prueba 2-----"

pruebaAlc3 :: IO ()
pruebaAlc3 = do putStrLn "-----Caso de prueba 3-----"

pruebaAlc4 :: IO ()
pruebaAlc4 = do putStrLn "-----Caso de prueba 4-----"

pruebaAlc5 :: IO ()
pruebaAlc5 = do putStrLn "-----Caso de prueba 5-----"