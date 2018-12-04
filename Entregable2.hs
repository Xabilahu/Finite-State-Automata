import Tipos
import Pruebas

determinista :: Af -> Bool
determinista (q,a,tau,sigma,y)
    | length tau == length [(qin,simb) | qin <- q, simb <- a] = True
    --Generamos una lista con tantos componentes como transiciones debería haber desde cada estado si fuera AFD (tantas como simbolos contenga el alfabeto), si hay tantas transiciones en tau
    --es AFD, sino es AFND.
    | otherwise = False

alcanzables :: Af -> Estado -> Estados
alcanzables (q,a,tau,sigma,y) estActual = alcanzables_aux (q,a,tau,sigma,y) estActual [] [] [estActual]
--Se devuelve la lista con aquellos estados que aparecen como último elemento en las tuplas de tau en los que el primer elemento es estActual, eliminando aquellos estados que aparecen repetidos

alcanzables_aux :: Af -> Estado -> Estados -> Estados -> Estados -> Estados
alcanzables_aux (q,a,tau,sigma,y) estActual examinados resultado porExaminar
    | length porExaminar == 0 = elim_repetidos resultado
    | otherwise = alcanzables_aux (q,a,tau,sigma,y) (head(porExaminar)) ((head porExaminar):examinados) (alcanzablesDesde ++ resultado) ((tail porExaminar) ++ alcanzablesDesde)
    where alcanzablesDesde = elim_repetidos [qf | (qin,simb,qf) <- tau, qin == estActual, qf `notElem` examinados]

elim_repetidos :: (Eq t) => [t] -> [t]
elim_repetidos [] = []
elim_repetidos (x:s)
    | x `elem` s = elim_repetidos s
    --Si x está repetido en el resto de la lista no lo añadimos a la lista resultado
    | otherwise = x : elim_repetidos s
    --Si x no está repetido lo añadimos a la lista resultado

aceptacion :: Af -> Estados
aceptacion (q,a,tau,sigma,y) = [x | x <- q, iterarEstAceptacion x (q,a,tau,sigma,y)] 
--Se devuelve la lista con aquellos estados que aparecen como primer elemento en las tuplas de tau en los que el último elemento pertenece a y (estados de aceptación)

--([0,1,2,3,4],"abc",[(0,'a',1),(0,'a',2),(1,'b',3),(1,'c',4)],0,[4])

iterarEstAceptacion :: Estado -> Af -> Bool
--Se crea una función auxiliar cuya función es dado un estado x y el Af , devuelve true si existe al menos un estado de aceptación alcanzable desde x
iterarEstAceptacion x (q,a,tau,sigma,y) = length [z | z <- y, z `elem` alcanzablesDesde] > 0
    where alcanzablesDesde = alcanzables (q,a,tau,sigma,y) x

simplificacion :: Af -> Af
simplificacion (q,a,tau,sigma,y)
    | determinista (q,a,tau,sigma,y) = (nuevoQDet,a,nuevoTauDet,sigma,nuevoYDet)
    | otherwise = (nuevoQNoDet,a,nuevoTauNoDet,sigma,nuevoYNoDet)
    where
        nuevoQDet = elim_repetidos (sigma:(alcanzables (q,a,tau,sigma,y) sigma))
        nuevoTauDet = [(qin,simb,qf) | (qin,simb,qf) <- tau, (qin `elem` nuevoQDet && qf `elem` nuevoQDet)]
        nuevoYDet = [qact | qact <- nuevoQDet, qact `elem` y]
        estadosAlcAceptacion = aceptacion (q,a,tau,sigma,y)
        nuevoQNoDet = elim_repetidos (sigma:[qact | qact <- nuevoQDet, qact `elem` estadosAlcAceptacion])
        nuevoTauNoDet = [(qin,simb,qf) | (qin,simb,qf) <- nuevoTauDet, (qin `elem` nuevoQNoDet && qf `elem` nuevoQNoDet)]
        nuevoYNoDet = [qact | qact <- nuevoQNoDet, qact `elem` nuevoYDet]