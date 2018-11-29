import Pruebas
import Tipos

menu :: IO ()
menu = do putStrLn "\n***Menú Principal***"
          putStrLn "Elige una opción:"
          putStrLn "1. Pertenencia de una palabra al lenguaje asociado a un AF."
          putStrLn "2. Clasificación de un AF como AFD o AFND."
          putStrLn "3. Simplificación de un AF."
          putStrLn "4. Transformación de un AF en AFD."
          putStrLn "5. Terminar."
          putStr "\n   Opción: "
          opcionStr <- getLine
          let opcion = read opcionStr :: Integer
          case opcion of
              1 -> do tupla <- pedirPertenencia
                      procesar_respuesta tupla
              2 -> putStrLn "Funcionalidad a implementar en el entregable II."
              3 -> putStrLn "Funcionalidad a implementar en el entregable II."
              4 -> putStrLn "Funcionalidad a implementar en el entregable III."
              5 -> return ()
              otherwise -> do putStrLn "Opción incorrecta, se volverá a lanzar el menú.\n"
                              menu

submenu :: String -> String -> String -> IO String
submenu x y z = do putStrLn ("\n***Submenú de " ++ x ++ "***")
                   putStrLn "Elige una opción: "
                   putStrLn ("1. " ++ y)
                   putStrLn "2. Volver al menú principal."
                   putStr "\n   Opción: "
                   opcionStr <- getLine
                   let opcion = read opcionStr :: Integer
                   case opcion of
                       1 -> do putStr z
                               getLine
                       2 -> return "No más palabras."
                       otherwise -> do putStrLn "Opción incorrecta, se volverá a lanzar el submenú."
                                       submenu x y z

pedirPertenencia :: IO (Af,Palabra,Bool)
pedirPertenencia = do putStr "\n\nIntroduzca el AF que define el lenguaje: "
                      afStr <- getLine
                      putStr "\nIntroduzca la palabra que quiera comprobar: "
                      palabraStr <- getLine
                      let af = read afStr :: Af
                      let palabra = read palabraStr :: Palabra
                      return (af,palabra,(pertenencia af palabra))

procesar_respuesta :: (Af,Palabra,Bool) -> IO ()
procesar_respuesta (af,palabra,pertenece) = do
    if pertenece
        then putStrLn "\n\nPalabra perteneciente al lenguaje definido por el AF.\n\n"
        else putStrLn "\n\nPalabra no perteneciente al lenguaje definido por el AF.\n\n"
    subOpt <- submenu ("pertenencia") ("Comprobar la pertenencia de otra palabra al lenguaje asociado al AF.") ("\nIntroduzca la sigiente palabra a comprobar: ")
    if subOpt == "No más palabras."
        then menu
        else do let nuevaPalabra = read subOpt :: Palabra
                let nuevoPertenece = pertenencia af nuevaPalabra
                procesar_respuesta (af,nuevaPalabra,nuevoPertenece)

pertenencia :: Af -> Palabra -> Bool
pertenencia (q,a,tau,sigma,y) word = pertenencia_aux (q,a,tau,sigma,y) word sigma []
--La lista vacía representa los estados que quedan por examinar en caso de computar un AFND, con un AFD esa lista permanecerá vacía

pertenencia_aux :: Af -> Palabra -> Estado -> [(Estado,Palabra)] -> Bool
pertenencia_aux (q,a,tau,sigma,y) word estActual porExaminar
    | length word == 0 && estActual `elem` y = True
    --Si la palabra es vacía y estamos en un estado de aceptación devolvemos True
    | length word == 0 && length porExaminar > 0 = pertenencia_aux (q,a,tau,sigma,y) (snd(head porExaminar)) (fst(head porExaminar)) (tail porExaminar)
    --Si la palabra es vacía y quedan elementos por examinar, comprobamos si ese elemento devuelve True volviendo a llamar a pertenencia_aux
    | length word == 0 = False
    --En este punto la palabra es vacía, no quedan elementos por examinar, y no estamos en un estado de aceptación, devolvemos False
    | (head word) `notElem` a = error "La palabra contiene algún símbolo no perteneciente al alfabeto."
    --Si el símbolo que vamos a comprobar no pertenece al alfabeto, lanzamos un error (no lo lanzamos antes, pues la palabra puede ser vacía y con head se lanza una interrupción)
    | length lista > 0 = pertenencia_aux (q,a,tau,sigma,y) (tail word) (head lista) (porExaminar ++ (zip (tail lista) ([(tail word) | x <- [1..(length (tail lista))]])))
    --Si hay algún estado al que transicionar, transicionamos y guardamos los demás junto con la palabra que nos queda por computar
    | length porExaminar > 0 = pertenencia_aux (q,a,tau,sigma,y) (snd(head porExaminar)) (fst(head porExaminar)) (tail porExaminar)
    --Si no hay estados a los que transicionar pero quedan estados por comprobar, comprobamos uno de ellos
    | otherwise = False
    --Si no hay ni estados a los que transicionar ni estados por examinar, devolvemos False
    where
        lista = [qf | (qin,simb,qf) <- tau, (qin == estActual && simb == (head word))]
            --lista contiene los estados a los que se puede transicionar desde estActual con head word

--AFD prueba: lenguaje palabras que empiezan por 'a' y terminan en 'bc'
--([0,1,2,3,4],"abc",[(0,'a',1),(0,'b',4),(0,'c',4),(1,'a',1),(1,'b',2),(1,'c',1),(2,'a',1),(2,'b',2),(2,'c',3),(3,'a',1),(3,'b',2),(3,'c',1),(4,'a',4),(4,'b',4),(4,'c',4)],0,[3])
--AFD prueba: lenguaje palabras que contienen exactamente dos "a" o un número par de "b"
--([0,1,2,3,4,5,6,7],"abc",[(0,'a',1),(0,'b',4),(0,'c',0),(1,'a',2),(1,'b',5),(1,'c',1),(2,'a',3),(2,'b',6),(2,'c',2),(3,'a',3),(3,'b',7),(3,'c',3),(4,'a',5),(4,'b',1),(4,'c',4),(5,'a',6),(5,'b',1),(5,'c',5),(6,'a',7),(6,'b',2),(6,'c',6),(7,'a',7),(7,'b',3),(7,'c',7)],0,[0,1,2,3,6])
--AFND prueba: lenguaje palabras que contienen exactamente dos "a" o un número par de "b"
--([0,1,2,3,4,5,6,7,8],"abc",[(0,'a',8),(0,'a',1),(0,'b',4),(0,'c',0),(1,'a',2),(1,'b',5),(1,'c',1),(2,'a',3),(2,'b',6),(2,'c',2),(3,'a',3),(3,'b',7),(3,'c',3),(4,'a',5),(4,'b',1),(4,'c',4),(5,'a',6),(5,'b',1),(5,'c',5),(6,'a',7),(6,'b',2),(6,'c',6),(7,'a',7),(7,'b',3),(7,'c',7)],0,[0,1,2,3,6])
--AFND prueba: lenguaje de las palabras que contienen aa pero no c.
--([0,1,2],"abc",[(0,'a',0),(0,'b',0),(0,'a',1),(1,'a',2),(2,'a',2),(2,'b',2)],0,[2])
--AFD prueba (a impar) simplificacion resultado -> ([0,1],"ab",[(0,'a',1),(0,'b',0),(1,'a',0),(1,'b',1)],0,[1])
--([0,1,2],"ab",[(0,'a',1),(0,'b',0),(1,'a',2),(1,'b',1),(2,'a',1),(2,'b',2)],0,[1])

determinista :: Af -> Bool
determinista (q,a,tau,sigma,y)
    | length tau == length [(qin,simb) | qin <- q, simb <- a] = True
    --Generamos una lista con tantos componentes como transiciones debería haber desde cada estado si fuera AFD (tantas como simbolos contenga el alfabeto), si hay tantas transiciones en tau
    --es AFD, sino es AFND.
    | otherwise = False

alcanzables :: Af -> Estado -> Estados
alcanzables (q,a,tau,sigma,y) estActual = elim_repetidos [qf | (qin,simb,qf) <- tau, qin == estActual]
--Se devuelve la lista con aquellos estados que aparecen como último elemento en las tuplas de tau en los que el primer elemento es estActual, eliminando aquellos estados que aparecen repetidos

elim_repetidos :: (Eq t) => [t] -> [t]
elim_repetidos [] = []
elim_repetidos (x:s)
    | x `elem` s = elim_repetidos s
    --Si x está repetido en el resto de la lista no lo añadimos a la lista resultado
    | otherwise = x : elim_repetidos s
    --Si x no está repetido lo añadimos a la lista resultado

aceptacion :: Af -> Estados
aceptacion (q,a,tau,sigma,y) = elim_repetidos [qin | (qin,simb,qf) <- tau, qf `elem` y]
--Se devuelve la lista con aquellos estados que aparecen como primer elemento en las tuplas de tau en los que el último elemento pertenece a y (estados de aceptación)

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
