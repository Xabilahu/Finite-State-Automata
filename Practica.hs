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
                      procesar_respuesta_pert tupla
              2 -> do det <- pedirClasificacion
                      procesar_respuesta_clas det
              3 -> do af <- pedirSimplificacion
                      procesar_respuesta_simpl af
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
                       2 -> return "Parar"
                       otherwise -> do putStrLn "Opción incorrecta, se volverá a lanzar el submenú."
                                       submenu x y z

pedirPertenencia :: IO (Af,Bool)
pedirPertenencia = do putStr "\n\nIntroduzca el AF que define el lenguaje: "
                      afStr <- getLine
                      putStr "\nIntroduzca la palabra que quiera comprobar: "
                      palabraStr <- getLine
                      let af = read afStr :: Af
                      let palabra = read palabraStr :: Palabra
                      return (af,(pertenencia af palabra))

pedirClasificacion :: IO Bool
pedirClasificacion = do putStr "\n\nIntroduzca el AF que quiera clasificar: "
                        afStr <- getLine
                        let af = read afStr :: Af
                        return (determinista af)
                    
pedirSimplificacion :: IO Af
pedirSimplificacion = do putStr "\n\nIntroduzca el AF que quiera simplificar: "
                         afStr <- getLine
                         let af = read afStr :: Af
                         return (simplificacion af)

procesar_respuesta_pert :: (Af,Bool) -> IO ()
procesar_respuesta_pert (af,pertenece) = do
    if pertenece
        then putStrLn "\n\nPalabra perteneciente al lenguaje definido por el AF.\n\n"
        else putStrLn "\n\nPalabra no perteneciente al lenguaje definido por el AF.\n\n"
    subOpt <- submenu "pertenencia" "Comprobar la pertenencia de otra palabra al lenguaje asociado al AF." "\nIntroduzca la sigiente palabra a comprobar: "
    if subOpt == "Parar"
        then menu
        else do let nuevaPalabra = read subOpt :: Palabra
                procesar_respuesta_pert (af,(pertenencia af nuevaPalabra))

procesar_respuesta_clas :: Bool -> IO ()
procesar_respuesta_clas det = do
    if det
        then putStrLn "\n\nEl AF introducido es determinista.\n\n"
        else putStrLn "\n\nEl AF introducido no es determinista.\n\n"
    subOpt <- submenu "clasificación" "Clasificar otro AF." "\nIntroduzca el siguiente AF a clasificar: "
    if subOpt == "Parar"
        then menu
        else do let nuevoAF = read subOpt :: Af
                procesar_respuesta_clas (determinista nuevoAF)

procesar_respuesta_simpl :: Af -> IO ()
procesar_respuesta_simpl af = do
    let afStr = show af
    let texto = "\n\nEl AF simplificado es " ++ afStr ++ ".\n\n"
    putStrLn texto
    subOpt <- submenu "simplificación" "Simplificar otro AF." "\nIntroduzca el siguiente AF a simplificar: "
    if subOpt == "Parar"
        then menu
        else do let nuevoAF = read subOpt :: Af
                procesar_respuesta_simpl (simplificacion nuevoAF)

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
