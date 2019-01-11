-- import Pruebas
import Tipos

menu :: IO ()
--Menu principal
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
              4 -> do af <- pedirTransformacion
                      procesar_respuesta_trans af
              5 -> return ()
              otherwise -> do putStrLn "Opción incorrecta, se volverá a lanzar el menú.\n"
                              menu

submenu :: String -> String -> String -> IO String
--"x" es el título del menú, "y" es el mensaje de la primera opción del submenu y "z" es el mensaje para volver a introducir otro dato.
--La finalidad de los tres parámetros de entrada es no tener código repetido para los distintos submenus de cada operación.
--Esta función devuelve el dato intoducido por el usuario, en caso de no querer introducir nada más devuelve "Parar".
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
--Devuelve como tupla tanto el Af como el resultado de la función pertenencia
pedirPertenencia = do putStr "\n\nIntroduzca el AF que define el lenguaje: "
                      afStr <- getLine
                      putStr "\nIntroduzca la palabra que quiera comprobar: "
                      palabraStr <- getLine
                      let af = read afStr :: Af
                      let palabra = read palabraStr :: Palabra
                      return (af,(pertenencia af palabra))

pedirClasificacion :: IO Bool
--Se devuelve el resultado de la función determinista
pedirClasificacion = do putStr "\n\nIntroduzca el AF que quiera clasificar: "
                        afStr <- getLine
                        let af = read afStr :: Af
                        return (determinista af)
                    
pedirSimplificacion :: IO Af
--Se devuelve el resultado de la función simplificación
pedirSimplificacion = do putStr "\n\nIntroduzca el AF que quiera simplificar: "
                         afStr <- getLine
                         let af = read afStr :: Af
                         return (simplificacion af)

pedirTransformacion :: IO Af
--Se devuelve el resultado de la función transformación
pedirTransformacion = do putStr "\n\nIntroduzca el AF que quiera transformar en AFD: "
                         afStr <- getLine
                         let af = read afStr :: Af
                         return (transformacion af)

procesar_respuesta_pert :: (Af,Bool) -> IO ()
--Muestra por pantalla el resultado y vuelve a pedir otra operación
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
--Muestra por pantalla el resultado y vuelve a pedir otra operación
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
--Muestra por pantalla el resultado y vuelve a pedir otra operación
procesar_respuesta_simpl af = do
    let afStr = show af
    let texto = "\n\nEl AF simplificado es " ++ afStr ++ ".\n\n"
    putStrLn texto
    subOpt <- submenu "simplificación" "Simplificar otro AF." "\nIntroduzca el siguiente AF a simplificar: "
    if subOpt == "Parar"
        then menu
        else do let nuevoAF = read subOpt :: Af
                procesar_respuesta_simpl (simplificacion nuevoAF)

procesar_respuesta_trans :: Af -> IO ()
--Muestra por pantalla el resultado y vuelve a pedir otra operación
procesar_respuesta_trans af = do
    let afStr = show af
    let texto = "\n\nEl AF transformado es " ++ afStr ++ ".\n\n"
    putStrLn texto
    subOpt <- submenu "transformación" "Transformar otro AF." "\nIntroduzca el siguiente AF a transformar: "
    if subOpt == "Parar"
        then menu
        else do let nuevoAF = read subOpt :: Af
                procesar_respuesta_trans (transformacion nuevoAF)

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