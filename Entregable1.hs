import Tipos
import Pruebas

----------------------Análisis del coste computacional----------------------
-- El coste computacional para los AFD's es lineal con respecto a la longitud de la palabra.

-- El coste computacional para los AFND's es exponencial con respecto al número de estados 
-- elevado a la longitud de la palabra (O(estados^|w|)).
-- Para calcular el coste en los AFND's hemos considerado el peor de los casos: aquel AFND
-- del que desde cada estado existe una transición a todos los estados con cada símbolo.

-- El número de comprobaciones va a ser el número de estados elevado a la longitud de la palabra.
-- Por ejemplo, en este caso (el diagrama de abajo), en el que el número de estados es 3 y la longitud
-- de la palabra (|W|) también, el coste sería 3^3.



--                                                                                                                                                   `......                                                                                                                                                     /|\ 
--                                                                                                                                                 `.-      :``                                                                                                                                                   |
--                                                                                                                                   ``............``:      : `..............`                                                                                                                                    |
--                                                                                                                     ``............``               ......                 `..............`                                                                                                                     |
--                                                                                                       ``............``                               :                                   ``.............``                                                                                                     |
--                                                                                         ``............``                                             :                                                  ``.............``                                                                                      |
--                                                                           ``............``                                                           :                                                                 ``.............``                                                                       |
--                                                             ``............``                                                                        .:..                                                                              ``.............``  ....                                                  |
--                                              `.....-........``                                                                                    -.    ..                                                                                           ``:.    ..                                                |
--                                             `-      :                                                                                            `-      :                                                                                             :      :                                                |
--                                             `-      :                                                                                             :`    .:                                                                                             :`    .:                                                |
--                                           `..-.....-...`                                                                                     .....``.:..  .....`                                                                                 `.....``.:..  ......`                                         |
--                                     `.....`     :      `......                                                                          .....        :         `.....                                                                       .....`        -`         `.....`                                   | |W|
--                                .....`           :             ......`                                                             `....`             :               `.....                                                           ......              -`               `.....`                             |
--                          ......                 :                   `......                                                  `....`                  :                     .....`                                               `.....                   .:.`                    `......   ...`                |
--              ....` ......                     `.:..                        ....-.....`                            ....` .....`                    `......                       `...-......                           ......-...`                      ..   `..                         .:.   `-`              |
--            ..    `:                          -`    ..                         -`     `.                         ..    `:                          :      :                         ..     `-                         -      -`                         :      :                         -`      :              |
--            :      :                         `-      :                         -`     `-                         :      -`                         -      :                         -`     `-                         :      -`                         -`    .-                         `-     -`              |
--            ..    .-                          -.    -.                          :.....:                          .-    .-                          .-....-.                          :.....:                           :.....-                          -..-..`-                          :..-....              |
--            -`..:. -`                        `-`.:.``-                         -`  :   -                         -...:.`-`                        ..  :   ..                        -`  :   -                         -   :  `-                        -`  -`  `-                        :   :   ..             |
--           -`   :   ..                      `-   :    -`                      -`   :    -`                      -`   :   ..                      ..   :    ..                      -`   :    -`                     `-    :   `-                      -`   ..   `-                      :    :    ..            |
--         `-     :    ..                    ..    :     -`                    -     :     -`                   `-     :    `-                    ..    :     ..                    -`    :     -`                   `-     :     -                    -`    `-    `-                    :     :     ..           |
--        `-      :     `-                  ..     :      ..                 `-      :      -`                 `-      :     `-                  -`     :      `-                  -      :      ..                 `-      :      -`                 -`      :     `-                  :      :      ..          |
-- `.....-.    ......`    :......    ......:`   `......    .-.....`    ......:    .......    :......    `.....--    ......`    :......    ......:`   `......    `:......    ......:    `......    --.....`    ......:    .......    :......    `.....:.    ......`   `:......    ....../    `......    .-.....`   |
-- :      :   :      :   -      ..  .`     `-  `-      :   :      :   :      -`  -`     `-  `.      :   :      :   :      -`  -`     ..  ..     `-   -      :   -`     ..  ..     `-  `-      :   :      :   :      .`  -`     `-  `-      :   :      :   :      -`  -`     `.  ..      -   :      :   :      :   |
-- :      :   :      :   -`     ..  ..     `-  `-      -   :      :   -`     -`  .`     ..  `-     `-   :      :   :      -   -`     ..  ..     `-   :      :   -`     ..  ..     `-   -      :   :      :   -`     -`  ..     `.  `-     `-   :      :   :      -`  -`     ..  ..     `-   :      :   :      :   |
-- `......     ......`    ......`    `......    `......     ......     ......`    `.....`    `......     ......     ......`    ......`    `......    `......     ......`    `......    `......     ......     ......`    `.....`    `......     ......     ......`    ......`    `......    `......     ......`  \|/
-- <----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------->
--                                                                                                                                                    Estados

----------------------------------------------------------------------------

pertenencia :: Af -> Palabra -> Bool
pertenencia (q,a,tau,sigma,y) word = pertenencia_aux (q,a,tau,sigma,y) word sigma []
--La lista vacía representa los estados que quedan por examinar en caso de computar un AFND (se guardarán los estados y la palabra que quedaba por computar en cada uno)
--En caso de computar un AFD esa lista permanecerá vacía

pertenencia_aux :: Af -> Palabra -> Estado -> [(Estado,Palabra)] -> Bool
pertenencia_aux (q,a,tau,sigma,y) word estActual porExaminar
    | longitudWord == 0 && estActual `elem` y = True
    --Si la palabra es vacía y estamos en un estado de aceptación devolvemos True
    | longitudWord == 0 && longitudPorExaminar > 0 = pertenencia_aux (q,a,tau,sigma,y) (snd(head porExaminar)) (fst(head porExaminar)) (tail porExaminar)
    --Si la palabra es vacía y quedan elementos por examinar, comprobamos si ese elemento devuelve True volviendo a llamar a pertenencia_aux
    | longitudWord == 0 = False
    --En este punto la palabra es vacía, no quedan elementos por examinar, y no estamos en un estado de aceptación, devolvemos False
    | (head word) `notElem` a = error "La palabra contiene algún símbolo no perteneciente al alfabeto."
    --Si el símbolo que vamos a comprobar no pertenece al alfabeto, lanzamos un error (no lo lanzamos antes, pues la palabra puede ser vacía y con head se lanza una interrupción)
    | length lista > 0 = pertenencia_aux (q,a,tau,sigma,y) (tail word) (head lista) (porExaminar ++ (zip (tail lista) ([(tail word) | x <- [1..(length (tail lista))]])))
    --Si hay algún estado al que transicionar, transicionamos y guardamos los demás junto con la palabra que nos queda por computar
    | longitudPorExaminar > 0 = pertenencia_aux (q,a,tau,sigma,y) (snd(head porExaminar)) (fst(head porExaminar)) (tail porExaminar)
    --Si no hay estados a los que transicionar pero quedan estados por comprobar, comprobamos uno de ellos
    | otherwise = False
    --Si no hay ni estados a los que transicionar ni estados por examinar, devolvemos False
    where
        lista = [qf | (qin,simb,qf) <- tau, (qin == estActual && simb == (head word))]
        --lista contiene los estados a los que se puede transicionar desde estActual con head word
        longitudWord = length word
        --longitud de la palabra que queda por computar
        longitudPorExaminar = length porExaminar
        --longitud de la lista de elementos por examinar 

casosDePrueba :: IO ()
casosDePrueba = do putStrLn "-----Pruebas AFD-----\n"
                   casoPrueba1
                   casoPrueba2
                   casoPrueba3
                   casoPrueba4
                   casoPrueba5
                   putStrLn "-----Pruebas AFND-----\n"
                   casoPrueba6
                   casoPrueba7
                   casoPrueba8
                   casoPrueba9
                   casoPrueba10

casoPrueba1 :: IO ()
casoPrueba1 = do putStrLn "-----Caso de prueba 1-----"
                 putStrLn "Lenguaje de las palabras que terminan con 'a'\n"
                 if (pertenencia getAutomataD "abc") 
                     then putStrLn "Incorrecto, abc no pertenece\n\n"
                     else putStrLn "Correcto, abc no pertenece\n\n"

casoPrueba2 :: IO ()
casoPrueba2 = do putStrLn "-----Caso de prueba 2-----"
                 putStrLn "Lenguaje de las palabras que terminan con 'a'\n"
                 if (pertenencia getAutomataD "abca") 
                    then putStrLn "Correcto, abca pertenece\n\n"
                    else putStrLn "Incorrecto, abca pertenece\n\n"

casoPrueba3 :: IO ()
casoPrueba3 = do putStrLn "-----Caso de prueba 3-----"
                 putStrLn "Lenguaje de las palabras que contienen 'aaa' o terminan en 'a' y, además, no contienen 'c'\n"
                 if (pertenencia getAutomataD3 "bbaba") 
                     then putStrLn "Correcto, bbaba pertenece\n\n"
                     else putStrLn "Incorrecto, bbaba pertenece\n\n"

casoPrueba4 :: IO ()
casoPrueba4 = do putStrLn "-----Caso de prueba 4-----"
                 putStrLn "Lenguaje de las palabras que contienen 'aaa' o terminan en 'a' y, además, no contienen 'c'\n"
                 if (pertenencia getAutomataD3 "bbaaab") 
                     then putStrLn "Correcto, bbaaab pertenece\n\n"
                     else putStrLn "Incorrecto, bbaaab pertenece\n\n"

casoPrueba5 :: IO ()
casoPrueba5 = do putStrLn "-----Caso de prueba 5-----"
                 putStrLn "Lenguaje de las palabras que contienen 'aaa' o terminan en 'a' y, además, no contienen 'c'\n"
                 if (pertenencia getAutomataD3 "bbaaabca") 
                     then putStrLn "Incorrecto, bbaaabca no pertenece\n\n"
                     else putStrLn "Correcto, bbaaabca no pertenece\n\n"

casoPrueba6 :: IO ()
casoPrueba6 = do putStrLn "-----Caso de prueba 6-----"
                 putStrLn "Lenguaje de las palabras que terminan con 'a'\n"
                 if (pertenencia getAutomataN "abc") 
                     then putStrLn "Incorrecto, abc no pertenece\n\n"
                     else putStrLn "Correcto, abc no pertenece\n\n"

casoPrueba7 :: IO ()
casoPrueba7 = do putStrLn "-----Caso de prueba 7-----"
                 putStrLn "Lenguaje de las palabras que terminan con 'a'\n"
                 if (pertenencia getAutomataN "abca") 
                     then putStrLn "Correcto, abca pertenece\n\n"
                     else putStrLn "Incorrecto, abca pertenece\n\n"

casoPrueba8 :: IO ()
casoPrueba8 = do putStrLn "-----Caso de prueba 8-----"
                 putStrLn "Lenguaje de las palabras que contienen 'aaa' o terminan en 'a' y, además, no contienen 'c'\n"
                 if (pertenencia getAutomataN4 "bbaba") 
                     then putStrLn "Correcto, bbaba pertenece\n\n"
                     else putStrLn "Incorrecto, bbaba pertenece\n\n"

casoPrueba9 :: IO ()
casoPrueba9 = do putStrLn "-----Caso de prueba 9-----"
                 putStrLn "Lenguaje de las palabras que contienen 'aaa' o terminan en 'a' y, además, no contienen 'c'\n"
                 if (pertenencia getAutomataN4 "bbaaab") 
                     then putStrLn "Correcto, bbaaab pertenece\n\n"
                     else putStrLn "Incorrecto, bbaaab pertenece\n\n"

casoPrueba10 :: IO ()
casoPrueba10 = do putStrLn "-----Caso de prueba 10-----"
                  putStrLn "Lenguaje de las palabras que contienen 'aaa' o terminan en 'a' y, además, no contienen 'c'\n"
                  if (pertenencia getAutomataN4 "bbaaabca") 
                     then putStrLn "Incorrecto, bbaaabca no pertenece\n\n"
                     else putStrLn "Correcto, bbaaabca no pertenece\n\n"
                     