module MapReduce where

import Data.Ord
import Data.List

-- ---------------------------------Sección 1---------Diccionario ---------------------------
type Dict k v = [(k,v)]

-- Ejercicio 1
belongs :: Eq k => k -> Dict k v -> Bool
belongs m = foldr (\x y -> (m == (fst x)) || y) False
-- Dado un elemento m y un diccionario, se recorre a este último de derecha a izquierda y se compara
-- a m con cada clave, uniendo cada comparación por un "OR". En el caso en el que coincida con algún
-- elemento, devuelve True. Caso contrario, False.

(?) :: Eq k => Dict k v -> k -> Bool
(?) xs m = belongs m xs
-- Evalúa la función belongs con un diccionario y una clave.

-- Ejercicio 2
get :: Eq k => k -> Dict k v -> v
get m xs = foldr 	(\x y -> if (m == (fst x)) then (snd x) else y) 
					(snd (head xs))
					xs
-- Dado un elemento m y un diccionario, lo recorre de derecha a izquierda comparando cada clave con
-- m. Una vez que encuentra la clave m, devuelve el significado de la misma. Dado que una vez que se
-- encuentra la clave correspondiente se devuelve directamente el significado sin que se lo combine
-- con resultados parciales, no afecta en el resultado el valor de inicio, que en nuestro caso es
-- (snd (head xs)).

(!) :: Eq k => Dict k v -> k -> v
(!) xs m = get m xs
-- Evalúa la función get con un diccionario y una clave.

-- Ejercicio 3
insertWith :: Eq k => (v -> v -> v) -> k -> v -> Dict k v -> Dict k v
insertWith f m l xs = if ((?) xs m)
                      then (foldr (\x rec -> 	if (m == fst x)
                      							then ((fst x, (f (snd x) l)):rec)
                      							else (x:rec)) [] xs) 
                      else (xs++[(m,l)])
-- Dada una función f, una clave m, un significado l y un diccionario, si la clave m pertenece a l
-- ((?) xs m), entonces se la compara con cada elemento del diccionario y una vez que se la
-- encuentra la agrega al diccionario combinando su significado con l a través de la función f. En
-- el caso en el que no esté, se concatena la clave con el significado l al diccionario.

-- Ejercicio 4
groupByKey :: Eq k => [(k,v)] -> Dict k [v]
groupByKey = foldl	(\y x -> (insertWith (++) (fst x) ([snd x]) y)) []
-- Para cada elemento del arreglo ingresado, se llama a la función insertWith con la función
-- concatenar, el primer elemento de la tupla, un arreglo conteniendo el segundo elemento y el resto
-- de los elementos del arreglo. De este modo, se concatenan los significados de cada elemento si es
-- que se encuentran repetidos y sino, son agregados normalmente a la salida de la función.

-- Ejercicio 5
unionWith :: Eq k => (v -> v -> v) -> Dict k v -> Dict k v -> Dict k v
unionWith f xs ys = foldl (\y x -> (insertWith f (fst x) (snd x) y))
							[]
							(xs++ys)
-- Dada la concatenación de dos diccionarios (xs++ys), toma cada elemento del primero y aplica insertWith de
-- una función f con la clave y el significado de cada uno de ellos. De este modo, se elimina la
-- posibilidad de tener claves repetidas en dicha concatenación.

-- ------------------------------Sección 2--------------MapReduce---------------------------

type Mapper a k v = a -> [(k,v)]
type Reducer k v b = (k, [v]) -> [b]


-- Ejercicio 6
distributionProcess :: Int -> [a] -> [[a]]
distributionProcess i = foldr (\m ns -> tail(ns) ++ [m:head(ns)])
								(replicate i [])
--Dado un número m y un arreglo xs, distribuye los elementos de xs en m arreglos de manera 
--balanceada. Para ésto, recorre el arreglo de derecha a izquierda y en cada iteración coloca 
--el primer elemento de xs en el primer arreglo de la salida (la cual comienza siendo m arreglos vacíos), luego  
--coloca dicho arreglo al final de la salida y vuelve a iterar.

-- Ejercicio 7
mapperProcess :: Eq k => Mapper a k v -> [a] -> [(k,[v])]
mapperProcess xs ys = groupByKey (foldr (\x y -> (xs x)++y) [] ys)
-- Dado un elemento de tipo Mapper a k v y un arreglo, le aplica Mapper a cada elemento obteniendo
-- un resultado de tipo [(k,v)] y lo concatena con la salida generada (que comienza con un arreglo
-- vacío). Por último, concatena los significados por su clave a través de groupByKey.

pruebaMapper :: (String,String) -> [(String,Char)]
pruebaMapper (x,y) = [(y,'I')]
-- Función de prueba para el Mapper. Permite crear el ejemplo del enunciado. Se puede probar el
-- ejemplo a través de mapperProcess (pruebaMapper)
-- [("Pablo","Berlin"),("Gabriela","Amsterdam"),("Taihu","Amsterdam")]

-- Ejercicio 8
combinerProcess :: (Eq k, Ord k) => [[(k, [v])]] -> [(k,[v])]
combinerProcess xss = order (foldr(\x y -> unionWith (++) x y) [] xss)
-- Dado un arreglo de diccionarios, los concatena mediante la función unionWith. Posteriormente, los
-- ordena de forma creciente (según su clave) mediante la función order.

order::(Eq a, Ord a)=>[(a,b)]->[(a,b)]
order = foldr	insertarOrdenado []
-- Dado un arreglo, lo recorre de derecha a izquierda para ordenar cada uno de sus elementos mediante 
-- la función insertarOrdenando.

insertarOrdenado::(Eq k, Ord k) => (k, v) -> [(k, v)] -> [(k, v)]
insertarOrdenado x xs = [less | less <- xs , (fst less) <= (fst x)] ++ [x] ++ [greater | greater <- xs , (fst greater) > (fst x)]
-- Dada una tupla t y un arreglo ordenado xs, coloca a t en xs de manera tal de que éste siga ordenado. 

-- Ejercicio 9
reducerProcess :: Reducer k v b -> [(k, [v])] -> [b]
reducerProcess red ls = concat (foldr	(\x rec -> (red x) : rec)
											[[]]
											ls)
											
-- Dada una función Reducer y un diccionario, se recorre el diccionario de derecha a izquierda, y se aplica la función
-- Reducer a cada elemento, quedando una lista de listas de tipo b. Finalmente se aplica concat para aplanar todas las listas
-- para obtener 1 lista sólamente, de tipo b.

reducerExample :: (k, v) -> [k]
reducerExample par = [fst par]

-- Ejemplo de función Reducer, que dado un par (k,v), devuelve una lista que contiene 1 elemento, siendo ese único
-- elemento, el primer elemento del par.

pruebaReducer :: (k, [v]) -> [(k, Int)]
pruebaReducer par = [(fst par, length (snd par))]

-- Otro ejemplo de función Reducer. Este ejemplo sirve para la resolución del ejemplo del enunciado

-- Ejercicio 10 
mapReduce :: (Eq k, Ord k) => Mapper a k v -> Reducer k v b -> [a] -> [b]
mapReduce fMap fRed ls = reducerProcess fRed combinedList

							where combinedList = combinerProcess mappedProcess
								-- mappedProcess :: [[(k,[v])]]		(Input)
								-- combinedProcess :: [(k,[v])]		(Output)

								where mappedProcess =  (foldr	(\x rec -> (mapperProcess fMap x) : rec)
																[[]]
																distributionList)

									-- Output :: [[(k,[v])]]

									where distributionList = distributionProcess 100 ls
									
-- Función que junta todos los pasos previamente explicados.
--	Dada una función Mapper, una funcion Reducer y una lista, realiza:
--		- Distribuye entre 100 procesos la lista
--		- Cada proceso, a su lista asignada, aplica la función Mapper a cada elemento de esa lista
--		- Luego, se combinan los resultados de cada proceso por medio del combinerProcess
--		- Finalmente se aplica la función de reducción sobre cada elemento obtenido tras la operación de combiner

-- Ejercicio 11
visitasPorMonumento :: Ord a => [a] -> Dict a Int
visitasPorMonumento = mapReduce divisionMapper monumentReducer

divisionMapper :: a -> Dict a Int
divisionMapper x = [(x,1)]

-- visitasPorMonumento aplica mapReduce a la función divisionMapper y monumentReducer.
-- divisionMapper se encarga de, dado un elemento x, crear un arreglo de tuplas donde el primer
-- elemento de la tupla agregada es x y el segundo el valor 1. Por otro lado, monumentReducer crea,
-- dada una tupla de tipo (a, [b]), un arreglo de tuplas de tipo (a, b) donde el segundo elemento
-- consiste en la sumatoria del arreglo de la tupla ingresada.

-- Ejercicio 12
monumentosTop :: Ord a => [a] -> [a]
monumentosTop xs = mapReduce topMapper topReducer (visitasPorMonumento xs)

topMapper :: (a, Int) -> [(Int, a)]
topMapper (x,y) = [(-y,x)]

topReducer :: (Int, [a]) -> [a]
topReducer par = snd par

--Dado un arreglo de elementos, monumentosTop se encarga de computar visitasPorMonumento. Ésta función
--genera tuplas (a,b) en donde b representa la cantidad de veces que aparece el elemento a en dicho
--arreglo. Posteriormente, topMapper mappea cada tupla (a,b) a (-b, a) para que se ordenen de manera
--creciente en la cantidad de apariciones. Por último, topReducer se encarga de tomar una tupla (b, [a]) 
--y devolver su segunda componente.

-- Ejercicio 13 
monumentosPorPais :: [(Structure, Dict String String)] -> [(String, Int)]
monumentosPorPais = mapReduce monumentMapper monumentReducer


monumentMapper :: (Structure, Dict String String) -> [(String, Int)]
monumentMapper entry =	if ( (fst entry) == Monument )
						then [(get "country" (snd entry), 1)]
						else []


monumentReducer :: (a, [Int]) -> [(a, Int)]
monumentReducer entry = [(fst entry, sum (snd entry))]

-- monumentosPorPais toma una lista de pares de tipo (Structure, Dict String String) y devuelve una lista
-- de tipo (String, Int), donde el primer elemento corresponde al nombre de un país, y el segundo elemento a la cantidad
-- de monumentos en ese país
-- La función Mapper (monumentMapper), toma un par de tipo (Structure, Dict String String), y devuelve una lista, siendo
--		En caso que la estructura no sea un Monument, se devuelva una lista vacia.
--		En caso que la estrucutra sea un Monument, se devuelve un par, en donde el primer elemento es el nombre del país,
--			y el segundo elemento es un 1 (que es la cantidad de monumentos en ese país dado ese par inicial).
-- La función Reducer (monumentReducer) toma un Dict String [Int]
--		Sabemos que cada clave de ese Diccinario es un país.
--		También sabemos que a cada clave le corresponde una lista de Int. La cantidad de elementos de esa lista es la cantidad de
--			pares (Structure, Dict String String) que eran Monument y correspondian a ese país. Dicho de otra forma, la cantidad de
--			elementos es la cantidad de monumentos de ese país
--		También se sabe que cada elemento de esa lista de Int, es un 1. Con lo cual la cantidad total de monumentos de ese país
--			puede ser tanto, la suma de los elementos de esa lista, como la cantidad de elementos de esa lista. En este caso se
--			decidió resolverlo usando la suma de los elementos de la lista.


-- ------------------------ Ejemplo de datos del ejercicio 13 ----------------------
data Structure = Street | City | Monument deriving (Show, Eq)

items :: [(Structure, Dict String String)]
items = [
    (Monument, [
      ("name","Obelisco"),
      ("latlong","-36.6033,-57.3817"),
      ("country", "Argentina")]),
    (Street, [
      ("name","Int. Güiraldes"),
      ("latlong","-34.5454,-58.4386"),
      ("country", "Argentina")]),
    (Monument, [
      ("name", "San Martín"),
      ("country", "Argentina"),
      ("latlong", "-34.6033,-58.3817")]),
    (City, [
      ("name", "Paris"),
      ("country", "Francia"),
      ("latlong", "-24.6033,-18.3817")]),
    (Monument, [
      ("name", "Bagdad Bridge"),
      ("country", "Irak"),
      ("new_field", "new"),
      ("latlong", "-11.6033,-12.3817")])
    ]


------------------------------------------------
------------------------------------------------
