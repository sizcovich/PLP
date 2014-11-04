--  Para correr los tests deben cargar en hugs el módulo Tests
--  y evaluar la expresión "main".
-- Algunas funciones que pueden utilizar para chequear resultados:
-- http://hackage.haskell.org/package/hspec-expectations-0.6.1/docs/Test-Hspec-Expectations.html#t:Expectation

import Test.Hspec
import MapReduce

main :: IO ()
main = hspec $ do
  describe "Utilizando Diccionarios" $ do
    it "puede determinarse si un elemento es una clave o no" $ do
      belongs 3 [(3, "A"), (0, "R"), (7, "G")]    `shouldBe` True
      belongs "k" []                              `shouldBe` False
      [("H", [1]), ("E", [2]), ("Y", [0])] ? "R"  `shouldBe` False
      [("V", [1]), ("O", [2]), ("S", [0])] ? "V"  `shouldBe` True
      [] ? 1                                      `shouldBe` False

    it "devuelve el significado de una clave" $ do
      get "city" [("calle",[3]),("city",[2,1])] `shouldBe` [2,1]
      get "c" [("a", "b"),("c","l"),("m","r")]  `shouldBe` "l" 
      [(1, "a"), (5,"z"), (12,"hi")] ! 12       `shouldBe` "hi"

    it "agrega una clave junto con un significado a un diccionario aplicando una funcion especifica" $ do
      insertWith (++) 2 ['p'] (insertWith (++) 1 ['a','b'] (insertWith (++) 1 ['l'] [])) `shouldBe` [(1,"lab"),(2,"p")]
      insertWith (+) 3 2 []                                                              `shouldBe` [(3,2)]
      insertWith (+) 2 4 (insertWith (+) 1 3 (insertWith (+) 1 2 []))                    `shouldBe` [(1,5), (2,4)]

    it "agrupar los datos por clave, generando un diccionario que asocia a cada clave con la lista de todos sus valores" $ do
      groupByKey [("calle","Jean_Jaures"),("ciudad","Brujas"), ("ciudad","Kyoto"),("calle","7")] `shouldBe` [("calle",["Jean_Jaures","7"]),("ciudad",["Brujas","Kyoto"])]
      groupByKey [("a",[3,2]), ("a", []),("b",[2,3,4]) ]                                         `shouldBe` [("a",[[3,2], []]),("b",[[2,3,4]])]

    it "une dos diccionarios por sus claves aplicando una funcion" $ do
      unionWith (++) [("calle",[3]),("city",[2,1])] [("city",[]), ("calle", [4]), ("altura", [1,3,2])] `shouldBe` [("calle",[3,4]),("city",[2,1]),("altura",[1,3,2])] 
      unionWith (+) [("a",3),("b",4)] [("b", 6), ("a", 7), ("c",10)]                                   `shouldBe` [("a",10), ("b",10), ("c",10)]

  describe "Utilizando Map Reduce" $ do
    it "divide la carga de manera balanceada" $ do
      distributionProcess 5 [1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10 ,11 ,12]  `shouldMatchList` [[5,10],[4,9],[3,8],[2,7,12],[1,6,11]]
      distributionProcess 2 ["a","b","c","d","e"]                    `shouldMatchList` [["b","d"],["a","c","e"]]
      distributionProcess 5 [1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10]          `shouldMatchList` [[5,10],[4,9],[3,8],[2,7],[1,6]]
      distributionProcess 5 [1 ,2 ,3]                                `shouldMatchList` [[],[],[3],[2],[1]]

    it "aplica Map a los elementos de una lista y luego agrupa los resultados" $ do
      mapperProcess (pruebaMapper) [("Pablo","Berlin"),("Gabriela","Amsterdam"),("Taihu","Amsterdam")]   `shouldMatchList` [("Berlin","I"),("Amsterdam","II")]
      mapperProcess (pruebaMapper) [("Sabrina","Francia"),("Sebastian","Argentina"),("Martin","Brasil")] `shouldMatchList` [("Francia","I"),("Brasil","I"),("Argentina", "I")]
      mapperProcess (pruebaMapper) []                                                                    `shouldMatchList` []
    
    it "combina los resultados de cada maquina" $ do
      combinerProcess [[("Berlin",['I']),("Amsterdam", ['I']),("Cairo",['I'])], [("Cairo",['I']),("Amsterdam", ['I','I'])]]        `shouldBe` [("Amsterdam","III"),("Berlin","I"),("Cairo","II")]
      combinerProcess [[("Berlin",['I']),("Amsterdam", ['I']),("Cairo",['I'])], [("Buenos Aires",['I']),("San Pablo", ['I','I'])]] `shouldBe` [("Amsterdam","I"),("Berlin","I"),("Buenos Aires","I"),("Cairo","I"),("San Pablo","II")]
      combinerProcess [[("Berlin",['I']),("Amsterdam", ['I']),("Cairo",['I'])], [("Cairo",['I','I','I'])],[]]                      `shouldBe` [("Amsterdam","I"),("Berlin","I"),("Cairo","IIII")]
    
    it "aplica reducer sobre cada elemento y aplana el resultado para unificar las soluciones" $ do
      reducerProcess (reducerExample) [("Amsterdam","III"),("Berlin","I"),("Cairo","II")]   `shouldBe` [("Amsterdam"),("Berlin"),("Cairo")]
      reducerProcess (pruebaReducer) [("Amsterdam","III"),("Berlin","I"),("Cairo","II")]    `shouldBe` [("Amsterdam", 3),("Berlin", 1),("Cairo", 2)]

    it "framework completo" $ do
      mapReduce (pruebaMapper) (pruebaReducer) [("Pablo","Berlin"),("Gabriela","Amsterdam"),("Taihu","Cairo"), ("Pablo", "Cairo"),("Taihu","Amsterdam"),("Juan","Amsterdam")] `shouldBe` [("Amsterdam", 3),("Berlin", 1),("Cairo", 2)]

  describe "Utilización" $ do
    it "visitas por monumento funciona en algún orden" $ do
      visitasPorMonumento [ "m1" ,"m2" ,"m3" ,"m2","m1", "m3", "m3"]                                           `shouldMatchList` [("m3",3), ("m1",2), ("m2",2)]
      visitasPorMonumento [ "Torta Frita" ,"Sandwich" ,"Empanada" ,"Empanada","Empanada", "Sandwich", "Pizza"] `shouldMatchList` [("Empanada",3), ("Sandwich",2), ("Torta Frita",1), ("Pizza", 1)] 
      visitasPorMonumento [ "Juan","Juan","Juan","Juan","Juan", "Juan", "Juan"]                                `shouldMatchList` [("Juan",7)] 

    it "monumentosTop devuelve los más visitados en algún orden" $ do 
      monumentosTop [ "m1", "m0", "m0", "m0", "m2", "m2", "m3"]                                          `shouldSatisfy` (\res -> res == ["m0", "m2", "m3", "m1"] || res == ["m0", "m2", "m1", "m3"])
      monumentosTop [ "Juan","Juan","Juan","Juan","Juan", "Juan", "Juan"]                                `shouldMatchList` ["Juan"] 
      monumentosTop [ "Torta Frita" ,"Sandwich" ,"Empanada" ,"Empanada","Empanada", "Sandwich", "Pizza"] `shouldSatisfy` (\res -> res == ["Empanada", "Sandwich", "pizza", "Torta Frita"] || res == ["Empanada", "Sandwich", "Torta Frita", "Pizza"])  
      
    it "monumentosPorPais devuelve la cantidad de monumentos por cada país, en algún orden" $ do
	  monumentosPorPais [] `shouldMatchList` []
	  monumentosPorPais [(City,[("country","Argentina")]), (Street,[]), (Street,[("country","Argentina")])] `shouldMatchList` []
	  monumentosPorPais [(City,[("country","Argentina")]), (Street,[]), (Street,[("country","Argentina")]), (Monument,[("country", "Siberia")])] `shouldMatchList` [("Siberia", 1)]
	  monumentosPorPais [(Monument,[("country", "Siberia")]), (Monument,[("country","Argentina")])] `shouldSatisfy` ( \res -> res == [("Siberia",1),("Argentina",1)] || res == [("Argentina",1),("Siberia",1)] )
	  monumentosPorPais [(Monument,[("country", "Siberia")]), (Monument,[("country","Argentina")]), (Monument,[("country","Argentina")]), (Monument,[("country","Argentina")]), (Monument,[("country", "Siberia")])] `shouldSatisfy` ( \res -> res == [("Siberia",2),("Argentina",3)] || res == [("Argentina",3),("Siberia",2)] )
	  
      
