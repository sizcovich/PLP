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

    it "devuelve el significado de una clave" $ do
      get [("calle",[3]),("city",[2,1])] ? "city" `shouldBe` [2,1]
      get [("a", "b"),("c","l"),("m","r")] ? "c"  `shouldBe` "l" 

    it "agrega una clave junto con un significado a un diccionario aplicando una funcion especifica"
      insertWith (++) 2 ['p'] (insertWith (++) 1 ['a','b'] (insertWith (++) 1 ['l'] [])) `shouldBe` [(1,"lab"),(2,"p")]

    it "agrupa los elementos de un diccionario por sus claves aplicando una funcion"
      groupByKey [("calle","Jean_Jaures"),("ciudad","Brujas"), ("ciudad","Kyoto"),("calle","7")] `shouldBe` [("calle",["Jean_Jaures","7"]),("ciudad",["Brujas","Kyoto"])]

    it "une dos diccionarios por sus claves aplicando una funcion"
      unionWith (++) [("calle",[3]),("city",[2,1])] [("calle", [4]), ("altura", [1,3,2])] `shouldBe` [("calle",[3,4]),("city",[2,1]),("altura",[1,3,2])] 

  describe "Utilizando Map Reduce" $ do
    it "divide la carga de manera balanceada" $ do
      distributionProcess 5 [1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10 ,11 ,12]  `shouldMatchList` [[5,10],[4,9],[3,8],[2,7,12],[1,6,11]]
      distributionProcess 2 ["a","b","c","d","e"]  `shouldMatchList` [["b","d"],["a","c","e"]]
      distributionProcess 5 [1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10]  `shouldMatchList` [[5,10],[4,9],[3,8],[2,7],[1,6]]
      distributionProcess 5 [1 ,2 ,3]  `shouldMatchList` [[],[],[3],[2],[1]]

    it "aplica Map a una lista aplicando alguna funcion"
      mapperProcess (pruebaMapper) [("Pablo","Berlin"),("Gabriela","Amsterdam"),("Taihu","Amsterdam")] `shouldMatchList` [("Berlin","I"),("Amsterdam","II")]
    
    it "combina los resultados de cada maquina" $ do
      combinerProcess [[("Berlin",['I']),("Amsterdam", ['I']),("Cairo",['I'])], [("Cairo",['I']),("Amsterdam", ['I','I'])]] `shouldBe` [("Amsterdam","III"),("Berlin","I"),("Cairo","II")]
      combinerProcess [[("Berlin",['I']),("Amsterdam", ['I']),("Cairo",['I'])], [("Buenos Aires",['I']),("San Pablo", ['I','I'])]] `shouldBe` [("Amsterdam","I"),("Berlin","I"),("Buenos Aires","I"),("Cairo","I"),("San Pablo","II")]
      combinerProcess [[("Berlin",['I']),("Amsterdam", ['I']),("Cairo",['I'])], [("Cairo",['I','I','I'])]] `shouldBe` [("Amsterdam","I"),("Berlin","I"),("Cairo","IIII")]

    it "visitas por monumento funciona en algún orden" $ do
      visitasPorMonumento [ "m1" ,"m2" ,"m3" ,"m2","m1", "m3", "m3"] `shouldMatchList` [("m3",3), ("m1",2), ("m2",2)]
      visitasPorMonumento [ "Torta Frita" ,"Sandwich" ,"Empanada" ,"Empanada","Empanada", "Sandwich", "Pizza"] `shouldMatchList` [("Empanada",3), ("Sandwich",2), ("Torta Frita",1), ("Pizza", 1)] 
      visitasPorMonumento [ "Juan","Juan","Juan","Juan","Juan", "Juan", "Juan"] `shouldMatchList` [("Juan",7)] 

    --it "monumentosTop devuelve los más visitados en algún orden" $ do 
    --  monumentosTop [ "m1", "m0", "m0", "m0", "m2", "m2", "m3"] 
    --  `shouldSatisfy` (\res -> res == ["m0", "m2", "m3", "m1"] || res == ["m0", "m2", "m1", "m3"])

