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

  describe "Utilizando Map Reduce" $ do
    it "divide la carga de manera balanceada" $ do
      distributionProcess 5 [1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10 ,11 ,12]  `shouldMatchList` [[5,10],[4,9],[3,8],[2,7,12],[1,6,11]]
      distributionProcess 2 ["a","b","c","d","e"]  `shouldMatchList` [["b","d"],["a","c","e"]]
      distributionProcess 5 [1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10]  `shouldMatchList` [[5,10],[4,9],[3,8],[2,7],[1,6]]
      distributionProcess 5 [1 ,2 ,3]  `shouldMatchList` [[],[],[3],[2],[1]]
    
    it "visitas por monumento funciona en algún orden" $ do
      visitasPorMonumento [ "m1" ,"m2" ,"m3" ,"m2","m1", "m3", "m3"] `shouldMatchList` [("m3",3), ("m1",2), ("m2",2)] 

    it "monumentosTop devuelve los más visitados en algún orden" $ do 
      monumentosTop [ "m1", "m0", "m0", "m0", "m2", "m2", "m3"] 
      `shouldSatisfy` (\res -> res == ["m0", "m2", "m3", "m1"] || res == ["m0", "m2", "m1", "m3"])