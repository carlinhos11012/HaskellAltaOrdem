-- Aluno: Carlos Henrique Moreira dos Santos
import Text.Printf

{--
1. Escreva uma função chamada fatorialn que usando o operador range e a função foldr devolva o fatorial de n. 
--}
fatorialn :: Int -> Int
fatorialn x = foldr (*) 1 [1..x]

{--
2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos inteiros listados. 
--}
quadradoReal :: [Double] -> [Double]
quadradoReal x = map (^2) x

{--
3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de palavras e devolve uma lista com o comprimento de cada uma destas palavras. 
--}
comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras x = map length x

{--
4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior número entre 0 e 100000 que seja divisivel por 29. 
--}
maiorMultiploDe29 :: Int
maiorMultiploDe29 = maximum (filter (\y -> y `mod` 29 == 0) [1..100000])

{--
5. Usando a função filter escreva uma função, chamada maiorMultiploDe que recebe um inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro. 
--}
maiorMultiploDe :: Int -> Int
maiorMultiploDe x = maximum (filter (\y -> y `mod` x == 0) [1..100000])

{--
6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠=1²+2²+3²+4²...+𝑛².
--}
somaQuadrados :: Int -> Int
somaQuadrados x = foldr (\y a -> y^2 + a) 0 [1..x]

{--
7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o comprimento (cardinalidade) de uma lista dada. 
--}
comprimento :: [x] -> Int
comprimento x = foldl (\a y -> a + 1) 0 x

{--
8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos. 
--}

-- FLIP (Faz a troca entre as duas variaveis passadas como parâmetro, e executa a função).
-- FLIP 1
flipDividirPor2 :: Double -> Double
flipDividirPor2 x = flip (/) 2 x

-- FLIP 2
flipMenorQue2 :: Double -> Bool
flipMenorQue2 x = flip (>) x 2

-- MAX (Retorna o maior entre dois números).
-- MAX 1
maiorEntre2Numeros :: Int -> Int -> Int
maiorEntre2Numeros x y = max x y

-- MAX 2
maiorEntre4Numeros :: Int -> Int -> Int -> Int -> Int
maiorEntre4Numeros w x y z = max (max w x) (max y z)

-- MIN (Retorna o menor entre dois números).
-- MIN 1
menorEntre2Numeros :: Int -> Int -> Int
menorEntre2Numeros x y = min x y

-- MIN 2
menorEntre4Numeros :: Int -> Int -> Int -> Int -> Int
menorEntre4Numeros w x y z = min (min w x) (min y z)

-- CURRY (Transforma uma função uncurry em uma função curry).
-- CURRY 1
primeiro :: Int -> Int -> Int
primeiro x y = curry fst x y

-- CURRY 2
dobroDaSoma :: Int -> Int -> Int
dobroDaSoma = curry (\ (x,y) -> 2*(x+y))

-- UNCURRY
-- UNCURRY 1
dividir :: Int -> Int -> Int
dividir x y = uncurry div (x,y)

-- UNCURRY 2
restoDivisao :: Int -> Int -> Int
restoDivisao x y = uncurry mod (x,y)

main = do
  --Ex1
  printf "\nFunc. 1: entrada:%d; resultado:%s\n" (4::Int) (show (fatorialn 4))

  --Ex2
  printf "\nFunc. 2: entrada:%s; resultado:%s\n" (show ([1..4]::[Int])) (show (quadradoReal [1..4]))

  -- Ex3
  printf "\nFunc. 3: entrada:%s; resultado:%s\n" (show ["Comprimento","de","Cada","Palavra"]) (show (comprimentoPalavras ["Comprimento","de","Cada","Palavra"]))

  --Ex4
  printf "\nFunc. 4: entrada:  ; resultado:%d\n" (maiorMultiploDe29)

  --Ex5
  printf "\nFunc. 5: entrada:%d; resultado:%d\n" (387::Int) (maiorMultiploDe 387)

  --Ex6
  printf "\nFunc. 6: entrada:%d; resultado:%d\n" (10::Int) (somaQuadrados 10)

  --Ex7
  printf "\nFunc. 7: entrada:%s; resultado:%s\n" (show ([1..10]::[Int])) (show (comprimento [1..10]))

  --Ex8

  --FLIP 1
  printf "\nFunc. 8 FLIP 1: entrada:%d; resultado:%f\n" (10::Int) (flipDividirPor2 10)

  --FLIP 2
  printf "\nFunc. 8 FLIP 2: entrada:%d; resultado:%s\n" (10::Int) (show(flipMenorQue2 1))


  --MAX 1
  printf "\nFunc. 8 MAX 1: entrada:%d %d; resultado:%d\n" (28::Int) (27::Int) (maiorEntre2Numeros 28 27)

  --MAX 2
  printf "\nFunc. 8 MAX 2: entrada:%d %d %d %d; resultado:%d\n" (28::Int) (27::Int) (31::Int) (3::Int) (maiorEntre4Numeros 28 27 31 3)

  --MIN 1
  printf "\nFunc. 8 MIN 1: entrada:%d %d; resultado:%d\n" (28::Int) (27::Int) (menorEntre2Numeros 28 27)

  --MIN 2
  printf "\nFunc. 8 MIN 2: entrada:%d %d %d %d; resultado:%d\n" (28::Int) (27::Int) (31::Int) (3::Int) (menorEntre4Numeros 28 27 31 3)

  --CURRY 1
  printf "\nFunc. 8 CURRY 1: entrada:%d %d; resultado:%d\n" (28::Int) (27::Int) (primeiro 28 27)

  --CURRY 2
  printf "\nFunc. 8 CURRY 2: entrada:%d %d; resultado:%d\n" (28::Int) (27::Int) (dobroDaSoma 28 27)

  --UNCURRY 1
  printf "\nFunc. 8 UNCURRY 1: entrada:%d %d; resultado:%d\n" (28::Int) (27::Int) (dividir 28 27)

  --UNCURRY 2
  printf "\nFunc. 8 UNCURRY 2: entrada:%d %d; resultado:%d\n" (1024::Int) (35::Int) (restoDivisao 1024 35)
  