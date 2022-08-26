{- Camila Przendziuk Franco Felix -}

{- Questão 1 
Escreva uma função para o cálculo dos números da sequência de Fibonacci, utilizando Haskell.-}
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)

{- Questão 2
Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor Comum (MDC) de  Euclides publicado por volta do ano 300 AC. Podemos simplificar este algoritmo dizendo que dados dois inteiros A e B, o MDC entre eles será dado pelo valor absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva uma função para o cálculo do MDC entre dois números inteiros positivos, usando o algoritmo de Euclides conforme apresentado aqui, utilizando Haskell. -}
mdc :: Integral a => a -> a -> a
mdc a b | b == 0 = a
        | a == 0 = b
        | a > b = mdc b (mod a b)
        | a < b = mdc a (mod b a)

{- Questão 3
Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos deste  número.  Exemplo: dado 1234 a função deverá devolver 10. Utilizando Haskell e recursividade. -}
sDigitos :: Int -> Int
sDigitos 0 = 0
sDigitos x = (x `mod` 10) + sDigitos (x `div` 10)

{- Questão 4 
Escreva uma função que devolva a soma de todos os números menores que 10000 que sejam múltiplos de 3 ou 5.-}

{- ^^FEITO!!^^ -}

{- Questão 5 
Escreva uma função que, recebendo uma lista de inteiros, apresente a diferença entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade.-}
quadrado1 :: [Int] -> [Int]
quadrado1 [] = []
quadrado1 (primeiro:resto) = primeiro^2 : (quadrado1 resto)

soma1 :: Int -> [Int] -> Int 
soma1 acumulador [] = acumulador
soma1 acumulador (primeiro:resto) = (+) primeiro (soma1 acumulador resto)

sQuadrados :: [Int] -> Int
sQuadrados x = soma1 0 (quadrado1 x)

qSoma :: [Int] -> Int
qSoma x = (soma1 0 x)^2

diferenca :: [Int] -> Int
diferenca x = (sQuadrados x) - (qSoma x)

{- Questão 6 
O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores que um determinado inteiro dado.-}
minus :: Ord x => [x] -> [x] -> [x]
minus = mb compare

mb :: (n -> m -> Ordering) -> [n] -> [m] -> [n]
mb cmp = loop
  where
    loop [] _ys = []
    loop xs [] = xs
    loop (x:xs) (y:ys)
      = case cmp x y of
        LT -> x : loop xs (y:ys)
        EQ ->     loop xs ys
        GT ->     loop (x:xs) ys

primo :: [Integer]
primo = 2 : euler [3,5..] where 
  euler ~(x:xs) = x : euler (minus xs $ map (* x) (x:xs))

{- Questão 7 
Nem só de Fibonacci vivem os exemplos de recursão. Escreva uma função que devolva todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado.-}
seqLucas :: Int -> [Int]
seqLucas 0 = [2]
seqLucas 1 = [1, 2]
seqLucas x = (head (seqLucas (x-1)) + head (seqLucas (x-2))) : (seqLucas (x - 1))

{- Questão 8
EsLreva uma função, chamada aoContrario em Haskel para reLerter uma lista. Dado [1,2,3] 
devolva [3,2,1] -}
aoContrario :: [n] -> [n]
aoContrario [] = []
aoContrario (x:xs) = (aoContrario xs)++[x]

{- Questão 9 
Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o produto destes valores sem usar o operador de multiplicação.-}
somaRecursiva :: Int -> Int -> Int
somaRecursiva x 1 = x
somaRecursiva x y = x + somaRecursiva x (y - 1)


{- Questão 10 
Escreva uma função chamada comprimento que receba uma lista de  inteiros e devolva o comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule o comprimento de uma lista.-}
comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (n : x) = 1 + comprimento x

main = do 
  putStrLn $ "\nFunc. 1: Entrada 10; Ressultado: " ++ show (fibonacci 10)
  putStrLn $ "\nFunc. 2: Entrada 2 3; Ressultado: " ++ show (mdc 2 3)
  putStrLn $ "\nFunc. 3: Entrada 1234; Ressultado: " ++ show (sDigitos 1234)
  putStrLn $ "\nFunc. 4: Entrada 10000; Ressultado: " ++ show (sum ([x | x <- [1..9999], x `mod` 3  == 0|| x `mod` 5 == 0]))
  putStrLn $ "\nFunc. 5: Entrada [1,2,3]; Ressultado: " ++ show (diferenca [1,2,3])
  putStrLn $ "\nFunc. 6: Entrada 10; Ressultado: " ++ show (take 10 (primo))
  putStrLn $ "\nFunc. 7: Entrada 10; Ressultado: " ++ show (reverse (seqLucas 10))
  putStrLn $ "\nFunc. 8: Entrada [1,2,3]; Ressultado: " ++ show (aoContrario [1,2,3])
  putStrLn $ "\nFunc. 9: Entrada 5 6; Ressultado: " ++ show (somaRecursiva 5 6)
  putStrLn $ "\nFunc. 10: Entrada [1,3,5,7,9]; Ressultado: " ++ show (comprimento [1,3,5,7,9])
