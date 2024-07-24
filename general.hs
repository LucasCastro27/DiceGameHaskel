module General(
    adicionarElemento,
    sortearNumero,
    removerElemento,
    valorNoIndice,
    tamanhoLista,
    maybeIntToInt,
    resto,
    firstTwo,
    headA,
    sortList
)where
import System.Random
import Data.List (delete,group,sort)
-----------------------Funçãoes simples de uso generico utilizado para implementação dos outros modulos --------------------------------
--Função para obter o primeiro elemento de uma lista 
headA :: [Int] -> Int
headA (x:_) = x
headA []    = -1


--Função para ordenar uma lista
sortList :: (Ord a) => [a] -> [a]
sortList = sort


--Função para obeter o modulo de dois valores 
resto :: Int -> Int -> Int
resto x y = x `mod` y

--Adiciona um dado valor no final de uma lista
adicionarElemento :: Int -> [Int] -> [Int]
adicionarElemento elemento lista = lista ++ [elemento]

--Função para obter um numero aleatorio dado um valor minimo e maixomo para o sorteio 
sortearNumero :: Int -> Int -> IO Int
sortearNumero min max = randomRIO (min, max)

--Função para remover um elemento de uma lista 
removerElemento :: Int -> [Int] -> [Int]
removerElemento _ [] = []  
removerElemento x (y:ys)
    | x == y    = ys     
    | otherwise = y : removerElemento x ys  

--Função para obter o valor que esta em um determinado indicie de uma lista
valorNoIndice :: [Int] -> Int -> Maybe Int
valorNoIndice lista indice
    | indice < 0 || indice >= length lista = Nothing   
    | otherwise = Just (lista !! indice)


--Função para obter o tamanho de uma lista 

tamanhoLista :: [a] -> Int
tamanhoLista [] = 0             
tamanhoLista (_:resto) = 1 + tamanhoLista resto

--Função para converter do tipo maybeint para o tipo Int
maybeIntToInt :: Maybe Int -> Int
maybeIntToInt maybeValue = case maybeValue of
    Just value -> value
    Nothing    -> 0 

--função para obter os dois primeiros valores de uma lista 
firstTwo :: [Int] -> (Int, Int)
firstTwo [] = (-1, -1)
firstTwo [x] = (x, -1)
firstTwo (x:y:_) = (x, y)



---------------------------------------------------------------------------------------------------------------------------------------