module General(
    adicionarElemento,
    sortearNumero,
    removerElemento,
    valorNoIndice,
    tamanhoLista,
    maybeIntToInt    
)where
import System.Random

-----------------------Funçãoes simples de uso generico utilizado para implementação dos outros modulos --------------------------------

adicionarElemento :: Int -> [Int] -> [Int]
adicionarElemento elemento lista = lista ++ [elemento]

sortearNumero :: Int -> Int -> IO Int
sortearNumero min max = randomRIO (min, max)

removerElemento :: Int -> [Int] -> [Int]
removerElemento _ [] = []  
removerElemento x (y:ys)
    | x == y    = ys     
    | otherwise = y : removerElemento x ys  


valorNoIndice :: [Int] -> Int -> Maybe Int
valorNoIndice lista indice
    | indice < 0 || indice >= length lista = Nothing   
    | otherwise = Just (lista !! indice)


tamanhoLista :: [a] -> Int
tamanhoLista [] = 0             
tamanhoLista (_:resto) = 1 + tamanhoLista resto

maybeIntToInt :: Maybe Int -> Int
maybeIntToInt maybeValue = case maybeValue of
    Just value -> value
    Nothing    -> 0 



---------------------------------------------------------------------------------------------------------------------------------------