module General(
    adicionarElemento,
    sortearNumero,
    removerElemento,
    valorNoIndice,
    tamanhoLista    
)where
import System.Random

adicionarElemento :: Int -> [Int] -> [Int]
adicionarElemento elemento lista = lista ++ [elemento]

sortearNumero :: Int -> Int -> IO Int
sortearNumero min max = randomRIO (min, max)

removerElemento :: Int -> [Int] -> [Int]
removerElemento _ [] = []  -- Caso base: lista vazia
removerElemento x (y:ys)
    | x == y    = ys       -- Se encontrar o elemento, retorna a lista sem ele
    | otherwise = y : removerElemento x ys  -- Caso contrário, continua procurando


valorNoIndice :: [Int] -> Int -> Maybe Int
valorNoIndice lista indice
    | indice < 0 || indice >= length lista = Nothing   -- índice fora dos limites, retorna Nothing
    | otherwise = Just (lista !! indice)


tamanhoLista :: [a] -> Int
tamanhoLista [] = 0             
tamanhoLista (_:resto) = 1 + tamanhoLista resto
