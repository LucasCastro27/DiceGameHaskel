module IADificil(
    iaDificilJogada    
)where
import Data.List (delete)

import Data.List (group)
import Data.List (sort)
import System.Random
import IAFacil
import General

headA :: [Int] -> Int
headA (x:_) = x
headA []    = -1

sortList :: (Ord a) => [a] -> [a]
sortList = sort

ordenarDados :: Int -> Int -> (Int, Int)
ordenarDados x y
    | x <= y    = (x, y)
    | otherwise = (y, x)

adjustTuple :: (Ord a, Num a) => (a, a) -> (a, a)
adjustTuple (a, b)
    | a + b <= 7 = (a, b)  -- Se a soma já for menor ou igual a 7, não muda nada
    | a <= b    = (a - (a + b - 7), b)  -- Se a é menor ou igual a b, ajusta a
    | otherwise = (a, b - (a + b - 7))  -- Caso contrário, ajusta b


diffElement :: (Eq a) => (a, a) -> (a, a) -> a
diffElement (a, b) (c, d)
    | a /= c && b /= d = error "More than one difference or no differences found"
    | a /= c = a
    | b /= d = b
    | otherwise = error "No differences found"

iaDificilUmDado:: [Int] -> IO [Int]
iaDificilUmDado tabuleiro = do 
    let dadoEscolhido = valorNoIndice tabuleiro 0
    let dadoEscolhidoIA = maybeIntToInt dadoEscolhido
    let auxIaTabuleiro = removerElemento dadoEscolhidoIA tabuleiro
    case dadoEscolhidoIA of

        1-> do
            putStrLn $ "Movimento da IA: " ++ show dadoEscolhidoIA ++ " -> removido"
            return auxIaTabuleiro
        2-> do 
            let iaTabuleiro = adicionarElemento 1 auxIaTabuleiro -- --adiciona o novo valor ao final 
            putStrLn $ "Movimento da IA: " ++ show dadoEscolhidoIA ++ " -> 1"
            return iaTabuleiro
        3-> do
            let iaTabuleiro = adicionarElemento 2 auxIaTabuleiro -- --adiciona o novo valor ao final 
            putStrLn $ "Movimento da IA D: " ++ show dadoEscolhidoIA ++ " -> 2" 
            return iaTabuleiro
        4-> do 
            let iaTabuleiro = adicionarElemento 2 auxIaTabuleiro -- --adiciona o novo valor ao final
            putStrLn $ "Movimento da IA D: " ++ show dadoEscolhidoIA ++ " -> 2" 
            return iaTabuleiro
        5-> do
            let iaTabuleiro = adicionarElemento 1 auxIaTabuleiro -- --adiciona o novo valor ao final 
            putStrLn $ "Movimento da IA D: " ++ show dadoEscolhidoIA ++ " -> 1" 
            return iaTabuleiro
        6-> do
            let iaTabuleiro = adicionarElemento 2 auxIaTabuleiro -- --adiciona o novo valor ao final 
            putStrLn $ "Movimento da IA D: " ++ show dadoEscolhidoIA ++ " -> 2" 
            return iaTabuleiro

iaDificilDoisDado:: [Int] -> IO [Int]
iaDificilDoisDado tabuleiro = do 
    let dado1aux = valorNoIndice tabuleiro 0
    let dado2aux = valorNoIndice tabuleiro 1

    let dado1  = maybeIntToInt dado1aux
    let dado2  = maybeIntToInt dado2aux

    

    if (dado1 == dado2) then do
        resultado <- iaFacilJogada tabuleiro
        return resultado

    else if (dado1 + dado2)==7 then do
        resultado <- iaFacilJogada tabuleiro
        return resultado

    else if (dado1 + dado2)>7 then do
        let (novoDado1, novoDado2) = adjustTuple (dado1, dado2)
        let modficado = diffElement (dado1, dado2) (novoDado1, novoDado2) 
        let auxIaTabuleiro = removerElemento modficado tabuleiro    
        if modficado == dado1 then do
            let iaTabuleiro = adicionarElemento novoDado1 auxIaTabuleiro
            putStrLn $ "Movimento da IA D: " ++ show dado1 ++ " -> " ++ show novoDado1
            return iaTabuleiro
        else do 
            let iaTabuleiro = adicionarElemento novoDado2 auxIaTabuleiro
            putStrLn $ "Movimento da IA D: " ++ show dado2 ++ " -> " ++ show novoDado2
            return iaTabuleiro

    else do 
        if (dado1 > dado2) then do
            let auxIaTabuleiro = removerElemento dado1 tabuleiro 
            let iaTabuleiro = adicionarElemento dado2 auxIaTabuleiro
            putStrLn $ "Movimento da IA D: " ++ show dado1 ++ " -> " ++ show dado2
            return iaTabuleiro
        else  do
            let auxIaTabuleiro = removerElemento dado2 tabuleiro 
            let iaTabuleiro = adicionarElemento dado1 auxIaTabuleiro
            putStrLn $ "Movimento da IA D: " ++ show dado2 ++ " -> " ++ show dado1
            return iaTabuleiro
            

remove5e2 :: [Int] -> [Int]
remove5e2 = filter (\x -> x /= 2 && x /= 5)

-- Função para remover pares que somam 7
condicaoMaisDados :: [Int] -> [Int]
condicaoMaisDados [] = []
condicaoMaisDados (x:xs) = 
    let remaining = condicaoMaisDados xs
    in case acharPar x remaining of
        Just y  -> delete y (delete x remaining) -- Remove ambos os elementos do par
        Nothing -> x : remaining

-- Função auxiliar para encontrar um par que soma 7
acharPar :: Int -> [Int] -> Maybe Int
acharPar x [] = Nothing
acharPar x (y:ys)
    | x + y == 7 = Just y
    | otherwise  = acharPar x ys
        
condicaoDadosIguais :: (Ord a) => [a] -> [a]
condicaoDadosIguais = concatMap removePairs . group . sort
  where
    removePairs xs
      | odd (length xs) = [Prelude.head xs]
      | otherwise       = []

firstTwo :: [Int] -> (Int, Int)
firstTwo [] = (-1, -1)
firstTwo [x] = (x, -1)
firstTwo (x:y:_) = (x, y)

iaDificilmaisDados:: [Int] -> IO [Int]
iaDificilmaisDados tabuleiro = do
    let tabuleiroSem5e2 = remove5e2 tabuleiro
    let quantidadeDadosValidos = tamanhoLista tabuleiroSem5e2
    let tabuleiroOrdenado = sortList tabuleiroSem5e2
    let tabuleiroaux1 = condicaoDadosIguais tabuleiroOrdenado
    let tabuleiroaux2 = condicaoMaisDados tabuleiroaux1

    let quantidadeFinal = tamanhoLista tabuleiroaux2
    print tabuleiroaux2
    if quantidadeFinal == 0 then do 
        resultado <- iaFacilJogada tabuleiro
        return resultado

    else if(quantidadeFinal == 1 )then do 
        resultado <- iaDificilUmDado tabuleiroaux2
        let dadoEscolhido = headA tabuleiroaux2
        let dadoNovo = headA resultado
        if dadoNovo == -1 then do 
            let iaTabuleiro = removerElemento 1 tabuleiro 
            return iaTabuleiro

        else do     
            let auxIaTabuleiro = removerElemento dadoEscolhido tabuleiro 
            let iaTabuleiro = adicionarElemento dadoNovo auxIaTabuleiro
            return iaTabuleiro

       
    else if(quantidadeFinal == 2) then do 
        resultado <- iaDificilDoisDado tabuleiroaux2
        let (removido1, removido2) = firstTwo tabuleiroaux2
        let (add1, add2) = firstTwo resultado
        let iaTabuleiro1 = removerElemento removido1 tabuleiro
        let iaTabuleiro2 = adicionarElemento add1 iaTabuleiro1
        
        if removido2 /= -1 then do 
            let iaTabuleiro3 = removerElemento removido2 iaTabuleiro2
            let iaTabuleiro4 = adicionarElemento add2 iaTabuleiro3
            print iaTabuleiro4
            return iaTabuleiro4 
        else do
            print iaTabuleiro2
            return iaTabuleiro2

    else do
        resultado <- iaFacilJogada tabuleiro
        return resultado 

    


iaDificilJogada::[Int] -> IO [Int]
iaDificilJogada tabuleiro = do
    let quantidadeDados = tamanhoLista tabuleiro

    case quantidadeDados of
        1 -> do
            resultado <- iaDificilUmDado tabuleiro
            return resultado
        2 -> do
            resultado <- iaDificilDoisDado tabuleiro
            return resultado
        3 -> do
            
            resultado <- iaDificilmaisDados tabuleiro
            return resultado
        _ | quantidadeDados > 3 -> do
            resultado <- iaDificilmaisDados tabuleiro
            return resultado


    