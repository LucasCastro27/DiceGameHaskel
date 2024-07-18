module IADificil(
    iaDificilJogada    
)where
import System.Random
import IAFacil
import General

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
            resultado <- iaFacilJogada tabuleiro
            return resultado
        _ | quantidadeDados > 3 -> do
            resultado <- iaFacilJogada tabuleiro
            return resultado


    