module IADificil(
    iaDificilJogada    
)where
import Data.List (delete,group,sort)
import System.Random
import IAFacil
import General
-- Função para remover 5 e 2 de uma lista
remove5e2 :: [Int] -> [Int]
remove5e2 = filter (\x -> x /= 2 && x /= 5)

-- Função para remover pares que somam 7
condicaoMaisDados :: [Int] -> [Int]                 
condicaoMaisDados [] = []
condicaoMaisDados (x:xs) = 
    let remaining = condicaoMaisDados xs
    in case acharPar x remaining of
        Just y  -> delete y (delete x remaining) 
        Nothing -> x : remaining

-- Função auxiliar para encontrar um par que soma 7
acharPar :: Int -> [Int] -> Maybe Int
acharPar x [] = Nothing
acharPar x (y:ys)
    | x + y == 7 = Just y
    | otherwise  = acharPar x ys
        

-- Função para remover dados que podem formar pares de uma lista 
condicaoDadosIguais :: (Ord a) => [a] -> [a]
condicaoDadosIguais = concatMap removePairs . group . sort
  where
    removePairs xs
      | odd (length xs) = [Prelude.head xs]
      | otherwise       = []

-- ajusta uma tupla de dados fazendo a modificação adequada para que o resultado perdedor seja obtido 
adjustTuple :: (Ord a, Num a) => (a, a) -> (a, a)
adjustTuple (a, b)
    | a + b <= 7 = (a, b)  -- Se a soma já for menor ou igual a 7, não muda nada
    | a <= b    = (a - (a + b - 7), b)  -- Se a é menor ou igual a b, ajusta a
    | otherwise = (a, b - (a + b - 7))  -- Caso contrário, ajusta b

--indentifica em uma tupla qual o elemento foi modificado
diffElement :: (Eq a) => (a, a) -> (a, a) -> a
diffElement (a, b) (c, d)
    | a /= c && b /= d = error "varias diferenças ou nem uma diferença"
    | a /= c = a --A é difernete 
    | b /= d = b --B é diferente 
    | otherwise = error "sem diferenças"

iaDificilUmDado:: [Int] -> IO [Int]
iaDificilUmDado tabuleiro = do 
    let dadoEscolhido = valorNoIndice tabuleiro 0                    --obtem o valor do dado 
    let dadoEscolhidoIA = maybeIntToInt dadoEscolhido                
    let auxIaTabuleiro = removerElemento dadoEscolhidoIA tabuleiro  --remove o dado do tabuleiro
    case dadoEscolhidoIA of                                         --adiciona o valor de 2 para todos os valores iniciais de dados menos para o 5 2 que são tranformados em 1 e para o 1 que e removido do tabuleiro

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

iaDificilDoisDado:: [Int] -> IO [Int]               ---solução para a melhor jogada com dois dados 
iaDificilDoisDado tabuleiro = do 
    let dado1aux = valorNoIndice tabuleiro 0        ---armazena o valor do primeiro dado na lista de dados 
    let dado2aux = valorNoIndice tabuleiro 1        ---armazena o valor do segundo dado na lista de dados 

    let dado1  = maybeIntToInt dado1aux             
    let dado2  = maybeIntToInt dado2aux

    if (dado1 == dado2) then do
        resultado <- iaFacilJogada tabuleiro        ---se os dois dados forem iguais não a melhor movimento, aplicando movimento aleatorio do bot facil
        return resultado

    else if (dado1 + dado2)==7 then do
        resultado <- iaFacilJogada tabuleiro        ---se a soma dos dois dados forem 7 não a melhor movimento aplicando movimento aleatorio do bot facil
        return resultado

    else if (dado1 + dado2)>7 then do
        let (novoDado1, novoDado2) = adjustTuple (dado1, dado2)             ---faz com que a soma dos dados seja 7 se possivel 
        let modficado = diffElement (dado1, dado2) (novoDado1, novoDado2)   --verifica qual o valor precisou ser modificado para ser possivel obter a soma de 7
        let auxIaTabuleiro = removerElemento modficado tabuleiro            --remove o valor modificado do tabuleiro 
        if modficado == dado1 then do                                       --adiciona o dado 1 no local do que foi removido 
            let iaTabuleiro = adicionarElemento novoDado1 auxIaTabuleiro    
            putStrLn $ "Movimento da IA D: " ++ show dado1 ++ " -> " ++ show novoDado1
            return iaTabuleiro
        else do                                                             --adiciona o dado 2 no local do que foi removido
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
            


iaDificilmaisDados:: [Int] -> IO [Int]
iaDificilmaisDados tabuleiro = do
    let tabuleiroSem5e2 = remove5e2 tabuleiro                       --inicio do processo de filtragem removendo 5 e 2
    let quantidadeDadosValidos = tamanhoLista tabuleiroSem5e2       --obtem o tamanho da lista
    let tabuleiroOrdenado = sortList tabuleiroSem5e2                --ordena os valores dos dados 
    let tabuleiroaux1 = condicaoDadosIguais tabuleiroOrdenado       --remoção de dados iguais que formão pares {1,1,1,2,2} -> {1}
    let tabuleiroaux2 = condicaoMaisDados tabuleiroaux1             --remove dados que podem formar duplas de valor 7

    let quantidadeFinal = tamanhoLista tabuleiroaux2                --valor da quantidade de dados depois da filtragem
    print tabuleiroaux2
    if quantidadeFinal == 0 then do                                 -- se n sobrou nem um dado n há jogada ideial pra ser feita
        resultado <- iaFacilJogada tabuleiro
        return resultado

    else if(quantidadeFinal == 1 )then do                           --caso apenas um dado restante utiliza a funçao de escolha ideial para um dado
        resultado <- iaDificilUmDado tabuleiroaux2
        let dadoEscolhido = headA tabuleiroaux2
        let dadoNovo = headA resultado
        if dadoNovo == -1 then do                                  --verifica se foi uma operação de mudança ou remoção de dado
            let iaTabuleiro = removerElemento 1 tabuleiro          --apenas remove o dado caso seja uma operação de remoção 
            return iaTabuleiro

        else do     
            let auxIaTabuleiro = removerElemento dadoEscolhido tabuleiro   --remove e adiciona o valor do novo dado caso seja operação de mudança
            let iaTabuleiro = adicionarElemento dadoNovo auxIaTabuleiro
            return iaTabuleiro

       
    else if(quantidadeFinal == 2) then do                       --- funciona de forma analoga ao condicional a cima 
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


--função inicial e principal onde e determinada a quantida de dados e qual tipo de solulçao deve ser aplicado 
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


    