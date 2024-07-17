module Player(
    moverDado,
    jogada,
    verificar
)where
import General

moverDado :: Int -> IO Int
moverDado face = do
    putStrLn "Informe para que lado deseja mover o dado:"
    let invalido = 7 - face
    input <- getLine
    let movimento = read input :: Int
    if movimento == invalido then  do 
        putStrLn "Movimento inválido, rotação de 180"
        moverDado face
    else if movimento < face then
        return movimento
    else do
        putStrLn "Movimento inválido, numero maior do que o atual "
        moverDado face

verificar :: [Int] -> Int -> Int
verificar [] _ = -1  
verificar (x:xs) alvo =
    if x == alvo
        then x  
        else verificar xs alvo  

jogada::[Int]-> IO [Int]
jogada tabuleiro = do
    putStrLn "Digite o dado que você quer mover:"
    input <- getLine
    let dado = read input :: Int
    let verificado = verificar tabuleiro dado
    if verificado > 0 then do
        let auxTabuleiro = removerElemento dado tabuleiro
        if verificado > 1 then do
            movimento <- moverDado dado
            let novoTabuleiro = adicionarElemento movimento auxTabuleiro
            return novoTabuleiro
        else do 
            putStrLn "Dado removido!"
            return auxTabuleiro
    else do 
        jogada tabuleiro 
        

