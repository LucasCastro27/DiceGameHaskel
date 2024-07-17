module Player(
    moverDado,
    jogada,
    verificar
)where
import General

moverDado :: Int -> IO Int                                              --função utilizada para validar o movimento do dado
moverDado face = do                                                     
    putStrLn "Informe para que lado deseja mover o dado:"  
    let invalido = 7 - face                                            --determina qual o lado oposto do dado e o marca como invalido
    input <- getLine                                                   --input do movemento desejado pelo usuario
    let movimento = read input :: Int
    if movimento == invalido then  do                                  --determina se o movimento desejado é o marcado como invalido
        putStrLn "Movimento inválido, rotação de 180"
        moverDado face                                                 --chama em recursão forçando usuario até o usuario fazer um movimento valido
    else if movimento < face then                                      --a face de destino deve ser menor do que a face atual
        return movimento                                               --retorno do novo valor do dado 
    else do
        putStrLn "Movimento inválido, numero maior do que o atual "
        moverDado face                                                 --chamada em recursão

verificar :: [Int] -> Int -> Int                                       --verifica se existe um dado com um valor na lista de dados 
verificar [] _ = -1                                                    --o numero -1 representa a inesistencia do dado
verificar (x:xs) alvo =
    if x == alvo
        then x                                                         --retorno do valor do dano caso o valor desejado seja achado
        else verificar xs alvo                                         --recursão para buscar em toda a fila

jogada::[Int]-> IO [Int]
jogada tabuleiro = do
    putStrLn "Digite o dado que você quer mover:"
    input <- getLine                                                   --pede para o usuario escolher um dado
    let dado = read input :: Int
    let verificado = verificar tabuleiro dado                          --verifica se o dado pode ser encontrado na lista de dados 
    if verificado > 0 then do
        let auxTabuleiro = removerElemento dado tabuleiro              --caso o valor seja encontrado ele e removido da lista
        if verificado > 1 then do                                      --se o valor for difernete de 1 um novo valor vai ser inserido no lugar do que foi removido
            movimento <- moverDado dado                                --obtem o novo valor do dado apartir do jogador 
            let novoTabuleiro = adicionarElemento movimento auxTabuleiro -- adiciona o novo elemento ao final da lista
            return novoTabuleiro                                       --retorna a lista com o valor modificado
        else do 
            putStrLn "Dado removido!"
            return auxTabuleiro                                        --se o usuario manipular o dado com valor 1 ele e removido lista com um valor a menos e retornada
    else do 
        jogada tabuleiro 
        

