module IAFacil(
    iaFacilEscolheDado,
    iaFacilEscolheValor,
    iaFacilJogada    
)where
import System.Random
import General

iaFacilEscolheDado :: [Int] -> IO Int                               --seleciona aleatoriamente um dos dados da lista
iaFacilEscolheDado tabuleiro = do                   
    let quantidadeDados = tamanhoLista tabuleiro                    --determina a quantida de elementos que tem na lista de dados 
    escolhido <- sortearNumero 0 (quantidadeDados-1)                --sorteia um index da lista 
    case valorNoIndice tabuleiro escolhido of                            
        Just dadoSelecionado -> return dadoSelecionado              --retorna o valor que foi sortiado 
        Nothing -> error "Índice inválido"                          --proteção em caso de lista vazia 

iaFacilEscolheValor :: Int -> IO Int
iaFacilEscolheValor maximo = do                                     --dado um valor de um dado sorteia um movimento aleatorio valido para aquele dado
    numero <- randomRIO (1, maximo - 1)                             --valor sortidado no invervalo permitido
    if numero + 7 /= maximo                                         --regra para inpedir rotação de 180 no dado
        then return numero
        else iaFacilEscolheValor maximo                             --recursão até valor valido ser achado


iaFacilJogada :: [Int] -> IO [Int]
iaFacilJogada tabuleiro = do 
    dadoEscolhidoIA <- iaFacilEscolheDado tabuleiro                --escolhe um dado
    let auxIaTabuleiro = removerElemento dadoEscolhidoIA tabuleiro --cria uma lista auxiliar sem a presença do dado selecionado
    if dadoEscolhidoIA > 1                                         --caso o dado selecionado seja difernete de 1 um valor novo sera atribuido 
        then do   
            valorEscolhido <- iaFacilEscolheValor dadoEscolhidoIA  --sorteio do novo valor do dado
            let iaTabuleiro = adicionarElemento valorEscolhido auxIaTabuleiro -- --adiciona o novo valor ao final 
            putStrLn $ "Movimento da IA: " ++ show dadoEscolhidoIA ++ " -> " ++ show valorEscolhido
            return iaTabuleiro                                     --retorna o tabuleiro com o valor do dado modificado
        else do
            putStrLn $ "Movimento da IA: " ++ show dadoEscolhidoIA ++ " -> remove"
            return auxIaTabuleiro                                  --caso o valor do dado seja um e retornado a lista com um valor a menos