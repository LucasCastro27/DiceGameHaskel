module IAFacil(
    iaFacilEscolheDado,
    iaFacilEscolheValor,
    iaFacilJogada    
)where
import System.Random
import General

iaFacilEscolheDado :: [Int] -> IO Int
iaFacilEscolheDado tabuleiro = do
    let quantidadeDados = tamanhoLista tabuleiro
    escolhido <- sortearNumero 0 (quantidadeDados-1)
    case valorNoIndice tabuleiro escolhido of
        Just dadoSelecionado -> return dadoSelecionado
        Nothing -> error "Índice inválido"

iaFacilEscolheValor :: Int -> IO Int
iaFacilEscolheValor maximo = do
    numero <- randomRIO (1, maximo - 1)
    if numero + 7 /= maximo
        then return numero
        else iaFacilEscolheValor maximo


iaFacilJogada :: [Int] -> IO [Int]
iaFacilJogada tabuleiro = do 
    dadoEscolhidoIA <- iaFacilEscolheDado tabuleiro
    let auxIaTabuleiro = removerElemento dadoEscolhidoIA tabuleiro
    if dadoEscolhidoIA > 1 
        then do   
            valorEscolhido <- iaFacilEscolheValor dadoEscolhidoIA
            let iaTabuleiro = adicionarElemento valorEscolhido auxIaTabuleiro
            putStrLn $ "Movimento da IA: " ++ show dadoEscolhidoIA ++ " -> " ++ show valorEscolhido
            return iaTabuleiro
        else do
            putStrLn $ "Movimento da IA: " ++ show dadoEscolhidoIA ++ " -> remove"
            return auxIaTabuleiro