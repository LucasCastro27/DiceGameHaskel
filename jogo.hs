module Jogo(
    rolarDados
)where
import System.Random


rolarDados :: Int -> IO [Int]
rolarDados quantidade = do
    gen <- newStdGen  
    return $ take quantidade $ randomRs (1, 6) gen