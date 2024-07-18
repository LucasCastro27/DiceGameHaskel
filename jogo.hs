module Jogo(
    rolarDados
)where
import System.Random


rolarDados :: Int -> IO [Int]
rolarDados quantidade = do
    gen <- newStdGen                                    --res
    return $ take quantidade $ randomRs (1, 6) gen      --faz a rolagem do dado, ou seja sorteia um numero aleatorio entre 1,6