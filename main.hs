module Main where 

import General
import Player
import Jogo
import IAFacil
import IADificil


import System.Random
import Control.Monad (replicateM)





jogo ::Int -> [Int] -> Int -> IO()
jogo 1 _ _= putStrLn "Partida Terminada: Vitoria!"   --estado de vitoria do jogar 
jogo 2 _ _ = putStrLn "Partida Terminada: Derrota!"   --estado de derrota do jogar 
jogo 0 tabuleiro difculdade = do                               --loop principal de execução do jogo 

    ----faze do jogador-------------------
    newboard <- jogada tabuleiro                     --realização da jogada do jogador
    let vitoriaP = tamanhoLista newboard             --obtem quantos dados ainda restão em jogo
    if vitoriaP == 0                                 --caso 0 dados restantes fim do loop 
        then do
            print newboard                           --Impressão dos dados restantes 
            jogo 1 newboard difculdade                         --invoca funçaõ com parametros para finalizar o jogo

    else do 
        print newboard                               
        newboardIA <- if difculdade == 1
                then iaFacilJogada newboard
                else iaDificilJogada newboard       

        let derrota = tamanhoLista newboardIA       --obtem quantos dados ainda restão em jogo
        if derrota == 0                             --caso 0 dados restantes fim do loop 
            then do                         
                print newboardIA                    --impressão dos dados restantes 
                jogo 2 newboardIA difculdade              --invoca função com parametros para finalizar o jogo 
        --------------------------------------
        else do       
            print newboardIA
            jogo 0 newboardIA difculdade                      --caso requisitos para fim do jogo não sejam antingidos invocção recursiva até o fim do jogo



main :: IO ()
main = do
    putStrLn "Digite a quantidade de Dados sorteados:"
    input <- getLine                               --obtem a informação do usuario sobre o numero de dados que deseja usar
    putStrLn "Digite a Dificuldade 1 para facil e 2 para dificil:" 
    input2 <- getLine
    let dificuldade = read input2 :: Int
    let quantidade = read input :: Int             --casting para o timppo inteiro 
    tabuleiro <- rolarDados quantidade             --faz um sorteio de numeros aleatorios para gerar uma lista com valores entre 1 e 6
    print(tabuleiro)                               
    jogo 0 tabuleiro dificuldade                             --chama da função que da inicio ao loop principal do jogo