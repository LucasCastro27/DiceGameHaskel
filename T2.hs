module Main where 

import General
import Player
import Jogo
import IAFacil

import System.Random
import Control.Monad (replicateM)


jogo2 ::Int -> [Int] -> IO()
jogo2 1 _ = putStrLn "Partida Terminada: Vitoria!"
jogo2 2 _ = putStrLn "Partida Terminada: Derrota!"
jogo2 0 tabuleiro = do 

    ----faze do jogador-------------------
    newboard <- jogada tabuleiro
    let vitoriaP = tamanhoLista newboard
    if vitoriaP == 0 
        then do
            print newboard 
            jogo2 1 newboard

    else do 
        print newboard 
        ----faze da IA Facil----------------
        newboardIA <- iaFacilJogada newboard
        let derrota = tamanhoLista newboardIA
        if derrota == 0 
            then do 
                print newboardIA
                jogo2 2 newboardIA
        --------------------------------------
        else do       
            print newboardIA
            jogo2 0 newboardIA



main :: IO ()
main = do
    putStrLn "Digite a quantidade de Dados sorteados:"
    input <- getLine
    let quantidade = read input :: Int
    tabuleiro <- rolarDados quantidade
    print(tabuleiro)
    jogo2 0 tabuleiro