-- |
-- Module      : Tarefa6_2021li1g020
-- Description : Resolve um nível
-- Copyright   : José Rodrigo Ferreira Matos <a100612@alunos.uminho.pt>;
--               António Filipe Castro Silva <a100533@alunos.uminho.pt>;
--
-- Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.

module Tarefa6_2021li1g020 where

import Data.List
import LI12122
import Tarefa4_2021li1g020

-- | Função principal que pega em todas as soluções e escolhe a mais pequena
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo jogadas jogoInicial =
  let solucoes = filter portaEncontrada (verificaSolucoes jogadas jogoInicial)
   in if null solucoes
        then Nothing -- Se não houver solução, só dá Nothing
        else Just (obterMovimentos (head (sortOn length solucoes)))

-- | Função que vai retirando o número de jogadas
verificaSolucoes :: Int -> Jogo -> [[Jogo]]
verificaSolucoes jogadas jogoInicial
  | jogadas <= 0 = [[jogoInicial]]
  | otherwise = [jogoInicial] : map (jogoInicial :) (concatMap (verificaSolucoes (jogadas - 1)) (encontrarJogos jogoInicial))

-- Função que testa todas as jogadas possíveis
encontrarJogos :: Jogo -> [Jogo]
encontrarJogos = encontrarJogos' [AndarEsquerda, AndarDireita, Trepar, InterageCaixa]
  where
    encontrarJogos' :: [Movimento] -> Jogo -> [Jogo]
    encontrarJogos' [] _ = []
    encontrarJogos' (h : t) jogoInicial =
      let novoJogo = moveJogador jogoInicial h
       in if novoJogo /= jogoInicial -- se o jogo estiver diferente significa q aquela jogada é possível
            then novoJogo : encontrarJogos' t jogoInicial
            else encontrarJogos' t jogoInicial

-- | Esta função vê se em alguma parte do Jogo todo o jogador  esteve uma porta, o q significa q o mapa é possível
portaEncontrada :: [Jogo] -> Bool
portaEncontrada caminhosPercorridos = (mapa !! y) !! x == Porta
  where
    Jogo mapa (Jogador (x, y) _ _) = last caminhosPercorridos

-- | transforma a lista de jogos nos movimentos q mudam de jogo para jogo
obterMovimentos :: [Jogo] -> [Movimento]
obterMovimentos [] = []
obterMovimentos [_] = []
obterMovimentos (x : y : t) = parDeJogosParaMovimento x y : obterMovimentos (y : t)
  where
    parDeJogosParaMovimento :: Jogo -> Jogo -> Movimento
    parDeJogosParaMovimento (Jogo _ (Jogador (_, y1) _ caixa1)) (Jogo _ (Jogador (_, y2) dir2 caixa2))
      | caixa1 /= caixa2 = InterageCaixa
      | y1 > y2 = Trepar
      | dir2 == Este = AndarDireita
      | otherwise = AndarEsquerda