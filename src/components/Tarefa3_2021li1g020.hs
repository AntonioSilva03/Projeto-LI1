{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- |
-- Module      : Tarefa3_2021li1g020
-- Description : Representação textual do jogo
-- Copyright   : José Rodrigo Ferreira Matos <a100612@alunos.uminho.pt>;
--               António Filipe Castro Silva <a100533@alunos.uminho.pt>;
--
-- Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
module Tarefa3_2021li1g020 where

import LI12122

-- | Pega numa lista de Peças, e se os dois inteiros corresponderem às coordenadas então adicionará a personagem, e dependendo da direção será '<', no caso de Oeste, ou '>', no caso de Este
-- Se for um Bloco adiciona um X, uma Caixa um C, uma Porta um P e se for Vazio simplesmente adiciona um espaço
converterLinha :: [Peca] -> Int -> Int -> Coordenadas -> Direcao -> String
converterLinha [] _ _ _ _ = ""
converterLinha (x : xs) xi yi c d
  | (xi, yi) == c && (x == Bloco || x == Caixa || x == Porta) = error "Jogo inválido: o Jogador tem de ser colocado no Vazio"
  | (xi, yi) == c = if d == Oeste then '<' : converterLinha xs (xi + 1) yi c d else '>' : converterLinha xs (xi + 1) yi c d
  | x == Bloco = 'X' : converterLinha xs (xi + 1) yi c d
  | x == Caixa = 'C' : converterLinha xs (xi + 1) yi c d
  | x == Porta = 'P' : converterLinha xs (xi + 1) yi c d
  | x == Vazio = ' ' : converterLinha xs (xi + 1) yi c d

-- | Usando a função converterLinha, recebe um Mapa e vai transformando cada linha do Mapa numa string, usando depois o "\n" para sinalizar que vai mudar de linha
converterColuna :: Mapa -> Int -> Coordenadas -> Direcao -> String
converterColuna [x] yi c d = converterLinha x 0 yi c d
converterColuna (x : xs) yi c d = converterLinha x 0 yi c d ++ "\n" ++ converterColuna xs (yi + 1) c d

-- | A função converter transforma o Jogo numa String utilizando a converterColuna e adicionando o Jogador na sua posição (x,y)
converter :: Jogo -> String
converter (Jogo mapa (Jogador (x, y) direcao _)) = converterColuna mapa 0 (x, y) direcao

-- | Este show serve para a String conseguir aparecer na consola
instance Show Jogo where
  show = converter
